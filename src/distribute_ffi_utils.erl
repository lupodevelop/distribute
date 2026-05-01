-module(distribute_ffi_utils).
-export([monotonic_ms/0,
         create_subject/2, encode_subject/1, decode_subject_safe/1,
         to_node_atom_safe/1, to_cookie_atom_safe/1,
         exit_shutdown/1, subject_tag_matches_name/2,
         atom_budget_reset/0, is_valid_registry_name/1,
         clamp_timeout/1, binary_copy/1]).

%% ----------------------------------------------------------------------------
%% Atom-creation budget
%%
%% Distribution requires that node names and cookies be Erlang atoms, and
%% atoms are never garbage-collected (default VM cap: 1 048 576). A caller
%% that loops `connect/ping` over millions of *valid* names exhausts the
%% table and crashes the entire VM with `system_limit (atom_table_full)`.
%%
%% Format validation alone cannot stop this -- only counting can. We track
%% how many *fresh* atoms (i.e. not already interned) have been created
%% through our helpers, and refuse further creation past a configurable
%% budget. The counter lives in an `atomics` ref shared across schedulers
%% so the check is lock-free and concurrency-safe.
%%
%% Existing atoms (already in the table) are always allowed -- they cost
%% nothing extra. Only the first creation of a name burns one budget unit.
%% ----------------------------------------------------------------------------

-define(BUDGET_VALUE_KEY, distribute_atom_budget).
-define(BUDGET_COUNTER_TABLE, distribute_atom_counter_table).
-define(DEFAULT_BUDGET, 10_000).

%% Lazy-init the atomics counter, race-safe and **without**
%% `persistent_term:put` -- the previous design rewrote the term once
%% per concurrent first-caller, and every overwrite of a `persistent_term`
%% slot triggers a global GC pass across every process in the VM.
%% Under a `cluster.health` fan-out (50 parallel pings, all hitting
%% `ensure_counter` at once on a cold VM) that produced 50 back-to-back
%% global GCs and froze the runtime for seconds.
%%
%% ETS is the right tool here:
%% - `ets:new(named_table, public, ...)` is atomic; concurrent racers
%%   either win or get `badarg` (which we catch).
%% - `ets:insert_new/2` is atomic check-and-insert; the first caller
%%   plants the atomics ref, every other caller reads the canonical
%%   ref via `ets:lookup/2`. No global GC, no rewrite.
%% - Reads after init cost a single hash lookup (with `read_concurrency`
%%   the BEAM serves them lock-free across schedulers).
ensure_counter() ->
    Table = ?BUDGET_COUNTER_TABLE,
    case ets:whereis(Table) of
        undefined ->
            try
                ets:new(Table, [
                    named_table, public, set, {read_concurrency, true}
                ])
            catch
                error:badarg -> Table
            end;
        _ ->
            Table
    end,
    case ets:lookup(Table, ref) of
        [{ref, Ref}] ->
            Ref;
        [] ->
            MyRef = atomics:new(1, [{signed, false}]),
            case ets:insert_new(Table, {ref, MyRef}) of
                true -> MyRef;
                false ->
                    [{ref, Ref}] = ets:lookup(Table, ref),
                    Ref
            end
    end.

current_budget() ->
    persistent_term:get(?BUDGET_VALUE_KEY, ?DEFAULT_BUDGET).

%% Atomically reserve one budget unit. Returns `{ok, Ref}` if the reservation
%% succeeded (caller may create the atom); `{error, atom_budget_exceeded}` if
%% the budget is full (and decrements its speculative add to keep the counter
%% accurate).
try_reserve_atom() ->
    Ref = ensure_counter(),
    New = atomics:add_get(Ref, 1, 1),
    case New =< current_budget() of
        true -> {ok, Ref};
        false ->
            atomics:sub(Ref, 1, 1),
            {error, atom_budget_exceeded}
    end.

%% Public: reset the counter. `@internal` on the Gleam side -- only
%% intended for tests that want to reuse the budget across cases.
atom_budget_reset() ->
    Ref = ensure_counter(),
    atomics:put(Ref, 1, 0),
    nil.

%% Common pattern: atom is needed for node/cookie use. Existing atom
%% costs nothing; fresh atom takes one budget unit. Returns
%% `{ok, Atom}`, `{error, invalid_format}` or `{error, atom_budget_exceeded}`.
%%
%% The `catch error:badarg` clause is the documented signal from
%% `binary_to_existing_atom/2` that the atom is not yet interned --
%% the *expected* path that triggers the budgeted creation below.
%% Catching all classes (`catch _:_`) would silently swallow real bugs in
%% this code path (for example a future refactor that hands in a non-binary).
%% We only catch `error:badarg` there.
%%
%% On the creation path, we catch `error:system_limit` from
%% `binary_to_atom/2` so atom-table saturation degrades to a typed
%% refusal instead of crashing the caller.
budget_aware_atom_create(Bin) ->
    try
        {ok, binary_to_existing_atom(Bin, utf8)}
    catch error:badarg ->
        case try_reserve_atom() of
            {error, atom_budget_exceeded} ->
                {error, atom_budget_exceeded};
            {ok, Ref} ->
                try
                    {ok, binary_to_atom(Bin, utf8)}
                catch error:system_limit ->
                    atomics:sub(Ref, 1, 1),
                    {error, atom_budget_exceeded}
                end
        end
    end.

%% @doc Get monotonic time in milliseconds.
%%
%% Strictly non-decreasing; immune to NTP slew, leap seconds, and
%% manual clock adjustments. The right tool for "is the deadline
%% reached?" / "how much budget is left?" -- a wall-clock skew of
%% even a few hundred ms could otherwise make a deadline check go
%% backwards and never expire.
-spec monotonic_ms() -> integer().
monotonic_ms() ->
    erlang:monotonic_time(millisecond).

%% @doc Construct a Subject tuple from a Pid and a tag.
%%
%% This is distribute's own boundary over gleam_erlang's Subject layout.
%% Subject layout (gleam_erlang >= 1.x): {subject, Pid, Tag}
-spec create_subject(pid(), term()) -> {subject, pid(), term()}.
create_subject(Pid, Tag) ->
    {subject, Pid, Tag}.

%% @doc Serialize a Subject using Erlang's term_to_binary.
%%
%% The Pid inside the Subject carries node info, so the encoded
%% form is routable across nodes after decoding.
-spec encode_subject(tuple()) -> binary().
encode_subject(Subject) ->
    erlang:term_to_binary(Subject).

%% @doc Check whether a Subject's tag equals a given binary name.
%%
%% `register_global` requires this invariant: a Subject registered under
%% name N must carry N as its tag, so that a remote `lookup` (which
%% reconstructs the Subject via `unsafe_from_name(N, Pid)`) produces a Subject
%% the actor selector actually receives on. A mismatch causes silent
%% mailbox accumulation on the receiver and is detected up front here.
%%
%% Returns true for `{subject, _Pid, Name}` where Name matches the
%% supplied binary, false otherwise.
-spec subject_tag_matches_name(tuple(), binary()) -> boolean().
subject_tag_matches_name({subject, _Pid, Tag}, Name) when is_binary(Name) ->
    Tag =:= Name;
subject_tag_matches_name(_, _) ->
    false.

%% @doc Send `shutdown` exit signal to a process.
%%
%% Catchable by actors that trap exits -- they receive
%% `{'EXIT', From, shutdown}` and can run cleanup before dying. Equivalent
%% to what an OTP supervisor sends as the first phase of `terminate_child`.
%% A process that does NOT trap exits dies immediately.
-spec exit_shutdown(pid()) -> true.
exit_shutdown(Pid) ->
    erlang:exit(Pid, shutdown).

%% @doc Convert a binary to a node-name atom safely.
%%
%% Distinct from to_atom_safe/1: a node atom must be created if it does
%% not yet exist (Erlang distribution requires node names as atoms), but
%% we still gate creation behind strict format validation to prevent
%% atom-table exhaustion through hostile input.
%%
%% Format: `<name>@<host>` where:
%%   - 1..255 bytes total
%%   - name part: [a-zA-Z0-9_-]+
%%   - host part: [a-zA-Z0-9._-]+
%%
%% Tries binary_to_existing_atom first to avoid creating duplicates of
%% atoms that already exist; falls back to binary_to_atom only after
%% format validation has passed.
-spec to_node_atom_safe(Input :: binary() | list()) ->
    {ok, atom()} | {error, invalid_node_name} | {error, atom_budget_exceeded}.
to_node_atom_safe(Bin) when is_list(Bin) ->
    to_node_atom_safe(list_to_binary(Bin));
to_node_atom_safe(Bin) when is_binary(Bin) ->
    case is_valid_node_name(Bin) of
        true -> budget_aware_atom_create(Bin);
        false -> {error, invalid_node_name}
    end;
to_node_atom_safe(_) ->
    {error, invalid_node_name}.

%% @doc Convert a binary to a cookie atom safely.
%%
%% Cookies must be valid Erlang atoms in the printable range. Allowed:
%% [a-zA-Z0-9_-], 1..255 bytes. No `@`, no whitespace, no control chars.
-spec to_cookie_atom_safe(Input :: binary() | list()) ->
    {ok, atom()} | {error, invalid_cookie} | {error, atom_budget_exceeded}.
to_cookie_atom_safe(Bin) when is_list(Bin) ->
    to_cookie_atom_safe(list_to_binary(Bin));
to_cookie_atom_safe(Bin) when is_binary(Bin) ->
    case is_valid_cookie(Bin) of
        true -> budget_aware_atom_create(Bin);
        false -> {error, invalid_cookie}
    end;
to_cookie_atom_safe(_) ->
    {error, invalid_cookie}.

%% Validate node name format: <name>@<host>, ASCII subset, 1..255 bytes.
is_valid_node_name(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    Size >= 3 andalso Size =< 255 andalso
    case binary:split(Bin, <<"@">>, [global]) of
        [Name, Host] when byte_size(Name) > 0, byte_size(Host) > 0 ->
            all_chars_match(Name, fun is_node_name_char/1) andalso
            all_chars_match(Host, fun is_node_host_char/1);
        _ ->
            false
    end;
is_valid_node_name(_) -> false.

%% Validate cookie format: ASCII subset, 1..255 bytes, no `@`.
is_valid_cookie(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    Size >= 1 andalso Size =< 255 andalso
    all_chars_match(Bin, fun is_cookie_char/1);
is_valid_cookie(_) -> false.

%% Validate registry name format: ASCII subset `[a-zA-Z0-9._-]+`,
%% 1..255 bytes. Mirrors the cookie/node charset so registry input
%% can never widen the attack surface beyond what cluster validation
%% already accepts.
-spec is_valid_registry_name(binary() | list()) -> boolean().
is_valid_registry_name(Bin) when is_list(Bin) ->
    is_valid_registry_name(list_to_binary(Bin));
is_valid_registry_name(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    Size >= 1 andalso Size =< 255 andalso
    all_chars_match(Bin, fun is_registry_name_char/1);
is_valid_registry_name(_) -> false.

is_registry_name_char(C) ->
    is_node_name_char(C) orelse C =:= $..

%% Clamp a user-supplied timeout to a non-negative integer before it
%% reaches an Erlang `receive after Timeout -> ...' expression.
%% Erlang raises `timeout_value' for negative timeouts; both `global'
%% and `receiver' bind here so they cannot drift on the clamp policy.
-spec clamp_timeout(integer()) -> non_neg_integer().
clamp_timeout(Ms) when is_integer(Ms), Ms < 0 -> 0;
clamp_timeout(Ms) when is_integer(Ms) -> Ms.

all_chars_match(<<>>, _Pred) -> true;
all_chars_match(<<C, Rest/binary>>, Pred) ->
    Pred(C) andalso all_chars_match(Rest, Pred).

is_node_name_char(C) ->
    (C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    C =:= $_ orelse C =:= $-.

is_node_host_char(C) ->
    is_node_name_char(C) orelse C =:= $..

is_cookie_char(C) ->
    is_node_name_char(C).

%% @doc Deserialize a Subject from binary, with validation.
%%
%% Uses binary_to_term with [safe] to prevent atom table attacks.
%% Validates that the result is a {subject, Pid, Tag} tuple.
%% Only the standard Subject layout is accepted.
%%
%% The narrowed `catch error:badarg` clause matches what
%% `erlang:binary_to_term/2` is documented to raise on malformed or
%% unsafe input: any other class (e.g. `error:system_limit` from a
%% saturated atom table, an `exit:` from an inadvertent process-
%% local kill propagated by a NIF) is a real fault and must
%% propagate upward rather than disguise as a plain
%% `Error(InvalidBinary)`. Catching `_:_` would hide those crashes
%% and break Gleam's "no silent runtime exceptions" promise at the
%% FFI boundary.
-spec decode_subject_safe(binary()) -> {ok, tuple()} | {error, nil}.
decode_subject_safe(Bin) ->
    try
        Term = erlang:binary_to_term(Bin, [safe]),
        case Term of
            {subject, Pid, _Tag} when is_pid(Pid) -> {ok, Term};
            _ -> {error, nil}
        end
    catch
        error:badarg -> {error, nil}
    end.

%% @doc Force a copy of a binary to detach it from a larger parent.
%%
%% Used by the codec when extracting small strings or bitarrays from
%% large network payloads. Without this copy, `bit_array:slice` creates
%% a sub-binary that keeps the entire parent payload pinned in memory,
%% leading to massive silent memory leaks if the extracted string is
%% kept alive in an actor's state.
-spec binary_copy(binary()) -> binary().
binary_copy(Bin) ->
    binary:copy(Bin).
