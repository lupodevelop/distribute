-module(distribute_ffi_utils).
-export([to_atom_safe/1, system_time_ms/0, is_ok_atom/1, get_error_reason/1,
         create_subject/2, encode_subject/1, decode_subject_safe/1]).

%% @doc Safely convert a binary, list, or atom to an existing atom.
%%
%% Always uses binary_to_existing_atom to prevent atom table exhaustion.
%% Returns {ok, Atom} on success, {error, Reason} on failure.
-spec to_atom_safe(Input :: binary() | list() | atom()) ->
    {ok, atom()} | {error, binary()}.
to_atom_safe(Bin) when is_list(Bin) ->
    to_atom_safe(list_to_binary(Bin));
to_atom_safe(Bin) when is_binary(Bin) ->
    try
        {ok, binary_to_existing_atom(Bin, utf8)}
    catch
        _:_ -> {error, <<"atom_not_found">>}
    end;
to_atom_safe(Atom) when is_atom(Atom) ->
    {ok, Atom};
to_atom_safe(_) ->
    {error, <<"badarg">>}.

%% @doc Get current system time in milliseconds.
-spec system_time_ms() -> integer().
system_time_ms() ->
    erlang:system_time(millisecond).

%% Shared helpers for Gleam FFI result classification.
is_ok_atom(ok) -> true;
is_ok_atom(_) -> false.

get_error_reason({error, Reason}) when is_binary(Reason) -> Reason;
get_error_reason({error, Reason}) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
get_error_reason({error, Reason}) ->
    iolist_to_binary(io_lib:format("~p", [Reason]));
get_error_reason(_) -> <<"unknown_error">>.

%% @doc Construct a Subject tuple from a Pid and a tag.
%%
%% This is distribute's own fallback for gleam_erlang's @internal
%% `unsafely_create_subject`.
%% A single place to update if the Subject representation ever changes
%% or if a public API is added upstream.
%%
%% Subject layout (gleam_erlang v1.x): {subject, Pid, Tag}
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

%% @doc Deserialize a Subject from binary, with validation.
%%
%% Uses binary_to_term with [safe] to prevent atom table attacks.
%% Validates that the result is a Subject tuple.
-spec decode_subject_safe(binary()) -> {ok, tuple()} | {error, nil}.
decode_subject_safe(Bin) ->
    try
        Term = erlang:binary_to_term(Bin, [safe]),
        case Term of
            {subject, Pid, _Tag} when is_pid(Pid) -> {ok, Term};
            {named_subject, Name} when is_atom(Name) -> {ok, Term};
            _ -> {error, nil}
        end
    catch
        _:_ -> {error, nil}
    end.
