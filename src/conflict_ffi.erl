%% Conflict-resolver FFI for `:global` registrations.
%%
%% `:global.register_name/3` accepts a third argument: a 3-arity
%% function the global_name_server invokes when two nodes register
%% the same name (typically after a partition heal). The resolver
%% must return the winning Pid, or the atom `none` to kill both.
%%
%% **Critical operational constraint**: the resolver runs *inside*
%% the global_name_server process. If the user's Gleam resolver
%% loops, blocks, or panics, every other `:global` operation
%% cluster-wide stalls until the function returns. We cannot trust
%% user code to be fast and total. The wrapper here interposes
%% three protections:
%%
%% 1. The user fn runs in a short-lived spawned worker, not in
%%    global_name_server.
%% 2. We wait for the worker's reply with a hard deadline (default
%%    5000 ms, see DEFAULT_RESOLVER_TIMEOUT_MS).
%% 3. If the worker crashes or the deadline expires, we apply a
%%    deterministic fallback (lowest term-ordered Pid wins) so
%%    `:global` always gets a Pid back and the cluster does not
%%    wedge.
%%
%% Telemetry: every invocation produces exactly one
%% `conflict_resolved` event, plus a preceding
%% `conflict_resolver_failed` event when the fallback fires. The
%% pair is what operators watch to detect "our resolver is broken"
%% versus "our cluster is flapping".
-module(conflict_ffi).
-export([register_with_resolver/3, default_resolver_timeout_ms/0]).

%% Read-through to the configured deadline. Default is 1 000 ms
%% (set in `config:default()` and mirrored in
%% `config_ffi:get_conflict_resolver_timeout/0` for unconfigured
%% nodes). 5 s, the v4-alpha number, was too long for cluster
%% throughput: the resolver runs inside the global_name_server
%% gen_server, so every `:global` operation cluster-wide queues
%% behind it for the duration. 1 s is comfortable for pure
%% resolvers (microseconds in practice) and for RPC-based ones on
%% a healthy cluster, while bounding the worst-case stall.
default_resolver_timeout_ms() ->
    config_ffi:get_conflict_resolver_timeout().

%% Register `Name -> Pid` in `:global` with a custom Gleam-side
%% resolver. Returns the same tagged result shape as the regular
%% `registry_ffi:register/2`. Reads the deadline at registration
%% time; runtime resolver invocations capture the value via the
%% closure so a later `configure/1` cannot retroactively shorten
%% the deadline of a running resolver.
register_with_resolver(Name, Pid, Resolver) when is_function(Resolver, 3) ->
    case erlang:is_pid(Pid) of
        false -> {error, invalid_process};
        true ->
            TimeoutMs = config_ffi:get_conflict_resolver_timeout(),
            Wrapped = fun(N, P1, P2) ->
                safe_invoke(Resolver, N, P1, P2, TimeoutMs)
            end,
            case global:register_name(Name, Pid, Wrapped) of
                yes -> {ok, nil};
                no  -> {error, already_exists}
            end
    end;
register_with_resolver(_Name, _Pid, _NotAFun) ->
    {error, invalid_process}.

%% Run user resolver in an isolated worker. Returns the Pid the
%% caller (`:global`) wants for the survivor, or `none` to kill
%% both. Never raises, never blocks past TimeoutMs.
safe_invoke(Resolver, Name, Pid1, Pid2, TimeoutMs) ->
    Parent = self(),
    Ref = make_ref(),
    {Worker, WorkerMon} = erlang:spawn_monitor(fun() ->
        Result = try
            translate_outcome(Resolver(Name, Pid1, Pid2))
        catch C:R:_S ->
            {crashed, format_crash(C, R)}
        end,
        Parent ! {Ref, Result}
    end),
    Outcome = receive
        {Ref, {keep, WinnerPid}} when is_pid(WinnerPid) ->
            cleanup_worker_monitor(WorkerMon),
            emit_resolved(Name, {some, WinnerPid}),
            WinnerPid;
        {Ref, kill_both} ->
            cleanup_worker_monitor(WorkerMon),
            emit_resolved(Name, none),
            none;
        {Ref, {crashed, ReasonBin}} ->
            cleanup_worker_monitor(WorkerMon),
            emit_resolver_failed(Name, ReasonBin),
            run_fallback(Name, Pid1, Pid2);
        {Ref, _Other} ->
            cleanup_worker_monitor(WorkerMon),
            %% Resolver returned a value that is neither
            %% `Keep(pid)` nor `KillBoth`. Treat as a crash for
            %% telemetry purposes; cannot trust the value.
            emit_resolver_failed(Name,
                <<"resolver returned non-ConflictOutcome value">>),
            run_fallback(Name, Pid1, Pid2);
        {'DOWN', WorkerMon, process, Worker, Reason} ->
            emit_resolver_failed(Name, format_worker_down(Reason)),
            drain_resolver_reply(Ref),
            run_fallback(Name, Pid1, Pid2)
    after TimeoutMs ->
        erlang:exit(Worker, kill),
        await_worker_down(WorkerMon, Worker),
        drain_resolver_reply(Ref),
        emit_resolver_failed(Name, <<"resolver timed out">>),
        run_fallback(Name, Pid1, Pid2)
    end,
    Outcome.

%% Gleam `Keep(pid)` -> `{keep, pid}`; `KillBoth` -> `kill_both`.
%% Anything else falls through to the {keep, _Other} catch in the
%% receive above and is treated as a malformed resolver result.
translate_outcome({keep, Pid}) when is_pid(Pid) -> {keep, Pid};
translate_outcome(kill_both) -> kill_both;
translate_outcome(Other) -> {malformed, Other}.

%% Deterministic fallback: lowest term-ordered Pid wins. Erlang
%% Pids have a total order; this gives reproducible behaviour
%% regardless of which side called `register_name` first.
run_fallback(Name, Pid1, Pid2) ->
    Winner = case Pid1 < Pid2 of
        true  -> Pid1;
        false -> Pid2
    end,
    emit_resolved(Name, {some, Winner}),
    Winner.

emit_resolved(Name, WinnerOption) ->
    telemetry_ffi:emit({conflict_resolved, Name, WinnerOption}).

emit_resolver_failed(Name, ReasonBin) ->
    telemetry_ffi:emit({conflict_resolver_failed, Name, ReasonBin}).

format_crash(C, R) ->
    iolist_to_binary(io_lib:format("~p:~p", [C, R])).

format_worker_down(Reason) ->
    iolist_to_binary(io_lib:format("resolver worker down: ~p", [Reason])).

cleanup_worker_monitor(WorkerMon) ->
    erlang:demonitor(WorkerMon, [flush]),
    ok.

await_worker_down(WorkerMon, Worker) ->
    receive
        {'DOWN', WorkerMon, process, Worker, _Reason} -> ok
    end.

drain_resolver_reply(Ref) ->
    receive
        {Ref, _} -> drain_resolver_reply(Ref)
    after 0 ->
        ok
    end.
