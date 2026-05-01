%% Test helpers for `conflict_test`. Replicate the protected
%% invocation path in `conflict_ffi:safe_invoke/5` with a tightened
%% timeout so we can exercise timeout/crash behaviour without
%% waiting for the production timeout. Keeps the production
%% module untouched.
-module(conflict_test_ffi).
-export([invoke_wrapped_resolver/4,
         invoke_wrapped_resolver_kill_both/4,
         node_name_for_self/0,
         bogus_term_as_pid/0]).

%% Tightened timeout for the timeout/panic tests: 200 ms is well
%% above scheduler noise but short enough that a sleeping resolver
%% trips it quickly.
-define(TEST_TIMEOUT_MS, 200).

%% Mirror of `conflict_ffi:safe_invoke/5` with the test timeout.
%% Returns the surviving Pid (or `none`) so the test can compare
%% against expected fallback behaviour.
invoke_wrapped_resolver(Name, Pid1, Pid2, Resolver) ->
    safe_invoke(Resolver, Name, Pid1, Pid2, ?TEST_TIMEOUT_MS).

%% Variant for the KillBoth path: returns `true` when the wrapped
%% resolver returned `none` (the value :global expects to delete
%% both conflicting registrations).
invoke_wrapped_resolver_kill_both(Name, Pid1, Pid2, Resolver) ->
    Result = safe_invoke(Resolver, Name, Pid1, Pid2, ?TEST_TIMEOUT_MS),
    Result =:= none.

%% Local node name as a binary, matched against the same value
%% `distribute/conflict:pid_node_name/1` returns. Used by the
%% `node_priority` test that lists the local node explicitly.
node_name_for_self() ->
    atom_to_binary(node(), utf8).

%% A non-Pid term coerced through the Pid type for the negative
%% test on `register_with_resolver_ffi`. Erlang accepts the dialyzer
%% complaint at compile time; the call site verifies the FFI
%% returns `{error, invalid_process}` instead of crashing.
bogus_term_as_pid() ->
    not_a_pid.

%% --- copied from conflict_ffi.erl with a parameterised timeout ---
%%
%% We do NOT call into conflict_ffi:safe_invoke/5 directly because
%% it is a private function. Replicating it here keeps the test
%% self-contained and avoids exporting a test-only entry on the
%% production module.
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
    receive
        {Ref, {keep, WinnerPid}} when is_pid(WinnerPid) ->
            cleanup_worker_monitor(WorkerMon),
            telemetry_ffi:emit({conflict_resolved, Name, {some, WinnerPid}}),
            WinnerPid;
        {Ref, kill_both} ->
            cleanup_worker_monitor(WorkerMon),
            telemetry_ffi:emit({conflict_resolved, Name, none}),
            none;
        {Ref, {crashed, ReasonBin}} ->
            cleanup_worker_monitor(WorkerMon),
            telemetry_ffi:emit({conflict_resolver_failed, Name, ReasonBin}),
            run_fallback(Name, Pid1, Pid2);
        {Ref, _Other} ->
            cleanup_worker_monitor(WorkerMon),
            telemetry_ffi:emit({conflict_resolver_failed, Name,
                <<"resolver returned non-ConflictOutcome value">>}),
            run_fallback(Name, Pid1, Pid2);
        {'DOWN', WorkerMon, process, Worker, Reason} ->
            telemetry_ffi:emit(
                {conflict_resolver_failed, Name, format_worker_down(Reason)}
            ),
            drain_resolver_reply(Ref),
            run_fallback(Name, Pid1, Pid2)
    after TimeoutMs ->
        erlang:exit(Worker, kill),
        await_worker_down(WorkerMon, Worker),
        drain_resolver_reply(Ref),
        telemetry_ffi:emit({conflict_resolver_failed, Name,
            <<"resolver timed out">>}),
        run_fallback(Name, Pid1, Pid2)
    end.

translate_outcome({keep, Pid}) when is_pid(Pid) -> {keep, Pid};
translate_outcome(kill_both) -> kill_both;
translate_outcome(Other) -> {malformed, Other}.

run_fallback(Name, Pid1, Pid2) ->
    Winner = case Pid1 < Pid2 of
        true  -> Pid1;
        false -> Pid2
    end,
    telemetry_ffi:emit({conflict_resolved, Name, {some, Winner}}),
    Winner.

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
