-module(atom_budget_test_ffi).
-export([normal_exit/0, killed_caller/0, concurrent_first_callers/0,
         killed_denied_callers/0,
         run_scenario/1]).

normal_exit() -> fresh_vm(normal_exit).
killed_caller() -> fresh_vm(killed_caller).
concurrent_first_callers() -> fresh_vm(concurrent_first_callers).
killed_denied_callers() -> fresh_vm(killed_denied_callers).

%% Exercise cold initialisation without erasing VM-global state in the
%% test runner. No distribution or epmd is required. A subprocess also
%% keeps minted atoms out of the suite.
fresh_vm(Scenario) ->
    Eval = lists:flatten(io_lib:format(
        "try atom_budget_test_ffi:run_scenario(~p), halt(0) "
        "catch C:R:S -> io:format(\"~~p:~~p~~n~~p~~n\", [C,R,S]), halt(1) end.",
        [Scenario]
    )),
    Port = open_port({spawn_executable, os:find_executable("erl")}, [
        binary, exit_status, use_stdio, stderr_to_stdout,
        {args, ["+S", "2:2", "-noshell", "-pa",
                filename:dirname(code:which(?MODULE)), "-eval", Eval]}
    ]),
    collect_output(Port, <<>>).

collect_output(Port, Output) ->
    receive
        {Port, {data, Data}} -> collect_output(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, 0}} -> {ok, nil};
        {Port, {exit_status, _}} -> {error, Output}
    after 15000 ->
        port_close(Port),
        {error, <<"atom-budget subprocess timed out">>}
    end.

run_scenario(Scenario) when Scenario =:= normal_exit; Scenario =:= killed_caller ->
    config_ffi:put_atom_budget(1),
    Parent = self(),
    Name = <<"budget_first@localhost">>,
    {Pid, Mon} = spawn_monitor(fun() ->
        Result = distribute_ffi_utils:to_node_atom_safe(Name),
        Parent ! {self(), Result},
        receive stop -> ok end
    end),
    receive {Pid, {ok, _}} -> ok after 5000 -> error(first_caller_failed) end,
    case Scenario of
        normal_exit -> Pid ! stop;
        killed_caller -> exit(Pid, kill)
    end,
    await_down(Pid, Mon),
    %% Existing atoms stay usable, but node and cookie paths share the
    %% exhausted budget even after the initialising process is gone.
    {ok, _} = distribute_ffi_utils:to_node_atom_safe(Name),
    {error, atom_budget_exceeded} =
        distribute_ffi_utils:to_node_atom_safe(<<"budget_second@localhost">>),
    {error, atom_budget_exceeded} =
        distribute_ffi_utils:to_cookie_atom_safe(<<"budget_cookie">>),
    %% Changing configuration must not reset the running count.
    config_ffi:put_atom_budget(2),
    {ok, _} = distribute_ffi_utils:to_cookie_atom_safe(<<"budget_cookie">>),
    {error, atom_budget_exceeded} =
        distribute_ffi_utils:to_node_atom_safe(<<"budget_third@localhost">>),
    %% The explicit test-only reset remains supported.
    distribute_ffi_utils:atom_budget_reset(),
    {ok, _} = distribute_ffi_utils:to_node_atom_safe(<<"budget_after_reset@localhost">>),
    ok;
run_scenario(concurrent_first_callers) ->
    Budget = 8,
    config_ffi:put_atom_budget(Budget),
    Parent = self(),
    Workers = [spawn_monitor(fun() ->
        receive go -> ok end,
        Name = <<"budget_racer_", (integer_to_binary(I))/binary, "@localhost">>,
        Parent ! {self(), distribute_ffi_utils:to_node_atom_safe(Name)}
    end) || I <- lists:seq(1, 64)],
    [Pid ! go || {Pid, _} <- Workers],
    Results = [receive
        {Pid, Result} -> await_down(Pid, Mon), Result;
        {'DOWN', Mon, process, Pid, Reason} -> error({caller_crashed, Reason})
    after 5000 -> error(missing_result)
    end || {Pid, Mon} <- Workers],
    Budget = length([ok || {ok, _} <- Results]),
    56 = length([ok || {error, atom_budget_exceeded} <- Results]),
    {error, atom_budget_exceeded} =
        distribute_ffi_utils:to_node_atom_safe(<<"budget_after_racers@localhost">>),
    ok;
run_scenario(killed_denied_callers) ->
    %% Repeated cancellation while already at the cap must not consume
    %% capacity. In particular, a denied caller killed between a speculative
    %% increment and its rollback must not leave the budget inflated.
    [denied_caller_churn(I) || I <- lists:seq(1, 10)],
    ok.

denied_caller_churn(I) ->
    distribute_ffi_utils:atom_budget_reset(),
    config_ffi:put_atom_budget(1),
    Prefix = <<"budget_churn_", (integer_to_binary(I))/binary>>,
    {ok, _} = distribute_ffi_utils:to_cookie_atom_safe(Prefix),
    Parent = self(),
    Workers = [spawn_monitor(fun() ->
        Parent ! {self(), ready},
        denied_loop()
    end) || _ <- lists:seq(1, 256)],
    [receive {Pid, ready} -> ok after 5000 -> error(worker_not_ready) end
     || {Pid, _} <- Workers],
    %% Let the two schedulers exercise the saturated path before cancellation.
    receive after 10 -> ok end,
    [exit(Pid, kill) || {Pid, _} <- Workers],
    [await_down(Pid, Mon) || {Pid, Mon} <- Workers],
    config_ffi:put_atom_budget(2),
    {ok, _} = distribute_ffi_utils:to_cookie_atom_safe(<<Prefix/binary, "_next">>),
    {error, atom_budget_exceeded} =
        distribute_ffi_utils:to_cookie_atom_safe(<<Prefix/binary, "_refused">>),
    ok.

denied_loop() ->
    {error, atom_budget_exceeded} =
        distribute_ffi_utils:to_cookie_atom_safe(<<"budget_churn_never_created">>),
    denied_loop().

await_down(Pid, Mon) ->
    receive
        {'DOWN', Mon, process, Pid, normal} -> ok;
        {'DOWN', Mon, process, Pid, killed} -> ok;
        {'DOWN', Mon, process, Pid, Reason} -> error({caller_crashed, Reason})
    after 5000 -> error(caller_still_alive)
    end.
