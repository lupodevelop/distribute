-module(multinode_prerequisite_test).
-include_lib("eunit/include/eunit.hrl").

missing_prerequisite_fails_in_ci_test() ->
    with_environment("true", false, fun() ->
        ?assertEqual({error, <<"Z2: distribution unavailable">>},
            multinode_real_ffi:prerequisite_unavailable(
                <<"Z2">>, <<"distribution unavailable">>))
    end).

explicit_requirement_fails_without_ci_test() ->
    with_environment(false, "1", fun() ->
        ?assertEqual({error, <<"Z3: peer start failed">>},
            multinode_real_ffi:prerequisite_unavailable(
                <<"Z3">>, <<"peer start failed">>))
    end).

ci_requirement_cannot_be_disabled_by_local_flag_test() ->
    with_environment("true", "false", fun() ->
        ?assertMatch({error, _},
            multinode_real_ffi:prerequisite_unavailable(<<"Z3">>, <<"missing">>))
    end).

peer_is_stopped_when_test_body_raises_test() ->
    case multinode_real_ffi:ensure_distribution() of
        {error, Reason} ->
            ?assertEqual({ok, nil}, multinode_real_ffi:prerequisite_unavailable(
                <<"peer cleanup">>, Reason));
        {ok, _} ->
            Key = {?MODULE, make_ref()},
            Name = <<"cleanup_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            try
                Outcome = catch multinode_real_ffi:with_peer(
                    <<"peer cleanup">>, Name, fun(#{peer := Controller}) ->
                        put(Key, Controller),
                        error(expected_test_failure)
                    end),
                case get(Key) of
                    undefined -> ?assertEqual({ok, nil}, Outcome);
                    Controller ->
                        ?assertMatch({'EXIT', {expected_test_failure, _}}, Outcome),
                        ?assertNot(is_process_alive(Controller))
                end
            after erase(Key)
            end
    end.

registration_waits_for_barrier_release_test() ->
    Parent = self(),
    Ref = make_ref(),
    Name = <<"barrier_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    {Pid, Mon} = spawn_monitor(fun() ->
        multinode_real_ffi:registration_contender(Parent, Ref, Name)
    end),
    try
        receive
            {Ref, ready, Pid} -> ok;
            {'DOWN', Mon, process, Pid, Reason} -> error({contender_died, Reason})
        after 1000 -> error(contender_not_ready)
        end,
        ?assertEqual(undefined, global:whereis_name(Name)),
        Pid ! {Ref, go},
        receive
            {Ref, result, Pid, yes} -> ok;
            {'DOWN', Mon, process, Pid, Reason2} -> error({contender_died, Reason2})
        after 1000 -> error(registration_did_not_finish)
        end,
        ?assertEqual(Pid, global:whereis_name(Name))
    after
        CleanupMon = monitor(process, Pid),
        exit(Pid, kill),
        receive {'DOWN', CleanupMon, process, Pid, _} -> ok after 1000 -> error(cleanup_timeout) end,
        demonitor(Mon, [flush])
    end.

post_boot_connection_failure_is_not_skipped_locally_test() ->
    %% An unnamed peer can boot over stdio but cannot join distribution.
    %% This is a real post-boot connection failure, not a mocked RPC result.
    {ok, Peer, Node} = peer:start_link(#{connection => standard_io,
        args => ["+S", "2:2"], wait_boot => 5000}),
    try
        with_environment(false, false, fun() ->
            ?assertError({peer_connection_failed, error, {badmatch, ignored}},
                multinode_real_ffi:connect_peer_to_origin(Peer, Node))
        end),
        ?assertNot(is_process_alive(Peer))
    after
        case is_process_alive(Peer) of true -> peer:stop(Peer); false -> ok end
    end.

with_environment(CI, Required, Run) ->
    OldCI = os:getenv("CI"),
    OldRequired = os:getenv("DISTRIBUTE_REQUIRE_MULTINODE"),
    try
        set_env("CI", CI),
        set_env("DISTRIBUTE_REQUIRE_MULTINODE", Required),
        Run()
    after
        set_env("CI", OldCI),
        set_env("DISTRIBUTE_REQUIRE_MULTINODE", OldRequired)
    end.

set_env(Key, false) -> os:unsetenv(Key);
set_env(Key, Value) -> os:putenv(Key, Value).
