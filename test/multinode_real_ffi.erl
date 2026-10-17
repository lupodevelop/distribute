%% Test-only peer driver. Spawns real BEAM peer nodes via OTP `peer`,
%% routes raw `erlang:send` calls through a TCP control channel so the
%% test process never has to share Subjects with the peer.
%%
%% Modelled on dev/peer_ffi.erl but kept under test/ so the test suite
%% can exercise real cross-node semantics without depending on the
%% manual `gleam dev` playground.
-module(multinode_real_ffi).
-export([
    ensure_distribution/0,
    start_peer/1,
    stop_peer/1,
    with_peer/3,
    connect_peer_to_origin/2,
    prerequisite_unavailable/2,
    race_register_global/3,
    registration_contender/3,
    peer_call_register_global/2,
    peer_call_whereis_global/2,
    connect_peers/2,
    sync_global/1
]).

%% Local skips are visible on stderr, outside EUnit's captured test output.
%% CI is always strict; the explicit flag also enables strict local runs.
prerequisite_unavailable(TestId, Reason) ->
    Message = <<TestId/binary, ": ", Reason/binary>>,
    case enabled("CI") orelse enabled("DISTRIBUTE_REQUIRE_MULTINODE") of
        true -> {error, Message};
        false ->
            io:format(standard_error, "SKIP ~ts~n", [Message]),
            {ok, nil}
    end.

enabled(Key) ->
    case os:getenv(Key) of
        false -> false;
        "" -> false;
        "false" -> false;
        "0" -> false;
        _ -> true
    end.

%% Always stop a started peer, including when an assertion in Body fails.
with_peer(TestId, ShortName, Body) ->
    case start_peer(ShortName) of
        {ok, Peer} ->
            try Body(Peer), {ok, nil}
            after stop_peer(Peer)
            end;
        {error, Reason} ->
            prerequisite_unavailable(TestId, <<"peer start failed: ", Reason/binary>>)
    end.

%% Try to start net_kernel as a long-name node bound to 127.0.0.1.
%% Returns `{ok, NodeBinary}` on success (or already-distributed),
%% `{error, ReasonBinary}` on failure (e.g. epmd not running).
ensure_distribution() ->
    case erlang:is_alive() of
        true ->
            {ok, atom_to_binary(node(), utf8)};
        false ->
            Name = list_to_atom(
                "distribute_test_" ++ integer_to_list(erlang:unique_integer([positive]))
                ++ "@127.0.0.1"),
            case net_kernel:start(Name, #{name_domain => longnames}) of
                {ok, _} ->
                    erlang:set_cookie(node(), 'distribute-test-cookie'),
                    {ok, atom_to_binary(node(), utf8)};
                {error, Reason} ->
                    R = iolist_to_binary(io_lib:format("~p", [Reason])),
                    {error, R}
            end
    end.

%% Start a peer BEAM node sharing the calling node's code path. The
%% peer establishes Erlang distribution back to the origin so
%% `erlang:send` from origin to peer works.
start_peer(ShortName) ->
    Cookie = atom_to_list(erlang:get_cookie()),
    [_, Host] = string:split(atom_to_list(node()), "@"),
    FullName = list_to_atom(binary_to_list(ShortName) ++ "@" ++ Host),
    Args = ["+S", "2:2", "-pa" | code:get_path()] ++ ["-setcookie", Cookie],
    try peer:start_link(#{name => FullName, connection => 0, args => Args,
                          wait_boot => 5000}) of
        {ok, Peer, NodeAtom} ->
            %% Exceptions in an `of` body are not handled by this catch:
            %% only boot failures become skippable prerequisite errors.
            connect_peer_to_origin(Peer, NodeAtom);
        {error, StartReason} ->
            {error, iolist_to_binary(io_lib:format("~p", [StartReason]))}
    catch StartClass:StartException ->
        {error, format_error(StartClass, StartException)}
    end.

connect_peer_to_origin(Peer, NodeAtom) ->
    try
        true = peer:call(Peer, net_kernel, connect_node, [node()]),
        {ok, #{peer => Peer, node => atom_to_binary(NodeAtom, utf8)}}
    catch Class:Reason ->
        peer:stop(Peer),
        error({peer_connection_failed, Class, Reason})
    end.

stop_peer(#{peer := Peer}) ->
    try peer:stop(Peer) catch exit:{noproc, _} -> ok end,
    nil.

%% Register a name on the peer pointing at a Pid spawned ON the peer.
%% The Pid is created via `peer:call(Peer, erlang, spawn, [Module, Fun, Args])`
%% so its `node(Pid)` is the peer node; `:global.register_name` accepts it.
%% Returns `{ok, true}` if global said yes, `{ok, false}` if it refused
%% (already registered), `{error, Reason}` on any RPC failure.
peer_call_register_global(#{peer := Peer}, NameBin) ->
    Name = NameBin,
    try
        %% Spawn a long-lived sleeper process on the peer that we can
        %% keep registered. The sleeper exits when the peer is stopped.
        Sleeper = peer:call(Peer, erlang, spawn, [
            erlang, apply, [fun() -> timer:sleep(infinity) end, []]
        ]),
        Result = peer:call(Peer, global, register_name, [Name, Sleeper]),
        ok = peer:call(Peer, global, sync, []),
        {ok, registration_won(Result)}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

%% Resolve a globally-registered name from the peer's perspective.
%% Returns `{ok, Pid}` when found, `{error, <<"not_found">>}` otherwise,
%% `{error, RpcReason}` on RPC failure.
peer_call_whereis_global(#{peer := Peer}, NameBin) ->
    try
        ok = peer:call(Peer, global, sync, []),
        case peer:call(Peer, global, whereis_name, [NameBin]) of
            undefined -> {error, <<"not_found">>};
            Pid       -> {ok, Pid}
        end
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

%% Force peer A to connect to peer B at the BEAM distribution layer,
%% then run :global.sync on both so they share a unified name table.
%% Without this step two peers spawned from origin may not yet be
%% aware of each other, and `:global.register_name` on the same name
%% can succeed on both before the conflict-resolve callback fires.
connect_peers(#{peer := PeerA, node := _NodeA}, #{peer := PeerB, node := NodeB}) ->
    try
        NodeBAtom = binary_to_atom(NodeB, utf8),
        true = peer:call(PeerA, net_kernel, connect_node, [NodeBAtom]),
        ok = peer:call(PeerA, global, sync, []),
        ok = peer:call(PeerB, global, sync, []),
        {ok, nil}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

%% Trigger a single :global.sync on a peer. Called from the test after
%% both peers have raced register_name to drive `:global`'s
%% conflict-resolve callback to a single winner.
sync_global(#{peer := Peer}) ->
    try
        ok = peer:call(Peer, global, sync, []),
        {ok, nil}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

%% Both contenders run on the peer nodes and announce readiness before
%% either is released. No peer:call waits for a registration to complete
%% before the other registration starts.
race_register_global(#{peer := PeerA}, #{peer := PeerB}, Name) ->
    Ref = make_ref(),
    Parent = self(),
    try
        A = peer:call(PeerA, erlang, spawn,
                      [?MODULE, registration_contender, [Parent, Ref, Name]]),
        B = peer:call(PeerB, erlang, spawn,
                      [?MODULE, registration_contender, [Parent, Ref, Name]]),
        MonA = monitor(process, A),
        MonB = monitor(process, B),
        try
            Deadline = erlang:monotonic_time(millisecond) + 5000,
            await_contender(Ref, ready, A, MonA, Deadline),
            await_contender(Ref, ready, B, MonB, Deadline),
            A ! {Ref, go},
            B ! {Ref, go},
            ResultA = await_contender(Ref, result, A, MonA, Deadline),
            ResultB = await_contender(Ref, result, B, MonB, Deadline),
            {ok, {registration_won(ResultA), registration_won(ResultB)}}
        after
            demonitor(MonA, [flush]),
            demonitor(MonB, [flush])
        end
    catch Class:Reason ->
        {error, format_error(Class, Reason)}
    end.

registration_contender(Parent, Ref, Name) ->
    Parent ! {Ref, ready, self()},
    receive {Ref, go} -> ok after 5000 -> exit(barrier_timeout) end,
    Result = global:register_name(Name, self()),
    Parent ! {Ref, result, self(), Result},
    %% Keep the winning registration alive until its peer is stopped.
    receive stop -> ok end.

await_contender(Ref, Kind, Pid, Mon, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, ready, Pid} when Kind =:= ready -> ok;
        {Ref, result, Pid, Result} when Kind =:= result -> Result;
        {'DOWN', Mon, process, Pid, Reason} -> error({contender_died, Reason})
    after Remaining -> error({contender_timeout, Kind})
    end.

registration_won(yes) -> true;
registration_won(no) -> false;
registration_won(Other) -> error({unexpected_registration_result, Other}).

format_error(Class, Reason) ->
    iolist_to_binary(io_lib:format("~p:~p", [Class, Reason])).
