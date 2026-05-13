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
    peer_call_register_global/2,
    peer_call_whereis_global/2,
    connect_peers/2,
    sync_global/1
]).

%% Try to start net_kernel as a short-name node bound to 127.0.0.1.
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
            case net_kernel:start([Name, longnames]) of
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
    Args = ["-pa" | code:get_path()] ++ ["-setcookie", Cookie],
    case peer:start(#{name => FullName, connection => 0, args => Args}) of
        {ok, Peer, NodeAtom} ->
            Origin = node(),
            case peer:call(Peer, net_kernel, connect_node, [Origin]) of
                true ->
                    {ok, #{peer => Peer, node => atom_to_binary(NodeAtom, utf8)}};
                false ->
                    peer:stop(Peer),
                    {error, <<"connect_to_origin_failed">>};
                ignored ->
                    peer:stop(Peer),
                    {error, <<"origin_not_distributed">>}
            end;
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

stop_peer(#{peer := Peer}) ->
    catch peer:stop(Peer),
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
        peer:call(Peer, global, sync, []),
        {ok, Result}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

%% Resolve a globally-registered name from the peer's perspective.
%% Returns `{ok, Pid}` when found, `{error, not_found}` otherwise,
%% `{error, RpcReason}` on RPC failure.
peer_call_whereis_global(#{peer := Peer}, NameBin) ->
    try
        peer:call(Peer, global, sync, []),
        case peer:call(Peer, global, whereis_name, [NameBin]) of
            undefined -> {error, not_found};
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
        peer:call(PeerA, global, sync, []),
        peer:call(PeerB, global, sync, []),
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
        peer:call(Peer, global, sync, []),
        {ok, nil}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.
