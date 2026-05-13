-module(multinode_peer_test_ffi).
-export([start_peer/1, stop_peer/1, peer_connected_nodes/1, send_to_actor/3]).

%% Minimal peer-node helper for automated tests.

start_peer(ShortName) ->
    Cookie = atom_to_list(erlang:get_cookie()),
    [_, Host] = string:split(atom_to_list(node()), "@"),
    FullName = list_to_atom(binary_to_list(ShortName) ++ "@" ++ Host),
    Args = ["-pa" | code:get_path()] ++ ["-setcookie", Cookie],
    case peer:start(#{name => FullName, connection => 0, args => Args}) of
        {ok, Peer, _NodeAtom} ->
            Origin = node(),
            case peer:call(Peer, net_kernel, connect_node, [Origin]) of
                true -> {ok, Peer};
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

stop_peer(Peer) ->
    ok = peer:stop(Peer),
    nil.

peer_connected_nodes(Peer) ->
    try
        Nodes = peer:call(Peer, erlang, nodes, [connected]),
        [atom_to_binary(N, utf8) || N <- Nodes]
    catch _:_ -> []
    end.

send_to_actor(Peer, ActorName, EncodedBinary) ->
    try
        peer:call(Peer, global, sync, []),
        case peer:call(Peer, global, whereis_name, [ActorName]) of
            undefined ->
                {error, <<"actor_not_found">>};
            Pid ->
                peer:call(Peer, erlang, send, [Pid, {ActorName, EncodedBinary}]),
                {ok, nil}
        end
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.