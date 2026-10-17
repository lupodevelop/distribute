-module(multinode_peer_test_ffi).
-export([peer_connected_nodes/1, send_to_actor/3]).

%% Minimal peer-node helper for automated tests.

peer_connected_nodes(#{peer := Peer}) ->
    try
        Nodes = peer:call(Peer, erlang, nodes, [connected]),
        [atom_to_binary(N, utf8) || N <- Nodes]
    catch _:_ -> []
    end.

send_to_actor(#{peer := Peer}, ActorName, EncodedBinary) ->
    try
        ok = peer:call(Peer, global, sync, []),
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
