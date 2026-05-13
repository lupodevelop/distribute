-module(peer_ffi).
-export([start_peer/1, stop_peer/1, peer_node/1, send_to_actor/3,
         peer_connected_nodes/1, send_pid_from_peer/4]).

%% Start a peer BEAM node that shares this node's code path.
%% The peer can therefore load all distribute modules without extra setup.
%%
%% Returns {ok, PeerMap} | {error, Reason}.
%% PeerMap is an Erlang map #{peer => PeerRef, node => NodeBinary}.
start_peer(ShortName) ->
    Cookie = atom_to_list(erlang:get_cookie()),
    %% Force the peer to use the same host as the calling node.
    %% Without this, peer inherits inet:gethostname() which may differ from
    %% the IP-based name (e.g. "127.0.0.1" vs "MBP-host.local"), causing
    %% the distribution layer to fail when routing messages between nodes.
    [_, Host] = string:split(atom_to_list(node()), "@"),
    FullName = list_to_atom(binary_to_list(ShortName) ++ "@" ++ Host),
    Args = ["-pa" | code:get_path()] ++ ["-setcookie", Cookie],
    %% `connection => 0` tells peer to open an auto-selected TCP port as the
    %% alternative control channel.  This is what makes peer:call/4 work.
    %% Without a `connection` value (or with the invalid atom `dist`), the port
    %% is immediately closed and peer_state.connection = undefined, which causes
    %% peer:call to throw noconnection.
    %%
    %% After peer:start we explicitly call net_kernel:connect_node(Origin) FROM
    %% THE PEER (via the TCP control channel) to establish real Erlang
    %% distribution so that erlang:send(MainPid, Msg) works across nodes.
    case peer:start(#{name => FullName, connection => 0, args => Args}) of
        {ok, Peer, NodeAtom} ->
            Origin = node(),
            case peer:call(Peer, net_kernel, connect_node, [Origin]) of
                true ->
                    {ok, #{peer => Peer, node => atom_to_binary(NodeAtom, utf8)}};
                false ->
                    peer:stop(Peer),
                    {error, <<"connect_to_origin_failed: net_kernel:connect_node returned false">>};
                ignored ->
                    peer:stop(Peer),
                    {error, <<"origin_not_distributed">>}
            end;
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Stop a peer node gracefully.
stop_peer(#{peer := Peer}) ->
    ok = peer:stop(Peer),
    nil.

%% Extract the node name binary from a peer map.
peer_node(#{node := Node}) ->
    Node.

%% Returns ALL connected nodes from the peer's perspective (visible + hidden).
%% The peer connects to the calling node as a hidden node by default,
%% so erlang:nodes() alone would return []. Use erlang:nodes(connected).
peer_connected_nodes(#{peer := Peer}) ->
    try
        Nodes = peer:call(Peer, erlang, nodes, [connected]),
        [atom_to_binary(N, utf8) || N <- Nodes]
    catch _:_ -> []
    end.

%% Send directly to a known Pid from the peer (bypasses :global lookup).
send_pid_from_peer(#{peer := Peer}, Pid, Tag, Binary) ->
    try
        peer:call(Peer, erlang, send, [Pid, {Tag, Binary}]),
        {ok, nil}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

send_to_actor(#{peer := Peer}, ActorName, EncodedBinary) ->
    try
        %% Force a full :global sync on the peer before looking up names.
        %% Without this, the peer may not yet see names registered on the
        %% main node, and lookups during the sync window throw noconnection.
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
