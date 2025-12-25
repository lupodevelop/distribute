%% Gossip protocol FFI for membership state propagation.
%% Provides functions to get local view, send/receive gossip, and merge entries.
-module(gossip_ffi).
-export([local_view/0, send_gossip/2, receive_gossip/1, merge_entry/4]).

-define(TABLE, membership_table).

%% Get the local gossip view as a list of {Node, Status, Incarnation, Timestamp} tuples.
local_view() ->
    ensure_table(),
    Tab = ?TABLE,
    FoldFun = fun({Node, Status, Inc, _Fails, TS}, Acc) ->
        [{Node, atom_to_list(Status), Inc, TS} | Acc]
    end,
    ets:foldl(FoldFun, [], Tab).

%% Send gossip to a remote node via RPC.
send_gossip(NodeStr, Entries) when is_binary(NodeStr) ->
    send_gossip(binary_to_list(NodeStr), Entries);
send_gossip(NodeStr, Entries) when is_list(NodeStr), is_list(Entries) ->
    Node = list_to_atom(NodeStr),
    catch rpc:call(Node, ?MODULE, receive_gossip, [Entries], 1000),
    ok.

%% Receive gossip from a peer and merge into local state.
receive_gossip(Entries) when is_list(Entries) ->
    lists:foreach(fun(Entry) ->
        case Entry of
            {Node, Status, Inc, TS} when is_list(Node), is_list(Status), is_integer(Inc), is_integer(TS) ->
                merge_entry(Node, Status, Inc, TS);
            {Node, Status, Inc, TS} when is_binary(Node) ->
                merge_entry(binary_to_list(Node), binary_to_list(Status), Inc, TS);
            _ -> ok
        end
    end, Entries),
    ok.

%% Merge a single entry into the local membership table.
%% Rule: higher incarnation wins; on tie, alive > suspect > dead.
merge_entry(NodeStr, StatusStr, Inc, TS) when is_binary(NodeStr) ->
    merge_entry(binary_to_list(NodeStr), binary_to_list(StatusStr), Inc, TS);
merge_entry(NodeStr, StatusStr, Inc, TS) when is_list(NodeStr), is_list(StatusStr), is_integer(Inc), is_integer(TS) ->
    ensure_table(),
    Status = case StatusStr of
        "alive" -> alive;
        "suspect" -> suspect;
        _ -> dead
    end,
    case ets:lookup(?TABLE, NodeStr) of
        [] ->
            %% New node, insert directly
            ets:insert(?TABLE, {NodeStr, Status, Inc, 0, TS});
        [{NodeStr, OldStatus, OldInc, _OldFails, _OldTS}] ->
            %% Merge based on incarnation and status priority
            ShouldUpdate = case Inc > OldInc of
                true -> true;
                false when Inc == OldInc ->
                    status_priority(Status) > status_priority(OldStatus);
                false -> false
            end,
            case ShouldUpdate of
                true ->
                    ets:insert(?TABLE, {NodeStr, Status, Inc, 0, TS});
                false ->
                    ok
            end
    end,
    ok;
merge_entry(_, _, _, _) ->
    ok.

%% Status priority: alive > suspect > dead
status_priority(alive) -> 3;
status_priority(suspect) -> 2;
status_priority(dead) -> 1;
status_priority(_) -> 0.

%% Ensure the membership table exists.
ensure_table() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end.
