%% SWIM-like membership background service
%% - Periodically probes nodes using direct ping
%% - On direct ping failure, performs indirect ping via K helpers (RPC)
%% - Maintains ETS table with {NodeString, StatusAtom, Fails}
%% - Performs simple gossip by pushing local view to random peers

-module(membership_ffi).
-export([start/1, start/4, stop/0, members_with_status/0, alive/0, suspect/0, current_leader/0, helper_ping/1, gossip_receive/1, metrics/0, metrics_increment/1, metrics_get/1]).

-define(TABLE, membership_table).
-define(PROC_NAME, membership_manager).

%% start(IntervalMs)
start(IntervalMs) when is_integer(IntervalMs), IntervalMs > 0 ->
    %% defaults: K=3, SuspectThresh=2, DeadThresh=5
    start(IntervalMs, 3, 2, 5).

start(IntervalMs, K, SuspectThresh, DeadThresh) ->
    case whereis(?PROC_NAME) of
        undefined ->
            _ = ensure_table(),
            %% ensure rand seeded
            Seed1 = erlang:monotonic_time(),
            Seed2 = erlang:phash2(erlang:unique_integer([monotonic])),
            Seed3 = erlang:node(),
            _ = catch rand:seed(exs1024s, {Seed1 rem 4294967295, Seed2 rem 4294967295, erlang:phash2(Seed3, 4294967295)}),
            P = spawn(fun() -> loop(IntervalMs, K, SuspectThresh, DeadThresh) end),
            register(?PROC_NAME, P),
            ok;
        _Pid -> ok
    end.

stop() ->
    case whereis(?PROC_NAME) of
        undefined -> ok;
        Pid -> unregister(?PROC_NAME), exit(Pid, kill), ets:delete(?TABLE), ok
    end.

members_with_status() ->
    ensure_table(),
    Tab = ?TABLE,
    FoldFun = fun({K, VStatus, Inc, _Fails, TS}, Acc) -> [{K, atom_to_list(VStatus), Inc, TS} | Acc] end,
    ets:foldl(FoldFun, [], Tab).

%% Expose simple metrics (ping_success, ping_fail, suspect_count)
metrics() ->
    %% Ensure metrics table exists and return string keys for FFI-friendly format
    ensure_metrics_table(),
    [ {atom_to_list(ping_success), get_counter(ping_success)},
      {atom_to_list(ping_fail), get_counter(ping_fail)},
      {atom_to_list(suspect_count), get_counter(suspect_count)} ].

get_counter(Name) ->
    ensure_metrics_table(),
    case ets:lookup(metrics_table, Name) of
        [{_, V}] -> V;
        [] -> 0
    end.

alive() ->
    ensure_table(),
    Tab = ?TABLE,
    FoldFun = fun({K, V, _Inc, _Fails, _TS}, Acc) -> case V of alive -> [K | Acc]; _ -> Acc end end,
    lists:reverse(ets:foldl(FoldFun, [], Tab)).

suspect() ->
    ensure_table(),
    Tab = ?TABLE,
    FoldFun = fun({K, V, _Inc, _Fails, _TS}, Acc) -> case V of suspect -> [K | Acc]; _ -> Acc end end,
    lists:reverse(ets:foldl(FoldFun, [], Tab)).

current_leader() ->
    Alive = alive(),
    case Alive of
        [] -> "";
        _ -> lists:max(Alive)
    end.

ensure_table() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end.

ensure_metrics_table() ->
    case ets:info(metrics_table) of
        undefined -> ets:new(metrics_table, [named_table, public, set]),
                     ets:insert(metrics_table, {ping_success, 0}),
                     ets:insert(metrics_table, {ping_fail, 0}),
                     ets:insert(metrics_table, {suspect_count, 0}),
                     ok;
        _ -> ok
    end.

%% helper RPC function used by indirect pings
helper_ping(Target) ->
    %% Target is an atom node name
    case net_adm:ping(Target) of
        pong -> pong;
        _ -> pang
    end.

%% Receive gossip from peer: View is list of {NodeStr, StatusStr}
gossip_receive(View) when is_list(View) ->
    ensure_table(),
    lists:foreach(fun({N, StatusStr}) ->
        Status = case StatusStr of
            "alive" -> alive;
            "suspect" -> suspect;
            _ -> dead
        end,
        ets:insert(?TABLE, {N, Status, 0})
    end, View),
    ok.

%% Main loop: probe nodes and gossip
loop(IntervalMs, K, SuspectThresh, DeadThresh) ->
    ensure_table(),
    Self = node(),
    Nodes = lists:delete(Self, nodes()),
    %% Build local view map for gossip
    LocalView = gather_local_view(Nodes),
    %% For each node, perform direct ping then indirect if needed
    lists:foreach(fun(Target) ->
                TargetStr = atom_to_list(Target),
                case net_adm:ping(Target) of
            pong ->
                ensure_metrics_table(),
                increment_counter(ping_success),
                %% On recovery, increment incarnation if previously not alive
                case ets:lookup(?TABLE, TargetStr) of
                    [] -> ets:insert(?TABLE, {TargetStr, alive, 0, 0, erlang:monotonic_time()});
                    [{_, PrevStatus, PrevInc, _, _}] ->
                        NewInc = case PrevStatus of
                            alive -> PrevInc;
                            _ -> PrevInc + 1
                        end,
                        ets:insert(?TABLE, {TargetStr, alive, NewInc, 0, erlang:monotonic_time()})
                end;
            _ ->
                ensure_metrics_table(),
                increment_counter(ping_fail),
                %% indirect ping via K helpers
                Helpers = pick_helpers(Nodes, Target, K),
                Res = lists:map(fun(H) ->
                    catch rpc:call(H, ?MODULE, helper_ping, [Target], 1000)
                end, Helpers),
                case lists:any(fun(X) -> X == pong end, Res) of
                    true -> ets:insert(?TABLE, {TargetStr, alive, 0, 0, erlang:monotonic_time()});
                    false -> inc_fail(TargetStr, SuspectThresh, DeadThresh)
                end
        end
    end, Nodes),
    %% Gossip local view to a random subset of peers (size K)
    GossipPeers = pick_random_peers(Nodes, K),
    lists:foreach(fun(Peer) ->
        catch rpc:call(Peer, ?MODULE, gossip_receive, [LocalView], 1000)
    end, GossipPeers),
    timer:sleep(IntervalMs),
    loop(IntervalMs, K, SuspectThresh, DeadThresh).

gather_local_view(_Nodes) ->
    ensure_table(),
    Tab = ?TABLE,
    FoldFun = fun({K, V, Inc, _Fails, TS}, Acc) -> [{K, atom_to_list(V), Inc, TS} | Acc] end,
    ets:foldl(FoldFun, [], Tab).

pick_helpers(Nodes, Target, K) ->
    Candidates = lists:filter(fun(N) -> N =/= Target end, Nodes),
    pick_random(Candidates, Target, K).

pick_random_peers(Nodes, K) ->
    pick_random(Nodes, node(), K).

%% pick_random: use a deterministic rotation based on a hash of the seed
pick_random(List, _Seed, K) when K >= length(List) -> List;
pick_random(List, Seed, K) ->
    Len = length(List),
    Rot = erlang:phash2(Seed, Len),
    RotList = rotate(List, Rot),
    take_k(RotList, K).

rotate(List, 0) -> List;
rotate([H|T], N) -> rotate(T ++ [H], N - 1).

take_k(List, K) -> lists:sublist(List, K).

inc_fail(TargetStr, SuspectThresh, DeadThresh) ->
    Now = erlang:monotonic_time(),
    case ets:lookup(?TABLE, TargetStr) of
        [] -> ets:insert(?TABLE, {TargetStr, suspect, 0, 1, Now});
        [{TargetStr, Status, Inc, Fails, _TS}] ->
            NewFails = Fails + 1,
            NewStatus = case NewFails of
                N when N >= DeadThresh -> dead;
                N when N >= SuspectThresh -> suspect;
                _ -> Status
            end,
            ets:insert(?TABLE, {TargetStr, NewStatus, Inc, NewFails, Now}),
            %% update suspect metric
            case NewStatus of
                suspect -> increment_counter(suspect_count);
                dead -> increment_counter(suspect_count);
                _ -> ok
            end
    end.

increment_counter(Name) ->
    ensure_metrics_table(),
    case ets:lookup(metrics_table, Name) of
        [{Name, V}] -> ets:insert(metrics_table, {Name, V + 1});
        [] -> ets:insert(metrics_table, {Name, 1})
    end.

metrics_increment(NameStr) when is_list(NameStr) ->
    %% Map known metric string names to atoms to avoid unbounded atom creation
    Name = case NameStr of
        "ping_success" -> ping_success;
        "ping_fail" -> ping_fail;
        "suspect_count" -> suspect_count;
        _ -> ping_fail
    end,
    increment_counter(Name),
    ok;
metrics_increment(NameBin) when is_binary(NameBin) ->
    metrics_increment(binary_to_list(NameBin)).

metrics_get(NameStr) when is_list(NameStr) ->
    Name = case NameStr of
        "ping_success" -> ping_success;
        "ping_fail" -> ping_fail;
        "suspect_count" -> suspect_count;
        _ -> ping_fail
    end,
    get_counter(Name);
metrics_get(NameBin) when is_binary(NameBin) ->
    metrics_get(binary_to_list(NameBin)).
