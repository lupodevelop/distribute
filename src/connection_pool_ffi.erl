%% Connection pool FFI for Gleam distribute library
%% Uses ETS for atomic counters and process-based pool management
%% module declaration
-module(connection_pool_ffi).
-export([new_pool/2, get_connection/1, release_connection/1,
         pool_stats/1, destroy_pool/1, is_ok/1, get_error/1, stress_test_pool/3]).

%% Create a new connection pool backed by ETS
%% Returns {ok, PoolId} or {error, Reason}
new_pool(TargetNode, MaxConnections) when is_binary(TargetNode), is_integer(MaxConnections) ->
    PoolId = erlang:unique_integer([positive]),
    TableName = list_to_atom("pool_" ++ integer_to_list(PoolId)),
    try
        ets:new(TableName, [named_table, public, set]),
        ets:insert(TableName, {target_node, TargetNode}),
        ets:insert(TableName, {max_connections, MaxConnections}),
        ets:insert(TableName, {active_count, 0}),
        ets:insert(TableName, {available_count, MaxConnections}),
        {ok, {pool, PoolId, TableName}}
    catch
        _:Reason -> {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end;
new_pool(_, _) ->
    {error, <<"invalid_arguments">>}.

%% Get a connection from the pool (atomic increment)
%% Returns {ok, ConnectionId} or {error, pool_exhausted}
get_connection({pool, _PoolId, TableName}) ->
    try
        [{max_connections, Max}] = ets:lookup(TableName, max_connections),
        case ets:update_counter(TableName, active_count, {2, 1, Max, Max}) of
            Max ->
                %% We hit the limit, decrement back
                ets:update_counter(TableName, active_count, {2, -1, 0, 0}),
                {error, pool_exhausted};
            Active when Active < Max ->
                ets:update_counter(TableName, available_count, {2, -1, 0, 0}),
                ConnId = erlang:unique_integer([positive]),
                {ok, {connection, ConnId, TableName}}
        end
    catch
        _:_ -> {error, pool_exhausted}
    end;
get_connection(_) ->
    {error, <<"invalid_pool">>}.

%% Release a connection back to the pool
release_connection({connection, _ConnId, TableName}) ->
    try
        ets:update_counter(TableName, active_count, {2, -1, 0, 0}),
        ets:update_counter(TableName, available_count, {2, 1}),
        ok
    catch
        _:_ -> {error, <<"release_failed">>}
    end;
release_connection(_) ->
    {error, <<"invalid_connection">>}.

%% Get pool statistics
pool_stats({pool, _PoolId, TableName}) ->
    try
        [{target_node, TargetNode}] = ets:lookup(TableName, target_node),
        [{max_connections, Max}] = ets:lookup(TableName, max_connections),
        [{active_count, Active}] = ets:lookup(TableName, active_count),
        [{available_count, Available}] = ets:lookup(TableName, available_count),
        {ok, #{
            target_node => TargetNode,
            max_connections => Max,
            active_connections => Active,
            available_connections => Available
        }}
    catch
        _:Reason -> {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end;
pool_stats(_) ->
    {error, <<"invalid_pool">>}.

%% Destroy a pool and clean up ETS table
destroy_pool({pool, _PoolId, TableName}) ->
    try
        ets:delete(TableName),
        ok
    catch
        _:_ -> {error, <<"destroy_failed">>}
    end;
destroy_pool(_) ->
    {error, <<"invalid_pool">>}.

%% Run a concurrent stress test: spawn Workers processes each performing OpsPerWorker
stress_test_pool(Pool = {pool, _PoolId, _TableName}, OpsPerWorker, Workers) when is_integer(OpsPerWorker), is_integer(Workers), OpsPerWorker >= 0, Workers >= 0 ->
    Parent = self(),
    spawn_workers(Workers, Parent, Pool, OpsPerWorker),
    wait_for_workers(Workers, 0),
    % Return pool stats at end
    pool_stats(Pool);
stress_test_pool(_, _, _) ->
    {error, <<"invalid_args">>}.

spawn_workers(0, _Parent, _Pool, _Ops) -> ok;
spawn_workers(N, Parent, Pool, Ops) when N > 0 ->
    spawn(fun() -> worker_loop(Pool, Ops, Parent) end),
    spawn_workers(N - 1, Parent, Pool, Ops).

worker_loop(Pool, Ops, Parent) ->
    case Ops of
        0 -> Parent ! done;
        _ ->
            % Simulate getting a connection (atomic increment) and releasing it
            case get_connection(Pool) of
                {ok, Conn} ->
                    % Do a small amount of work
                    ok,
                    release_connection(Conn),
                    worker_loop(Pool, Ops - 1, Parent);
                {error, _} ->
                    % If pool exhausted or error, continue
                    worker_loop(Pool, Ops - 1, Parent)
            end
    end.

wait_for_workers(0, _CountDone) -> ok;
wait_for_workers(N, _CountDone) ->
    receive
        done -> wait_for_workers(N - 1, _CountDone + 1)
    end.

%% Helpers
is_ok({ok, _}) -> true;
is_ok(ok) -> true;
is_ok(_) -> false.

get_error({error, Reason}) when is_binary(Reason) -> Reason;
get_error({error, Reason}) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
get_error({error, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_error(_) -> <<"unknown_error">>.
