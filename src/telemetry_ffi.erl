%% Telemetry sink storage. Single ETS slot, lock-free read on the hot
%% path, last-wins replacement on `install`. Default = absent slot ->
%% `emit/1` is a constant-time no-op.
%%
%% ETS over `persistent_term`: a `persistent_term:put/2` triggers a
%% global GC pass across every BEAM process. `install` is allowed to
%% be called more than once (tests swap sinks between cases) so we
%% cannot afford a global GC per install. ETS gives us O(1) read with
%% `read_concurrency`, lock-free on hot SMP, and `insert` without GC.
-module(telemetry_ffi).
-export([install/1, reset/0, emit/1]).

-define(TABLE, distribute_telemetry_sink_table).
-define(KEY, sink).

%% Lazy-init the ETS table. Race-safe: concurrent first callers
%% either win the `ets:new` or catch the `badarg` and discover the
%% existing table.
ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined ->
            try
                ets:new(?TABLE, [
                    named_table, public, set, {read_concurrency, true}
                ])
            catch
                error:badarg -> ?TABLE
            end;
        _ ->
            ?TABLE
    end.

install(Sink) when is_function(Sink, 1) ->
    ensure_table(),
    ets:insert(?TABLE, {?KEY, Sink}),
    nil.

reset() ->
    case ets:whereis(?TABLE) of
        undefined -> nil;
        _ ->
            ets:delete(?TABLE, ?KEY),
            nil
    end.

%% Hot path: ETS lookup -> sink invocation. The sink is run inline
%% in the caller's process so a slow sink directly slows the emit
%% point. Library callers that want async fan-out should make the
%% sink itself send to a background process.
%%
%% Sink panics are caught and logged via `logger:warning/2` rather
%% than propagated. Rationale: `emit` is called from inside actor
%% mailbox handlers, the call selector, and the `call_isolated`
%% proxy. A buggy sink that raises would otherwise kill those
%% library-internal processes, taking down operationally critical
%% paths (registered actors, in-flight RPCs) for what is by
%% definition an observability bug. Catching keeps the library
%% resilient; logging keeps the bug visible to operators. We log
%% the event tag (not the full event payload) to avoid leaking
%% potentially sensitive `attempted_input` strings into shared logs.
emit(Event) ->
    case ets:whereis(?TABLE) of
        undefined ->
            nil;
        _ ->
            case ets:lookup(?TABLE, ?KEY) of
                [{?KEY, Sink}] ->
                    safe_invoke(Sink, Event),
                    nil;
                [] ->
                    nil
            end
    end.

safe_invoke(Sink, Event) ->
    try
        Sink(Event)
    catch
        Class:Reason:Stack ->
            EventTag = event_tag(Event),
            logger:warning(
                "distribute: telemetry sink raised on event ~p: ~p:~p~n~p",
                [EventTag, Class, Reason, Stack]
            ),
            nil
    end.

%% Project an event onto its tag (the leading atom in the tuple form
%% Gleam constructors compile to). For unit constructors the value
%% is itself an atom, so we return it as-is.
event_tag(Event) when is_tuple(Event), tuple_size(Event) >= 1 ->
    element(1, Event);
event_tag(Event) when is_atom(Event) ->
    Event;
event_tag(_) ->
    unknown_event_shape.
