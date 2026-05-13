-module(cluster_ffi).
-export([start_node/2, connect/1, nodes/0, self_node/0, ping/1,
         monitor_nodes/1, decode_node_event/1, is_alive/0]).

-import(distribute_ffi_utils, [to_node_atom_safe/1, to_cookie_atom_safe/1]).

%% Start a distributed BEAM node.
%%
%% Returns typed tagged tuples that map directly to Gleam StartError variants:
%%   {ok, nil}                              -> Ok(Nil)
%%   {error, already_started}               -> Error(AlreadyStarted)
%%   {error, {invalid_node_name, R}}        -> Error(InvalidNodeName(R))
%%   {error, {invalid_cookie_format, R}}    -> Error(InvalidCookieFormat(R))
%%   {error, {network_error, R}}            -> Error(NetworkError(R))
%%   {error, {start_failed, R}}             -> Error(StartFailed(R))
%%
%% Atom creation for both name and cookie happens INSIDE the try block:
%% any validation failure surfaces as a typed error rather than an
%% uncaught Erlang exception.
%%
%% Atom-budget exhaustion is observable via telemetry. The FFI emits
%% `AtomBudgetExhausted(<offending input>, AtomBudgetOnStartNode)`
%% before returning the typed error, so observers see *which* input
%% (name or cookie) ran the budget out. The Gleam side does not
%% re-emit; doing so would lose the input attribution because the
%% public StartError is a unit constructor.
start_node(Name, Cookie) ->
    try
        case to_node_atom_safe(Name) of
            {error, invalid_node_name} ->
                {error, {invalid_node_name, <<"name failed format validation">>}};
            {error, atom_budget_exceeded} ->
                telemetry_ffi:emit(
                    {atom_budget_exhausted, Name, atom_budget_on_start_node}
                ),
                {error, start_atom_budget_exceeded};
            {ok, NameAtom} ->
                case to_cookie_atom_safe(Cookie) of
                    {error, invalid_cookie} ->
                        {error, {invalid_cookie_format, <<"cookie failed format validation">>}};
                    {error, atom_budget_exceeded} ->
                        telemetry_ffi:emit(
                            {atom_budget_exhausted, Cookie, atom_budget_on_start_node}
                        ),
                        {error, start_atom_budget_exceeded};
                    {ok, CookieAtom} ->
                        Type = node_type_for(NameAtom),
                        case net_kernel:start([NameAtom, Type]) of
                            {ok, _} ->
                                erlang:set_cookie(node(), CookieAtom),
                                {ok, nil};
                            {error, {already_started, _}} ->
                                {error, already_started};
                            {error, Reason} ->
                                R = iolist_to_binary(io_lib:format("~p", [Reason])),
                                case is_network_reason(Reason) of
                                    true  -> {error, {network_error, R}};
                                    false -> {error, {start_failed, R}}
                                end
                        end
                end
        end
    catch
        error:CaughtReason:CaughtStack ->
            R2 = iolist_to_binary(io_lib:format("~p:~p", [CaughtReason, CaughtStack])),
            {error, {start_failed, R2}}
    end.

node_type_for(NameAtom) ->
    case string:split(atom_to_list(NameAtom), "@") of
        [_, Host] ->
            case lists:member($., Host) of
                true  -> longnames;
                false -> shortnames
            end;
        _ -> shortnames
    end.

%% Connect to a remote node.
%%
%%   {ok, nil}                          -> Ok(Nil)
%%   {error, connect_failed}            -> Error(ConnectFailed)
%%   {error, connect_ignored}           -> Error(ConnectIgnored)
%%   {error, {invalid_node_format, R}}  -> Error(InvalidNodeFormat(R))
%%
%% Uses the safe-create helper so first-contact with a never-seen-before
%% node atom does not silently fail with `connect_failed`.
connect(Node) ->
    case to_node_atom_safe(Node) of
        {ok, A} ->
            case net_kernel:connect_node(A) of
                true    -> {ok, nil};
                false   -> {error, connect_failed};
                ignored -> {error, connect_ignored}
            end;
        {error, invalid_node_name} ->
            {error, {invalid_node_format, <<"name failed format validation">>}};
        {error, atom_budget_exceeded} ->
            {error, connect_atom_budget_exceeded}
    end.

nodes() ->
    [atom_to_binary(N, utf8) || N <- erlang:nodes()].

self_node() ->
    atom_to_binary(node(), utf8).

%% Whether the local node is running BEAM distribution. Returns true iff
%% net_kernel has been started (`erlang:is_alive/0`). This is authoritative
%% and does not rely on the textual form of the node name.
is_alive() ->
    erlang:is_alive().

monitor_nodes(Flag) ->
    net_kernel:monitor_nodes(Flag).

decode_node_event({NodeUp, Node}) when NodeUp =:= nodeup; NodeUp =:= nodedown ->
    {ok, {atom_to_binary(NodeUp, utf8), atom_to_binary(Node, utf8)}};
decode_node_event(_) ->
    {error, nil}.

%% Ping a remote node. Returns `false` for invalid names AND for
%% unreachable peers. Both cases are "cannot reach" semantically.
%%
%% Atom-budget exhaustion is observable via the telemetry sink even
%% though the public Gleam return type is `Bool`. We emit
%% `AtomBudgetExhausted(Node, AtomBudgetOnPing)` from the FFI before
%% returning `false`, so downstream observability sees the budget
%% refusal that the function itself cannot surface as a typed error.
ping(Node) ->
    case to_node_atom_safe(Node) of
        {ok, A} ->
            case net_adm:ping(A) of
                pong -> true;
                _    -> false
            end;
        {error, atom_budget_exceeded} ->
            telemetry_ffi:emit(
                {atom_budget_exhausted, Node, atom_budget_on_ping}
            ),
            false;
        {error, _} ->
            false
    end.

%% ---------------------------------------------------------------------------
%% Private helpers
%% ---------------------------------------------------------------------------

is_network_reason(network) -> true;
is_network_reason(eaddrinuse) -> true;
is_network_reason(econnrefused) -> true;
is_network_reason({error, Reason}) -> is_network_reason(Reason);
is_network_reason({shutdown, Reason}) -> is_network_reason(Reason);
is_network_reason({failed_to_start_child, _Child, Reason}) ->
    is_network_reason(Reason);
is_network_reason({'EXIT', Reason}) -> is_network_reason(Reason);
is_network_reason(Tuple) when is_tuple(Tuple) ->
    lists:any(fun is_network_reason/1, tuple_to_list(Tuple));
is_network_reason(List) when is_list(List) ->
    lists:any(fun is_network_reason/1, List);
is_network_reason(_) -> false.
