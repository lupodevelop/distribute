%% Tiny pure helpers used by `distribute/conflict` built-in resolvers.
%% Kept separate from `conflict_ffi.erl` (which owns the `:global`
%% registration boundary) so the resolver helper code stays free of
%% scheduling-sensitive logic and is trivially unit-testable.
-module(conflict_ffi_helpers).
-export([pid_node_is_local/1, pid_node_name/1]).

%% True when the Pid is owned by the local node (`node(Pid) =:= node()`).
pid_node_is_local(Pid) when is_pid(Pid) ->
    erlang:node(Pid) =:= erlang:node();
pid_node_is_local(_) ->
    false.

%% Node name as a binary, suitable for direct comparison with the
%% `String` values supplied to `node_priority/1`.
pid_node_name(Pid) when is_pid(Pid) ->
    atom_to_binary(erlang:node(Pid), utf8);
pid_node_name(_) ->
    <<"">>.
