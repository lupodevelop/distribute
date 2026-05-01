-module(cluster_monitor_test_ffi).
-export([send_nodedown_event/2, send_nodeup_event/2, send_garbage_term/1]).

%% Send a raw {nodedown, Node} message to a Pid, exactly as net_kernel would.
%% This lets us test the cluster_monitor actor without a real distributed node.
send_nodedown_event(Pid, NodeName) when is_binary(NodeName) ->
    NodeAtom = binary_to_atom(NodeName, utf8),
    Pid ! {nodedown, NodeAtom},
    nil.

%% Send a raw {nodeup, Node} message to a Pid.
send_nodeup_event(Pid, NodeName) when is_binary(NodeName) ->
    NodeAtom = binary_to_atom(NodeName, utf8),
    Pid ! {nodeup, NodeAtom},
    nil.

%% Send a term that is neither a Subject message nor a node event.
%% Used to exercise the unknown-message diagnostic hook.
send_garbage_term(Pid) ->
    Pid ! {totally_unrecognised, weird, payload, 42},
    nil.
