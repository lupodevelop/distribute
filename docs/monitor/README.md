# Monitor Module

Low-level process monitoring primitives for distributed Gleam/Erlang applications. The monitor module provides direct access to Erlang's monitoring capabilities for tracking process and node liveness.

## Overview

The Monitor module provides:

- **Process monitoring** - Detect when processes terminate
- **Node monitoring** - Detect when nodes disconnect
- **Reference-based tracking** - Unique references for managing monitors
- **Low-level control** - Direct access to Erlang monitoring primitives

This module is a thin wrapper around Erlang's built-in monitoring system, providing the building blocks for higher-level monitoring abstractions.

## Quick Start

### Getting the Current Process

```gleam
import distribute/monitor

pub fn current_process() {
  let pid = monitor.self()
  io.println("My PID: " <> string.inspect(pid))
  pid
}
```

### Monitoring a Process

```gleam
import distribute/monitor.{type Pid, type Reference}

pub fn watch_process(target: Pid) {
  // Start monitoring
  let ref = monitor.monitor(target)
  
  // The current process will receive a message when target dies:
  // {'DOWN', Reference, process, Pid, Reason}
  
  ref
}

pub fn stop_watching(ref: Reference) {
  // Stop monitoring (returns true if monitor existed)
  let removed = monitor.demonitor(ref)
  
  case removed {
    True -> io.println("Monitor removed")
    False -> io.println("Monitor didn't exist")
  }
}
```

### Monitoring Node Connections

```gleam
import distribute/monitor

pub fn watch_node(node_name: String) {
  // Enable monitoring for the node
  let enabled = monitor.monitor_node(node_name, True)
  
  case enabled {
    True -> {
      io.println("Now monitoring node: " <> node_name)
      // Will receive {nodedown, node_name} when disconnected
    }
    False -> io.println("Failed to monitor node")
  }
}

pub fn stop_watching_node(node_name: String) {
  // Disable monitoring
  let _ = monitor.monitor_node(node_name, False)
  io.println("Stopped monitoring node: " <> node_name)
}
```

## API Reference

### Types

#### Pid

```gleam
pub type Pid
```

An Erlang process identifier. Represents a running process that can be monitored or sent messages.

#### Reference

```gleam
pub type Reference
```

A unique reference returned by `monitor()`. Used to identify and cancel specific monitors.

### Functions

#### self

```gleam
pub fn self() -> Pid
```

Returns the PID of the calling process.

**Returns:** The current process's PID.

#### monitor

```gleam
pub fn monitor(pid: Pid) -> Reference
```

Creates a monitor for the given process. When the monitored process terminates, the calling process receives a `DOWN` message.

**Parameters:**
- `pid` - The process to monitor

**Returns:** A unique reference identifying this monitor.

**Message format on termination:**
```erlang
{'DOWN', Reference, process, Pid, Reason}
```

#### demonitor

```gleam
pub fn demonitor(ref: Reference) -> Bool
```

Removes a previously created monitor.

**Parameters:**
- `ref` - The monitor reference returned by `monitor()`

**Returns:** `True` if the monitor existed and was removed, `False` otherwise.

#### monitor_node

```gleam
pub fn monitor_node(node: String, flag: Bool) -> Bool
```

Enables or disables node monitoring for the specified node.

**Parameters:**
- `node` - The name of the node to monitor (e.g., `"myapp@host.example.com"`)
- `flag` - `True` to enable monitoring, `False` to disable

**Returns:** `True` if the operation succeeded, `False` otherwise.

**Message format on node disconnect:**
```erlang
{nodedown, NodeName}
```

## Usage Patterns

### Pattern 1: Supervised Process Monitoring

Monitor a child process and restart it on failure:

```gleam
import distribute/monitor
import gleam/erlang/process

pub type SupervisorMsg {
  Down(reference: monitor.Reference, pid: monitor.Pid, reason: Dynamic)
}

pub fn supervise(child_pid: monitor.Pid, state: State) {
  let ref = monitor.monitor(child_pid)
  
  // Store the reference to match against DOWN messages
  let state = State(..state, monitored: dict.insert(state.monitored, ref, child_pid))
  
  // In your message handler:
  // case msg {
  //   Down(ref, pid, reason) -> {
  //     // Child died, restart it
  //     let new_pid = start_child()
  //     supervise(new_pid, state)
  //   }
  // }
  
  state
}
```

### Pattern 2: Linked Process Cleanup

Clean up resources when a linked process dies:

```gleam
import distribute/monitor

pub fn track_client(client_pid: monitor.Pid, session_id: String) {
  let ref = monitor.monitor(client_pid)
  
  // When client disconnects, we'll get a DOWN message
  // and can clean up the session
  #(ref, session_id)
}

pub fn handle_client_down(ref: monitor.Reference, sessions: Dict) {
  case dict.get(sessions, ref) {
    Ok(session_id) -> {
      cleanup_session(session_id)
      dict.delete(sessions, ref)
    }
    Error(Nil) -> sessions
  }
}
```

### Pattern 3: Cluster Health Monitoring

Monitor all nodes in a cluster:

```gleam
import distribute/monitor
import gleam/list

pub fn monitor_cluster(nodes: List(String)) {
  list.each(nodes, fn(node) {
    let _ = monitor.monitor_node(node, True)
    io.println("Monitoring node: " <> node)
  })
}

pub fn handle_nodedown(node: String, state: ClusterState) {
  io.println("Node disconnected: " <> node)
  
  // Update cluster state
  let state = ClusterState(
    ..state,
    active_nodes: list.filter(state.active_nodes, fn(n) { n != node }),
    failed_nodes: [node, ..state.failed_nodes],
  )
  
  // Trigger recovery procedures
  initiate_failover(state)
  
  state
}
```

## Best Practices

1. **Always store monitor references** - You need the reference to cancel monitors and to match incoming `DOWN` messages.

2. **Clean up monitors** - Call `demonitor()` when you no longer need to track a process to avoid unnecessary messages and memory usage.

3. **Handle DOWN messages promptly** - Process `DOWN` messages in your actor's message loop to maintain accurate state.

4. **Use unique references** - Each call to `monitor()` returns a unique reference, even for the same PID. Use this to distinguish multiple monitors.

5. **Prefer higher-level abstractions** - For most use cases, use `gleam_otp` supervisors or the distribute library's higher-level monitoring instead of raw monitors.

6. **Node names must be exact** - When using `monitor_node()`, the node name must exactly match the Erlang node name including the full domain.

7. **Check return values** - Both `demonitor()` and `monitor_node()` return booleans indicating success. Handle failure cases appropriately.

## Erlang Interoperability

The types in this module map directly to Erlang types:

| Gleam Type | Erlang Type |
|------------|-------------|
| `Pid` | `pid()` |
| `Reference` | `reference()` |

Monitor messages are standard Erlang monitor messages and can be received using Gleam's selective receive or in actor message handlers.

## Related Modules

- [Actor](../actor/README.md) - Higher-level actor model with built-in monitoring
- [Discovery](../discovery/README.md) - Cluster-wide peer discovery
- [Cluster](../cluster/README.md) - Cluster management with automatic monitoring
