# Groups Module

Process groups for distributed Gleam/Erlang applications. The groups module provides a way to organize processes into named groups and broadcast messages to all members.

## Overview

The Groups module provides:

- **Named process groups** - Organize processes by logical grouping
- **Dynamic membership** - Processes can join and leave groups at runtime
- **Broadcasting** - Send messages to all members of a group
- **Type-safe variants** - Typed versions for compile-time safety with Gleam subjects

## Quick Start

### Basic Group Operations

```gleam
import distribute/groups
import gleam/erlang/process.{type Pid}

pub fn join_cluster_group(pid: Pid) {
  case groups.join("cluster_workers", pid) {
    Ok(Nil) -> io.println("Joined cluster_workers group")
    Error(groups.GroupFailed(reason)) -> io.println("Failed: " <> reason)
    Error(groups.EncodeFailed(_)) -> io.println("Encoding error")
    Error(groups.MemberConversionFailed(_)) -> io.println("Conversion error")
  }
}

pub fn leave_cluster_group(pid: Pid) {
  case groups.leave("cluster_workers", pid) {
    Ok(Nil) -> io.println("Left cluster_workers group")
    Error(_) -> io.println("Failed to leave group")
  }
}
```

### Querying Group Members

```gleam
import distribute/groups

pub fn list_workers() {
  let members = groups.members("cluster_workers")
  
  io.println("Current workers: " <> int.to_string(list.length(members)))
  
  list.each(members, fn(pid) {
    io.println("Worker PID: " <> string.inspect(pid))
  })
}
```

### Type-Safe Groups with Subjects

```gleam
import distribute/groups
import gleam/erlang/process.{type Subject}

pub type WorkerMessage {
  DoWork(task: String)
  Shutdown
}

pub fn join_typed_group(subject: Subject(WorkerMessage)) {
  case groups.join_typed("typed_workers", subject) {
    Ok(Nil) -> io.println("Joined typed workers group")
    Error(_) -> io.println("Failed to join")
  }
}

pub fn broadcast_to_workers(task: String) {
  let encoder = fn(msg: WorkerMessage) {
    // Your message encoder
    encode_worker_message(msg)
  }
  
  case groups.broadcast_typed("typed_workers", DoWork(task), encoder) {
    Ok(Nil) -> io.println("Broadcast sent to all workers")
    Error(_) -> io.println("Broadcast failed")
  }
}
```

## API Reference

### Types

#### GroupError

```gleam
pub type GroupError {
  GroupFailed(String)
  EncodeFailed(codec.EncodeError)
  MemberConversionFailed(String)
}
```

Error types returned by group operations:

- `GroupFailed(reason)` - The group operation failed at the system level (reason contains details)
- `EncodeFailed(error)` - Message encoding failed during broadcast (wraps codec error)
- `MemberConversionFailed(reason)` - Failed to convert member PID/Subject

### Functions

#### join

```gleam
pub fn join(group: String, pid: Pid) -> Result(Nil, GroupError)
```

Adds a process to a named group. If the group doesn't exist, it will be created automatically.

**Parameters:**
- `group` - The name of the group to join
- `pid` - The process ID to add to the group

**Returns:** `Ok(Nil)` on success, or a `GroupError` on failure.

#### leave

```gleam
pub fn leave(group: String, pid: Pid) -> Result(Nil, GroupError)
```

Removes a process from a named group. If the process is not a member, this is a no-op.

**Parameters:**
- `group` - The name of the group to leave
- `pid` - The process ID to remove from the group

**Returns:** `Ok(Nil)` on success, or a `GroupError` on failure.

#### members

```gleam
pub fn members(group: String) -> List(Pid)
```

Returns all current members of a group. Returns an empty list if the group doesn't exist.

**Parameters:**
- `group` - The name of the group to query

**Returns:** A list of process IDs that are members of the group.

#### broadcast (DEPRECATED)

```gleam
@deprecated("Use broadcast_typed for type-safe broadcasting")
pub fn broadcast(group: String, msg: a) -> Result(Nil, GroupError)
```

⚠️ **DEPRECATED**: Use `broadcast_typed` instead for type-safe message broadcasting.

Sends a message to all members of a group.

#### join_typed

```gleam
pub fn join_typed(group: String, subject: Subject(msg)) -> Result(Nil, GroupError)
```

Type-safe version of `join` that works with Gleam subjects instead of raw PIDs.

**Parameters:**
- `group` - The name of the group to join
- `subject` - The typed subject to add to the group

**Returns:** `Ok(Nil)` on success, or a `GroupError` on failure.

#### leave_typed

```gleam
pub fn leave_typed(group: String, subject: Subject(msg)) -> Result(Nil, GroupError)
```

Type-safe version of `leave` that works with Gleam subjects.

**Parameters:**
- `group` - The name of the group to leave
- `subject` - The typed subject to remove from the group

**Returns:** `Ok(Nil)` on success, or a `GroupError` on failure.

#### members_typed

```gleam
pub fn members_typed(group: String) -> List(Pid)
```

Returns all current members of a typed group as PIDs.

**Parameters:**
- `group` - The name of the group to query

**Returns:** A list of process IDs (note: returns PIDs, not subjects).

#### broadcast_typed

```gleam
pub fn broadcast_typed(
  group: String,
  msg: msg,
  encoder: fn(msg) -> BitArray,
) -> Result(Nil, GroupError)
```

Type-safe broadcast that sends an encoded message to all group members.

**Parameters:**
- `group` - The name of the group to broadcast to
- `msg` - The message to send
- `encoder` - A function that encodes the message to BitArray

**Returns:** `Ok(Nil)` on success, or a `GroupError` on failure.

## Usage Patterns

### Pattern 1: Worker Pool

Create a pool of workers that can receive tasks:

```gleam
import distribute/groups
import gleam/otp/actor

pub type Task {
  Process(data: String)
  Stop
}

pub fn spawn_worker() {
  let assert Ok(subject) = actor.start(Nil, handle_task)
  
  // Join the worker pool
  let _ = groups.join_typed("worker_pool", subject)
  
  subject
}

pub fn distribute_task(data: String) {
  let workers = groups.members("worker_pool")
  
  case workers {
    [] -> Error("No workers available")
    [worker, ..] -> {
      // Send to first available worker (simple strategy)
      process.send(worker, Process(data))
      Ok(Nil)
    }
  }
}
```

### Pattern 2: Pub/Sub Pattern

Implement publish-subscribe messaging:

```gleam
import distribute/groups

pub type Event {
  UserCreated(id: String)
  UserUpdated(id: String)
  UserDeleted(id: String)
}

// Subscribe to events
pub fn subscribe_to_user_events(handler: Subject(Event)) {
  groups.join_typed("user_events", handler)
}

// Publish events
pub fn publish_event(event: Event) {
  groups.broadcast_typed("user_events", event, encode_event)
}

fn encode_event(event: Event) -> BitArray {
  // Your encoding logic
  <<>>
}
```

### Pattern 3: Service Discovery with Groups

Use groups to track available services:

```gleam
import distribute/groups

pub fn register_service(service_name: String, pid: Pid) {
  let group_name = "service:" <> service_name
  groups.join(group_name, pid)
}

pub fn find_service(service_name: String) -> Result(Pid, Nil) {
  let group_name = "service:" <> service_name
  
  case groups.members(group_name) {
    [] -> Error(Nil)
    [pid, ..] -> Ok(pid)
  }
}

pub fn find_all_services(service_name: String) -> List(Pid) {
  let group_name = "service:" <> service_name
  groups.members(group_name)
}
```

## Best Practices

1. **Use typed variants** - Prefer `join_typed`, `leave_typed`, and `broadcast_typed` for compile-time type safety and better error messages.

2. **Clean up on exit** - Always call `leave` or `leave_typed` when a process terminates to keep group membership accurate.

3. **Use descriptive group names** - Name groups clearly to indicate their purpose, e.g., `"worker_pool:image_processing"` instead of `"workers"`.

4. **Handle empty groups** - Always check if `members()` returns an empty list before attempting operations.

5. **Avoid broadcast for large groups** - For groups with many members, consider a more targeted message distribution strategy.

6. **Don't rely on message ordering** - Broadcasts don't guarantee delivery order across members.

7. **Use namespaced groups** - Prefix group names to avoid collisions, e.g., `"myapp:notifications"`.

## Related Modules

- [Registry](../registry/README.md) - Named process registration
- [Discovery](../discovery/README.md) - Peer discovery across nodes
- [Messaging](../messaging/README.md) - Cross-node message passing
