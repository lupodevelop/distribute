# Cluster Module

## Overview

The cluster module provides a type-safe wrapper around BEAM's distributed node features. It handles starting distributed nodes, connecting to peers, and monitoring cluster membership.

### Key Features

- ✅ Start distributed BEAM nodes with validation
- ✅ Connect to remote nodes with structured errors  
- ✅ Query connected nodes
- ✅ Ping remote nodes for health checks
- ✅ Type-safe error handling (`StartError`, `ConnectError`)

---

## Quick Start

### Starting a Distributed Node

```gleam
import distribute/cluster

pub fn main() {
  // Start this node as a distributed node
  case cluster.start_node("myapp@127.0.0.1", "secret_cookie") {
    Ok(Nil) -> {
      io.println("Node started successfully!")
      io.println("Self: " <> cluster.self_node())
    }
    Error(cluster.AlreadyStarted) -> 
      io.println("Node was already started")
    Error(cluster.InvalidNodeName(reason)) -> 
      io.println("Invalid name: " <> reason)
    Error(err) -> 
      io.println("Failed to start: " <> string.inspect(err))
  }
}
```

### Connecting to Another Node

```gleam
import distribute/cluster

pub fn join_cluster(peer: String) -> Result(Nil, String) {
  case cluster.connect(peer) {
    Ok(Nil) -> {
      io.println("Connected to " <> peer)
      Ok(Nil)
    }
    Error(cluster.ConnectTimeout) -> 
      Error("Connection timed out")
    Error(cluster.NodeNotFound) -> 
      Error("Node not found or unreachable")
    Error(cluster.ConnectIgnored) -> 
      // Already connected or tried to connect to self
      Ok(Nil)
    Error(cluster.ConnectNetworkError(reason)) ->
      Error("Network error: " <> reason)
  }
}
```

### Listing Connected Nodes

```gleam
import distribute/cluster
import gleam/io
import gleam/list

pub fn show_cluster_status() {
  let connected = cluster.nodes()
  let self = cluster.self_node()
  
  io.println("This node: " <> self)
  io.println("Connected nodes: " <> string.inspect(connected))
  io.println("Total cluster size: " <> int.to_string(list.length(connected) + 1))
}
```

### Health Checking with Ping

```gleam
import distribute/cluster

pub fn check_node_health(node: String) -> Bool {
  case cluster.ping(node) {
    True -> {
      io.println(node <> " is reachable")
      True
    }
    False -> {
      io.println(node <> " is NOT reachable")
      False
    }
  }
}
```

---

## API Reference

### Types

#### StartError

Errors that can occur when starting a distributed node:

```gleam
pub type StartError {
  /// Node name is invalid (must contain '@' and be a valid atom)
  InvalidNodeName(String)
  /// Node is already started as a distributed node
  AlreadyStarted
  /// Cookie is too long (max 255 characters)
  CookieTooLong
  /// Network-related error (e.g., port binding failure)
  NetworkError(String)
  /// Permission denied or other system error
  SystemError(String)
  /// Generic startup failure with detailed reason
  StartFailed(String)
}
```

#### ConnectError

Errors that can occur when connecting to a node:

```gleam
pub type ConnectError {
  /// Connection timed out
  ConnectTimeout
  /// Node not found or unreachable
  NodeNotFound
  /// Network connectivity issue
  ConnectNetworkError(String)
  /// Connection was ignored (already connected or self)
  ConnectIgnored
}
```

### Functions

#### start_node

```gleam
pub fn start_node(name: String, cookie: String) -> Result(Nil, StartError)
```

Start the current VM as a distributed BEAM node.

**Parameters:**
- `name`: Node name in format `"name@host"` (e.g., `"myapp@127.0.0.1"`)
- `cookie`: Shared secret for cluster authentication (max 255 chars)

**Example:**
```gleam
cluster.start_node("worker1@192.168.1.10", "my_secret_cookie")
```

---

#### connect

```gleam
pub fn connect(node: String) -> Result(Nil, ConnectError)
```

Connect to a remote distributed node.

**Parameters:**
- `node`: Remote node name in format `"name@host"`

**Example:**
```gleam
cluster.connect("master@192.168.1.1")
```

---

#### nodes

```gleam
pub fn nodes() -> List(String)
```

Get the list of all currently connected node names.

**Example:**
```gleam
let peers = cluster.nodes()
// ["node2@host", "node3@host"]
```

---

#### self_node

```gleam
pub fn self_node() -> String
```

Get the name of the current node.

**Example:**
```gleam
let me = cluster.self_node()
// "myapp@127.0.0.1"
```

---

#### ping

```gleam
pub fn ping(node: String) -> Bool
```

Ping a remote node to check connectivity.

**Returns:** `True` if the node responds, `False` otherwise.

**Example:**
```gleam
case cluster.ping("peer@host") {
  True -> io.println("Node is alive")
  False -> io.println("Node is down")
}
```

---

## Patterns

### Pattern 1: Building a Simple Cluster

```gleam
import distribute/cluster
import gleam/list
import gleam/result

/// Start a node and connect to seed nodes
pub fn bootstrap_cluster(
  my_name: String,
  cookie: String,
  seed_nodes: List(String),
) -> Result(Nil, String) {
  // Start our node
  use _ <- result.try(
    cluster.start_node(my_name, cookie)
    |> result.map_error(fn(_) { "Failed to start node" })
  )
  
  // Connect to all seed nodes (ignore failures)
  list.each(seed_nodes, fn(seed) {
    case cluster.connect(seed) {
      Ok(_) -> io.println("Connected to " <> seed)
      Error(_) -> io.println("Could not reach " <> seed)
    }
  })
  
  io.println("Cluster bootstrapped with " 
    <> int.to_string(list.length(cluster.nodes())) 
    <> " peers")
  
  Ok(Nil)
}
```

### Pattern 2: Retry Connection with Backoff

```gleam
import distribute/cluster
import distribute/retry
import gleam/erlang/process

pub fn connect_with_retry(node: String) -> Result(Nil, cluster.ConnectError) {
  let policy = retry.default_with_jitter()
    |> retry.with_max_attempts(5)
  
  do_connect_retry(node, policy, 1)
}

fn do_connect_retry(
  node: String,
  policy: retry.RetryPolicy,
  attempt: Int,
) -> Result(Nil, cluster.ConnectError) {
  case cluster.connect(node) {
    Ok(_) -> Ok(Nil)
    Error(err) -> {
      case retry.should_retry(policy, attempt) {
        True -> {
          process.sleep(retry.delay_ms(policy, attempt))
          do_connect_retry(node, policy, attempt + 1)
        }
        False -> Error(err)
      }
    }
  }
}
```

### Pattern 3: Node Watcher

```gleam
import distribute/cluster
import gleam/erlang/process
import gleam/list
import gleam/set.{type Set}

/// Periodically check for node changes
pub fn start_node_watcher(callback: fn(String, Bool) -> Nil) {
  let initial_nodes = set.from_list(cluster.nodes())
  node_watcher_loop(initial_nodes, callback)
}

fn node_watcher_loop(
  known: Set(String),
  callback: fn(String, Bool) -> Nil,
) {
  process.sleep(1000)  // Check every second
  
  let current = set.from_list(cluster.nodes())
  
  // Find new nodes
  set.difference(current, known)
  |> set.to_list()
  |> list.each(fn(node) { callback(node, True) })
  
  // Find departed nodes
  set.difference(known, current)
  |> set.to_list()
  |> list.each(fn(node) { callback(node, False) })
  
  node_watcher_loop(current, callback)
}
```

---

## Best Practices

### 1. Always Validate Node Names

Node names must follow the format `name@host`. The module validates this automatically, but you should handle `InvalidNodeName` errors gracefully.

### 2. Use Consistent Cookies

All nodes in a cluster must use the same cookie. Store this securely and inject via environment variables:

```gleam
import gleam/erlang
import distribute/cluster

let cookie = erlang.get_env("CLUSTER_COOKIE") |> result.unwrap("default_cookie")
cluster.start_node("myapp@host", cookie)
```

### 3. Handle `ConnectIgnored`

`ConnectIgnored` means either:
- You're already connected to the node
- You tried to connect to yourself

This is usually not an error:

```gleam
case cluster.connect(node) {
  Ok(_) | Error(cluster.ConnectIgnored) -> Ok(Nil)
  Error(e) -> Error(e)
}
```

### 4. Use Ping for Health Checks

Before assuming a node is reachable, use `ping`:

```gleam
case cluster.ping(node) && cluster.connect(node) {
  True -> // Connected
  False -> // Node unreachable
}
```

---

## Related Modules

- [monitor](../monitor/README.md) - Process and node monitoring
- [discovery](../discovery/README.md) - Automatic peer discovery
- [transport](../transport/README.md) - Message transport layer
