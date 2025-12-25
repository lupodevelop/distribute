# distribute

<p align="center">
  <img src="assets/img/distribute.png" alt="distribute logo" width="256" height="256" />
</p>

[![Package Version](https://img.shields.io/hexpm/v/distribute)](https://hex.pm/packages/distribute)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/distribute/)

**Distribute** brings the full power of Erlang's distributed computing to Gleam, with a type-safe and idiomatic API.

While Gleam runs on the BEAM, accessing distributed primitives (like connecting nodes, global registration, or RPC) often requires writing raw Erlang FFI or dealing with untyped atoms. **Distribute** solves this by providing a robust, type-safe layer over these primitives, making it easy to build distributed systems without leaving Gleam.

## Features

- **Node Management** — Start distributed nodes, connect to peers, and list connected nodes with proper error handling.
- **Global Registry** — Register and lookup processes across the cluster using a type-safe wrapper around `:global`.
- **Cross-node Messaging** — Send messages to remote processes or globally registered names transparently.
- **Process Groups** — Join/leave groups and broadcast messages to all members (wrapper around `:pg`).
- **Remote Monitoring** — Monitor processes and nodes for failure detection across the network.
- **RPC** — Perform Remote Procedure Calls to any Erlang/Gleam module with timeout control.
- **SWIM-like Membership** — A built-in background service for failure detection, gossip, and cluster membership.
- **Raft-lite Election** — A lightweight leader election implementation for simple coordination needs.

## Installation

```sh
gleam add distribute
```

## Documentation

- **Main Documentation**: https://hexdocs.pm/distribute/
- **Examples**:
  - [General Examples](https://hexdocs.pm/distribute/examples.html)
  - [Two Nodes Setup](https://hexdocs.pm/distribute/examples-two-nodes.html)
  - [Two Nodes App](https://hexdocs.pm/distribute/examples-two-nodes-app.html)

## Quick Start

### 1. Start a Distributed Node

```gleam
import distribute/cluster
import gleam/io
import gleam/string

pub fn main() {
  // Start this node as "app@127.0.0.1" with cookie "secret"
  case cluster.start_node("app@127.0.0.1", "secret") {
    Ok(Nil) -> io.println("Node started successfully!")
    Error(err) -> io.println("Failed to start: " <> string.inspect(err))
  }
}
```

### 2. Connect to Another Node

```gleam
import distribute/cluster
import gleam/io
import gleam/string

pub fn connect_peer() {
  case cluster.connect("other@127.0.0.1") {
    Ok(_) -> io.println("Connected to peer!")
    Error(err) -> io.println("Connection failed: " <> string.inspect(err))
  }
}
```

### 3. Register and Send Messages Globally

```gleam
import distribute/registry
import distribute/messaging
import distribute/monitor

pub fn register_and_send() {
  // Register current process globally
  let pid = monitor.self()
  let _ = registry.register("my_service", pid)

  // Send a message to a globally registered name
  let _ = messaging.send_global("my_service", "Hello, world!")
}
```

### 4. Process Groups

```gleam
import distribute/groups
import distribute/monitor

pub fn use_groups() {
  let pid = monitor.self()

  // Join a group named "workers"
  let _ = groups.join("workers", pid)

  // Broadcast a message to all members of "workers"
  let _ = groups.broadcast("workers", #("task", 42))

  // Get a list of all members in the group
  let members = groups.members("workers")
}
```

### 5. SWIM Membership Service

```gleam
import distribute/cluster/membership

pub fn use_membership() {
  // Start the background membership service (probe every 500ms)
  membership.start_service(500)

  // Get a list of alive nodes
  let alive = membership.alive()

  // Get nodes with full status details
  let nodes = membership.members_with_status()
  // Returns: [#("node@host", Alive, 0), ...]

  // Get current leader (lexicographically largest alive node)
  let leader = membership.current_leader()

  // Stop the service when done
  membership.stop_service()
}
```

### 6. Leader Election (Raft-lite)

```gleam
import distribute/election/raft_lite
import gleam/io

pub fn elect_leader() {
  case raft_lite.elect() {
    raft_lite.Leader(name) -> io.println("Current Leader: " <> name)
    raft_lite.NoLeader -> io.println("No leader available yet")
  }
}
```

### 7. Remote Procedure Calls

```gleam
import distribute/remote_call
import gleam/io
import gleam/string

pub fn call_remote() {
  // Call erlang:node() on a remote node
  case remote_call.call("other@host", "erlang", "node", [], []) {
    Ok(result) -> io.println("Remote node name: " <> string.inspect(result))
    Error(remote_call.RpcBadRpc(reason)) -> io.println("RPC failed: " <> reason)
    Error(_) -> io.println("RPC error")
  }
}
```

## Module Reference

| Module | Description |
|--------|-------------|
| `distribute/cluster` | Node management: start, connect, ping, list nodes |
| `distribute/registry` | Global process registration (uses `:global`) |
| `distribute/messaging` | Send messages to PIDs or global names |
| `distribute/groups` | Process groups with join/leave/broadcast (uses `:pg`) |
| `distribute/monitor` | Monitor processes and nodes |
| `distribute/remote_call` | RPC to remote nodes |
| `distribute/cluster/membership` | SWIM-like membership with gossip and failure detection |
| `distribute/cluster/gossip` | Gossip protocol for membership state propagation |
| `distribute/cluster/health` | Health checks for nodes and cluster |
| `distribute/election/raft_lite` | Lightweight leader election with term-based voting |

> **Note**: `cluster.connect_bool` is kept for backward compatibility but is deprecated. Please prefer the `Result`-returning `cluster.connect` which provides structured error handling.

## Running Tests

```sh
gleam test
```

## Integration Tests

You can run a multi-node SWIM integration test using the provided script:

```sh
./examples/two_nodes/swim_integration.sh
```

This script starts 3 local Erlang nodes, runs the membership service, and verifies gossip convergence.

> **Note**: Scripts under `examples/` are **manual integration demos**. They rely on local node naming (`-sname` + `hostname`) and are not intended to be a CI hard requirement.

## Architecture

```
src/
├── distribute.gleam        # Top-level module
├── distribute/
│   ├── cluster.gleam       # Node management
│   ├── registry.gleam      # Global registry
│   ├── messaging.gleam     # Cross-node messaging
│   ├── groups.gleam        # Process groups
│   ├── monitor.gleam       # Process/node monitoring
│   ├── remote_call.gleam   # RPC
│   ├── cluster/
│   │   ├── membership.gleam # SWIM-like membership
│   │   ├── gossip.gleam     # Gossip protocol
│   │   └── health.gleam     # Health checks
│   └── election/
│       └── raft_lite.gleam  # Leader election
└── *_ffi.erl               # Erlang FFI files (in src root)
```

## Design Philosophy

1. **No magic** — Everything is explicit.
2. **Zero custom runtime** — Only wraps standard BEAM features.
3. **Small but complete APIs** — Just what you need for clustering.
4. **Type-safe** — Gleam's type system prevents common errors.
5. **Compatible with gleam/otp** — Works alongside standard OTP patterns.

## Safety

- **Atom creation**: Some module APIs (FFI) accept string inputs that are converted to Erlang atoms. Creating atoms dynamically from arbitrary input can exhaust the BEAM atom table (which is not garbage-collected). Use caution when passing untrusted input to functions that convert strings to atoms. Where possible, use existing atoms or map inputs to a constrained set of known atoms.

  The library makes a best-effort attempt to avoid creating new atoms where possible (attempting `binary_to_existing_atom` first), but code paths that require atoms (e.g., `global:register_name`, `pg:join`) may still create atoms in some cases. Prefer using stable, pre-defined names or registering via a controlled mapping to avoid growing the atom table.

## Settings

This library exposes a small `settings` API which controls behaviour that impacts safety and logging.

- `settings.set_allow_atom_creation(allow: Bool)` — default: `False`.
  - If `False`, FFI calls will attempt `binary_to_existing_atom` and *will not* create a new atom if it doesn't exist, returning an error instead. This prevents uncontrolled atom creation from untrusted inputs.
  - If `True`, the library may create atoms when necessary for compatibility (e.g., RPC and monitor fallback).

- `settings.set_use_crypto_ids(use_crypto: Bool)` — default: `False`.
  - If `True`, `log.generate_correlation_id()` will prefer crypto-derived IDs for better randomness/uniqueness. If `False`, a monotonic/time-based fallback is used.

Example:

```gleam
import settings

pub fn main() {
  // Secure default — don't allow uncontrolled atom creation
  settings.set_allow_atom_creation(False)

  // Prdistribute/settings

pub fn main() {
  // Secure default — don't allow uncontrolled atom creation
  settings.set_allow_atom_creation(False)

  // Prefer crypto-based correlation ids
  settings.set_use_crypto_ids(True)
}
```

## Logging & Correlation IDs

This library provides a structured logging helper `log` with metadata and correlation ids.

- Set logging backend:
  - `log.set_backend("console")` (default)
  - `log.set_backend("erlang_logger")`

- Generate correlation ids:
  - `let id = log.generate_correlation_id()`
  - Use `log.info_with_correlation`, `log.error_with_correlation`, etc. to attach `id` to logs and `metadata`.

Example:

```gleam
import distribute/log
import distribute/settings

pub fn main() {
  settings.set_use_crypto_ids(True)
  log.set_backend("erlang_logger")
  let id = log.generate_correlation_id()
  log.info_with_correlation("Starting operation", [#("user","alice")], id)
}
```

## RPC Timeouts & Error Handling

Remote calls use a default timeout of 5000ms. Use `remote_call.call_with_timeout` to customise the timeout for long-running calls.

Example with a custom timeout:

```gleam
import gleam/io
import gleam/string
import distribute/
}
```

## Examples

- Multi-node shell scripts live in `examples/two_nodes/`.
- A tiny example project wrapper lives in `examples/two_nodes_app/`.

## Contributing

Contributions welcome! Please open an issue or PR on GitHub.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
