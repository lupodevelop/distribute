# distribute

<p align="center">
  <img src="assets/img/distribute.png" alt="distribute logo" width="256" height="256" />
</p>

[![Package Version](https://img.shields.io/hexpm/v/distribute)](https://hex.pm/packages/distribute)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/distribute/)

**Distribute** brings the full power of Erlang's distributed computing to Gleam, with a **type-safe** and **gleam_otp-integrated** API.

While Gleam runs on the BEAM, accessing distributed primitives (like connecting nodes, global registration, or RPC) traditionally required dealing with untyped atoms and unsafe Erlang terms. **Distribute v2.0** solves this by providing:

✅ **Type-safe messaging** using binary codecs (`Encoder(a)`, `Decoder(a)`) and `gleam/erlang/process.Subject(BitArray)`  
✅ **Full gleam_otp integration** — integrates with actors and selectors  
✅ **Explicit error handling** — all operations return typed `Result` values  
✅ **Composable codecs** — built-in support for primitives, Option, Result, tuples, and custom types  
✅ **Production-ready** — comprehensive error handling, deprecated legacy APIs  

> **Note:** Use the `_typed` variants of all functions (e.g., `send_global_typed`, `broadcast_typed`) for full type safety. Legacy untyped functions are deprecated and will be removed in v3.0.  

## Features

### Core Distributed Primitives

- **Node Management** — Start distributed nodes, connect to peers, and list connected nodes with proper error handling.
- **Global Registry** — Register and lookup processes across the cluster using a type-safe wrapper around `:global`.
- **Cross-node Messaging** — Send messages to remote processes or globally registered names transparently.
- **Process Groups** — Join/leave groups and broadcast messages to all members (wrapper around `:pg`).
- **Remote Monitoring** — Monitor processes and nodes for failure detection across the network.
- **RPC** — Perform Remote Procedure Calls to any Erlang/Gleam module with timeout control.

### Type-Safe API (v2.0)

- **Binary Codec System** — Encoder/Decoder types for compile-time safe serialization
- **Envelope Protocol** — Tag + version validation for protocol mismatch detection  
- **Typed Messaging** — `send_typed`, `call_typed`, `broadcast_typed` with explicit errors
- **Receiver Helpers** — Convenient `receive_typed` integration with gleam/erlang/process
- **gleam_otp Compatible** — Use standard `Subject(BitArray)` from gleam/erlang/process

### Advanced Features

- **SWIM-like Membership** — A built-in background service for failure detection, gossip, and cluster membership.
- **Raft-lite Election** — A lightweight leader election implementation for simple coordination needs.

## Installation

```toml
# gleam.toml
[dependencies]
gleam_stdlib = ">= 0.43.0"
gleam_erlang = ">= 0.5.0"
gleam_otp = ">= 0.1.0"
distribute = "~> 2.0"
```

```sh
gleam add distribute 
```

## Documentation

- **Main Documentation**: https://hexdocs.pm/distribute/
- **Examples**:
  - [General Examples](https://hexdocs.pm/distribute/examples.html)
  - [Two Nodes Setup](https://hexdocs.pm/distribute/examples-two-nodes.html)
  - [Two Nodes App](https://hexdocs.pm/distribute/examples-two-nodes-app.html)

## Quick Start (Type-Safe API)

### 1. Start a Distributed Node

```gleam
import distribute/node_builder
import gleam/io

pub fn main() {
  // Start this node with builder pattern
  let assert Ok(_) = node_builder.new()
    |> node_builder.with_name("app@127.0.0.1")
    |> node_builder.with_cookie("secret")
    |> node_builder.start()
  
  io.println("Node started!")
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

### 3. Register and Lookup Processes Globally (Type-Safe)

```gleam
import distribute/codec
import distribute/global
import distribute/messaging
import distribute/receiver
import distribute/registry
import gleam/erlang/process

pub fn register_and_lookup() {
  // Create a type-safe global subject with encoder/decoder
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()
  let global = global.new(encoder, decoder)
  
  // Register it globally
  let _ = registry.register_typed("my_service", global.subject(global))

  // From another node/process: look up the service type-safely
  let assert Ok(remote_service) = registry.whereis_global(
    "my_service",
    encoder,
    decoder
  )
  
  // Send a typed message through GlobalSubject
  let _ = global.send(remote_service, "Hello, world!")
  
  // Or use messaging API directly
  let _ = messaging.send_global_typed(
    "my_service", 
    "Hello, world!",
    encoder
  )
  
  // Receive typed messages
  let assert Ok(msg) = receiver.receive_typed(
    global.subject(global),
    decoder,
    1000
  )
}
```

### 4. Process Groups (Type-Safe)

```gleam
import distribute/codec
import distribute/codec/builder
import distribute/global
import distribute/groups

pub type Task {
  Task(name: String, id: Int)
}

pub fn use_groups() {
  // Build a codec for Task
  let #(task_encoder, task_decoder) = builder.custom2(
    codec.string_encoder(),
    codec.int_encoder(),
    codec.string_decoder(),
    codec.int_decoder(),
    Task,
    fn(t) { #(t.name, t.id) },
  )
  
  // Create a global subject for group communication
  let global = global.new(task_encoder, task_decoder)

  // Join a group named "workers"
  let _ = groups.join_typed("workers", global.subject(global))

  // Broadcast a typed message to all members
  let _ = global.send(global, Task("process_data", 42))

  // Get members list
  let members = groups.members_typed("workers")
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
| `distribute/registry` | Global process registration with type-safe subjects |
| `distribute/messaging` | Type-safe cross-node messaging with codecs |
| `distribute/groups` | Process groups with type-safe broadcast |
| `distribute/monitor` | Monitor processes and nodes |
| `distribute/remote_call` | Type-safe RPC to remote nodes |
| `distribute/codec` | Binary encoding/decoding for primitives and composite types |
| `distribute/codec/builder` | Helpers for building custom type codecs |
| `distribute/codec/tagged` | Tag and version validation for protocol safety |
| `distribute/global` | Type-safe global subjects with integrated codecs |
| `distribute/receiver` | Type-safe message receiving with codecs |
| `distribute/sugar` | Convenience helpers for common patterns |
| `distribute/actor` | Simple actor wrappers |
| `distribute/cluster/membership` | SWIM-like membership with gossip and failure detection |
| `distribute/cluster/gossip` | Gossip protocol for membership state propagation |
| `distribute/cluster/health` | Health checks for nodes and cluster |
| `distribute/election/raft_lite` | Lightweight leader election with term-based voting |

> **Note**: Legacy untyped functions (`send_global`, `broadcast`, `call`) are deprecated and will be removed in v3.0. Use the `_typed` variants with codecs for full type safety.

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

> **Note:** The legacy wrapper `examples/two_nodes/run_swim_integration.sh` has been removed — run `examples/two_nodes/swim_integration.sh` directly.


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
import distribute/settings

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
import distribute/remote_call
import gleam/io
import gleam/string

pub fn call_remote_with_timeout() {
  // Call erlang:node() on a remote node with a 10s timeout
  case remote_call.call_with_timeout("other@host", "erlang", "node", [], [], 10_000) {
    Ok(result) -> io.println("Remote node name: " <> string.inspect(result))
    Error(remote_call.RpcBadRpc(reason)) -> io.println("RPC failed: " <> reason)
    Error(_) -> io.println("RPC error")
  }
}
```

## Documentation

- Main documentation: https://hexdocs.pm/distribute/
- Examples overview: [examples/README.md](examples/README.md)

## Examples

- Multi-node shell scripts live in `examples/two_nodes/`.
- A tiny example project wrapper lives in `examples/two_nodes_app/`.

## Contributing

Contributions welcome! Please open an issue or PR on GitHub.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
