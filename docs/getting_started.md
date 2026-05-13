# Getting Started

`distribute` is a typed messaging layer for distributed Gleam applications. This guide will walk you through the first-time setup and configuration.

## Installation

Add `distribute` to your Gleam project:

```sh
gleam add distribute
```

The library requires **Gleam >= 1.16.0** and runs exclusively on the **Erlang** target.

## Initializing the Node

To communicate across the network, your BEAM instance must be started as a distributed node with a name and a shared secret (cookie).

```gleam
import distribute

pub fn main() {
  // Start this node. The name must contain an '@'.
  let assert Ok(Nil) = distribute.start_node("myapp@127.0.0.1", "secret-cookie")

  // Connect to another known node
  let _ = distribute.connect("other_app@127.0.0.1")
}
```

> [!TIP]
> Use `distribute.self_node()` to check your own name and `distribute.nodes()` to see who you are currently connected to.

You can also take a deterministic health snapshot at any time:

```gleam
let h = distribute.health()
// `reachable_nodes` and `unreachable_nodes` keep the same order as
// `connected_nodes`, even if ping replies arrive out-of-order.
```

## Global Configuration

`distribute` uses Erlang's `persistent_term` to store its configuration for zero-overhead access. You should configure the library **once** during your application's boot sequence.

```gleam
import distribute
import distribute/config
import gleam/io

pub fn init() {
  let cfg = config.Config(
    ..config.default(),
    default_call_timeout_ms: 10_000,
    default_init_timeout_ms: 5_000,
    max_payload_size_bytes: 2 * 1024 * 1024, // 2 MiB
  )

  case distribute.configure(cfg) {
    Ok(Nil) -> io.println("Configured successfully")
    Error(config.AlreadyConfigured) -> io.println("Warning: already configured")
    Error(err) -> io.println("Error: " <> config.config_error_to_string(err))
  }
}
```

### Configuration Defaults

If you don't call `configure`, the library uses the following defaults:

- **Call Timeout**: 5,000 ms
- **Init Timeout**: 5,000 ms
- **Max Payload**: 4 MiB
- **Max Distribution Atoms**: 10,000

> [!CAUTION]
> **Immutability and GC**: Configuration is effectively immutable. Every call to `distribute.configure` triggers a global garbage collection pass in the Erlang VM to update process heaps. The v4.0.0 release strictly enforces this by returning `AlreadyConfigured` on subsequent calls.

## Next Steps

- [Learn about Actors and the Registry](./actors_and_registry.md)
- [Master Messaging and the Call pattern](./messaging.md)
