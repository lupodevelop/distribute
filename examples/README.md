# Examples

This folder contains runnable integration examples demonstrating **type-safe** distributed messaging with the `distribute` library.

> These examples use the new **GlobalSubject** with integrated codecs, showing compile-time type safety for cross-node communication.

## Prerequisites

- `gleam` in your `PATH`
- Erlang/OTP installed (so `erl` and `erlc` are available)

## Available Examples

### 1. `two_nodes/` — Basic Type-Safe Messaging

Demonstrates:
- Creating `GlobalSubject` with codecs
- Type-safe global registration
- Type-safe message sending with `send_global_typed`

**Run:**
```sh
./examples/two_nodes/integration_test.sh
```

### 2. `two_nodes_app/` — SWIM Membership + Type-Safe Messaging

Demonstrates:
- `GlobalSubject` with membership service
- Leader election
- Type-safe distributed messaging

**Run:**
```sh
./examples/two_nodes_app/scripts/run_integration.sh
```

### 3. `typed_messaging/` — Advanced Type-Safe Examples

Demonstrates:
- `GlobalSubject` with custom codecs
- Type-safe process groups
- Type-safe broadcast
- Receiving typed messages with `global.receive(global, timeout_ms)`

**Run:**
```sh
# Terminal 1
gleam run -m examples/typed_messaging/typed_node_a

# Terminal 2
gleam run -m examples/typed_messaging/typed_node_b
```

**Example of receiving a message:**
```gleam
case global.receive(my_global, 5_000) {
  Ok(msg) -> // use msg
  Error(_) -> // timeout or decode error
}
```

## What's New in v2.0

All examples now use:
- ✅ **GlobalSubject** with integrated encoder/decoder
- ✅ **Type-safe APIs** (`send_global_typed`, `broadcast_typed`)
- ✅ **No unsafe Erlang terms** - everything goes through codecs
- ✅ **Compile-time type checking** for distributed messages

## Integration Tests

The `two_nodes/` directory also contains additional integration test scripts:
- `swim_integration.sh` - SWIM membership protocol test (moved from test/)
- `partition_rejoin.sh` - Network partition handling
- `raft_failover_test.sh` - Leader election failover
