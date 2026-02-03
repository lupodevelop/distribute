# Advanced Patterns with SWIM and Raft

This directory contains advanced examples demonstrating integration of `distribute/actor` with SWIM membership and Raft leader election.

## Examples

### 1. SWIM-based Service Coordinator (`swim_coordinator.gleam`)

A distributed service coordinator that uses SWIM for failure detection:

**Features:**
- Automatic service registration and discovery
- Health monitoring using SWIM membership protocol
- Automatic removal of failed services
- Supervision support for high availability

**Usage:**

```gleam
import advanced_patterns/swim_coordinator

pub fn main() {
  // Start coordinator
  let assert Ok(coordinator) = swim_coordinator.start()
  
  // Register a service
  let _ = swim_coordinator.register_service(
    coordinator,
    "api_server",
    ["http", "json"],
  )
  
  // Get available services (only alive nodes)
  let assert Ok(services) = swim_coordinator.get_services(coordinator)
  io.println("Available services: " <> string.inspect(services))
}
```

**With Supervision:**

```gleam
// Start under supervisor for automatic restart on failure
let assert Ok(#(sup_pid, coordinator)) = 
  swim_coordinator.start_supervised()

// Coordinator will be automatically restarted if it crashes
```

### 2. Raft-based Key-Value Store (`raft_kv_store.gleam`)

A consistent distributed KV store using Raft for leader election:

**Features:**
- Strong consistency through leader-based writes
- Automatic leader election and failover
- Read replicas for scalability
- Worker pool support for concurrent reads

**Usage:**

```gleam
import advanced_patterns/raft_kv_store

pub fn main() {
  // Start KV store
  let assert Ok(store) = raft_kv_store.start()
  
  // Put a value (only leader accepts writes)
  case raft_kv_store.put(store, "user:1", "Alice") {
    Ok(_) -> io.println("✓ Written to leader")
    Error(msg) -> io.println("✗ Write failed: " <> msg)
  }
  
  // Get a value (any node can serve reads)
  case raft_kv_store.get(store, "user:1") {
    Ok(value) -> io.println("Value: " <> value)
    Error(msg) -> io.println("Not found: " <> msg)
  }
}
```

**With Worker Pool:**

```gleam
// Start with 5 read replicas for load balancing
let assert Ok(#(sup_pid, workers)) = 
  raft_kv_store.start_with_pool(5)

// Distribute reads across workers
list.each(workers, fn(worker) {
  case raft_kv_store.get(worker, "user:1") {
    Ok(value) -> io.println("Got: " <> value)
    Error(_) -> Nil
  }
})
```

## Architecture Patterns

### Pattern 1: Supervised Typed Actors

Using `actor.start_typed_actor_supervised` for high availability:

```gleam
let result = actor.start_typed_actor_supervised(
  initial_state,
  encoder(),
  decoder(),
  handle_message,
)

case result {
  Ok(#(supervisor, actor_subject)) -> {
    // Actor is under supervision
    // Will be automatically restarted on crash
    // Use actor_subject for messaging
  }
  Error(err) -> // handle error
}
```

### Pattern 2: Worker Pools

Using `actor.pool_supervisor` for parallel processing:

```gleam
let result = actor.pool_supervisor(
  pool_size: 10,
  initial_state,
  encoder(),
  decoder(),
  handle_message,
)

case result {
  Ok(#(supervisor, workers)) -> {
    // All workers are supervised
    // Use workers list for load balancing
    // Round-robin or random distribution
  }
  Error(err) -> // handle error
}
```

### Pattern 3: SWIM + Actors

Combining SWIM membership with typed actors:

1. Start SWIM membership service: `membership.start_service(interval_ms)`
2. Create typed actor with health checks
3. Use `membership.alive()` to filter available services
4. Automatically detect and handle node failures

### Pattern 4: Raft + Actors

Combining Raft election with typed actors:

1. Start Raft service: `raft_lite.start_service()`
2. Check leadership: `raft_lite.am_i_leader()`
3. Route writes to leader only
4. Replicate state changes across cluster
5. Handle leader changes gracefully

## Testing Multi-Node Setup

### Terminal 1 - Start Node A:

```bash
cd examples/advanced_patterns
gleam run -m swim_coordinator -- --name nodeA@127.0.0.1
```

### Terminal 2 - Start Node B:

```bash
cd examples/advanced_patterns
gleam run -m swim_coordinator -- --name nodeB@127.0.0.1
```

### Terminal 3 - Connect Nodes:

```bash
# On Node B
cluster.connect("nodeA@127.0.0.1")
```

Now services registered on either node will be visible cluster-wide, with automatic failure detection through SWIM.

## Best Practices

1. **Always use supervision** for production actors
2. **Handle leader changes** gracefully in Raft-based systems
3. **Use worker pools** for CPU-intensive or concurrent operations
4. **Monitor SWIM metrics** for cluster health (`membership.metrics()`)
5. **Implement proper error handling** for network partitions
6. **Use typed messages** for compile-time safety
7. **Register actors globally** for cross-node access
8. **Test failure scenarios** (node crashes, network partitions)

## Related Documentation

- [Actor Module](../../src/distribute/actor.gleam)
- [SWIM Membership](../../src/distribute/cluster/membership.gleam)
- [Raft Election](../../src/distribute/election/raft_lite.gleam)
- [Supervision](https://hexdocs.pm/gleam_otp/gleam/otp/supervision.html)

## Performance Considerations

- **SWIM overhead**: Probe interval affects failure detection speed vs network load
- **Raft elections**: Minimize leader changes for stability
- **Worker pools**: Size based on workload (CPU-bound: #cores, IO-bound: higher)
- **Message encoding**: Use efficient codecs for high-throughput scenarios
- **Registry lookups**: Cache global subjects when possible

## License

See main project LICENSE.
