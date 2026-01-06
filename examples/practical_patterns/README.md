# Practical Patterns Examples

This directory contains practical, production-ready examples demonstrating common patterns in distributed Gleam applications using the `distribute` library.

## Examples Overview

### 1. Supervision Patterns (`supervision_example.gleam`)

Learn how to build fault-tolerant actor systems with supervision trees:

- **Single Supervised Actor**: Start an actor under a supervisor in one call
- **Worker Pools**: Create and manage pools of worker actors
- **Nested Supervision Trees**: Build multi-layer supervision hierarchies
- **Named Actors with Registry**: Combine supervision with global registration

**Key Functions Demonstrated:**
```gleam
// Start and supervise in one call
actor.start_typed_actor_supervised(init, loop, encoder, decoder)

// Create a worker pool
actor.pool_supervisor(
  pool_size: 3,
  init: worker_init,
  loop: worker_loop,
  encoder: encoder,
  decoder: decoder,
)

// Build custom supervision trees
static_supervisor.new(OneForOne)
|> static_supervisor.add(child_spec)
|> static_supervisor.start()
```

**When to Use:**
- Production services requiring automatic recovery
- Load-balanced worker pools
- Complex service architectures with dependencies
- Long-running processes that should restart on failure

---

### 2. Registry Patterns (`registry_patterns.gleam`)

Master actor discovery and coordination across distributed nodes:

- **Manual Registration**: Traditional two-step start + register
- **Convenience Registration**: Start and register in one call
- **Supervised + Registered**: Fault-tolerant + discoverable actors
- **Service Directory**: Multiple services with centralized discovery
- **Async Registration**: Non-blocking registration patterns

**Key Functions Demonstrated:**
```gleam
// Convenience: start and register together
actor.start_typed_actor_registered(
  name: "my-service",
  init: init,
  loop: loop,
  encoder: encoder,
  decoder: decoder,
)

// New convenience wrappers
registry.register_global(global_subject, "my-service")
registry.lookup_global("my-service", encoder, decoder)
registry.register_with_retry(global_subject, "my-service", retries: 3, delay: 100)
registry.lookup_with_timeout("my-service", encoder, decoder, timeout: 5000, poll: 100)
```

**When to Use:**
- Service discovery in microservices architecture
- Dynamic actor lookup across cluster nodes
- Leader election and singleton services
- Service mesh implementations

---

## Quick Start

### Running the Examples

```bash
# From the distribute project root
cd examples/practical_patterns

# Run supervision patterns
gleam run -m supervision_example

# Run registry patterns  
gleam run -m registry_patterns
```

### Integration into Your Project

Copy the patterns you need and adapt them:

```gleam
import distribute/actor
import distribute/registry
import distribute/global

// Pattern 1: Supervised worker pool
pub fn start_api_workers() {
  actor.pool_supervisor(
    pool_size: 10,
    init: api_worker_init,
    loop: api_worker_loop,
    encoder: request_encoder(),
    decoder: request_decoder(),
  )
}

// Pattern 2: Registered singleton service
pub fn start_auth_service() {
  actor.start_typed_actor_registered(
    name: "auth_service",
    init: auth_init,
    loop: auth_loop,
    encoder: auth_encoder(),
    decoder: auth_decoder(),
  )
}

// Pattern 3: Lookup with retry
pub fn get_database_service() {
  registry.lookup_with_timeout(
    "database_service",
    db_encoder(),
    db_decoder(),
    timeout_ms: 5000,
    poll_interval_ms: 100,
  )
}
```

---

## Pattern Comparison

| Pattern | Fault Tolerance | Discovery | Complexity | Use Case |
|---------|----------------|-----------|------------|----------|
| **Simple Actor** | ❌ None | ❌ No | Low | Scripts, tests |
| **Supervised** | ✅ Auto-restart | ❌ No | Medium | Background jobs |
| **Registered** | ❌ None | ✅ Global | Medium | Singletons |
| **Supervised + Registered** | ✅ Auto-restart | ✅ Global | Medium | Services |
| **Worker Pool** | ✅ Auto-restart | ❌ No | High | Load balancing |

---

## Best Practices

### 1. **Always Use Type-Safe GlobalSubject**
```gleam
// ✅ GOOD: Type-safe with encoder/decoder
let subject = actor.start_typed_actor(init, loop, encoder, decoder)

// ❌ AVOID: Raw Subject(BitArray) - no type safety
let subject = actor.start(init, loop)  // Deprecated
```

### 2. **Choose the Right Registration Pattern**
```gleam
// For services that must survive failures:
actor.start_typed_actor_supervised(...)
|> result.try(fn(#(_pid, subject)) {
  registry.register_global(subject, "my-service")
})

// For simple one-off services:
actor.start_typed_actor_registered("my-service", ...)
```

### 3. **Handle Registry Failures Gracefully**
```gleam
case registry.lookup_with_timeout(
  name: "remote-service",
  encoder: encoder,
  decoder: decoder,
  timeout_ms: 5000,
  poll_interval_ms: 100,
) {
  Ok(service) -> use_service(service)
  Error(_) -> {
    log.error("Service not available", [#("name", "remote-service")])
    fallback_behavior()
  }
}
```

### 4. **Use Worker Pools for Concurrency**
```gleam
// Instead of creating N actors manually:
actor.pool_supervisor(
  pool_size: runtime.schedulers_online() * 2,
  init: worker_init,
  loop: worker_loop,
  encoder: encoder,
  decoder: decoder,
)
```

---

## Common Pitfalls

### ❌ Don't: Register before starting supervisor
```gleam
// WRONG - actor might not be supervised yet
let #(_pid, subject) = actor.start_typed_actor_supervised(...)
registry.register_global(subject, "my-service")  // Race condition!
```

### ✅ Do: Register after supervision is confirmed
```gleam
// CORRECT - supervision started, then register
case actor.start_typed_actor_supervised(...) {
  Ok(#(_pid, subject)) -> registry.register_global(subject, "my-service")
  Error(err) -> Error(err)
}
```

---

### ❌ Don't: Use blocking lookups in hot paths
```gleam
// WRONG - blocks on every request
pub fn handle_request(req) {
  let service = registry.whereis_global("service", encoder, decoder)
  // Process request...
}
```

### ✅ Do: Cache the GlobalSubject
```gleam
// CORRECT - lookup once, reuse
pub fn init() {
  let service = registry.whereis_global("service", encoder, decoder)
  State(service: service)
}

pub fn handle_request(req, state) {
  global.send(state.service, req)
}
```

---

## Advanced Topics

### Multi-Node Setup

These patterns work seamlessly across distributed nodes:

```bash
# Terminal 1 - Start node A with service
iex --sname nodeA -S mix
> :net_kernel.connect_node(:"nodeB@localhost")

# Terminal 2 - Start node B, lookup service from A
iex --sname nodeB -S mix
> # Service registered on nodeA is visible here!
```

See `../advanced_patterns/` for SWIM and Raft integration examples.

### Observability

Add metrics to your actors:

```gleam
pub fn handle_message(msg, state) {
  metrics.increment("messages_processed")
  // ... handle message
}
```

### Testing

Test supervision and registration:

```gleam
import gleeunit/should

pub fn supervision_test() {
  // Start supervised actor
  let assert Ok(#(_pid, subject)) = 
    actor.start_typed_actor_supervised(init, loop, encoder, decoder)
  
  // Verify it's running
  let reply = process.new_subject()
  global.send(subject, Ping(reply))
  process.receive(reply, 1000) |> should.be_ok()
}
```

---

## Further Reading

- **Advanced Patterns**: See `../advanced_patterns/` for SWIM membership and Raft consensus
- **Two-Node Examples**: See `../two_nodes/` for cluster communication
- **Typed Messaging**: See `../typed_messaging/` for codec patterns

---

## Support

For questions or issues:
- Check the main [distribute README](../../README.md)
- Review the [MIGRATION guide](../../MIGRATION.md)
- Open an issue on GitHub

---

**Legend:**
- ✅ Recommended / Best Practice
- ❌ Anti-pattern / Avoid
- ⚠️ Use with Caution
