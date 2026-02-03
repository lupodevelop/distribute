# Registry Documentation

## Overview

The **registry** is the cluster-wide **name service**. It allows processes to be found by name from any node, without knowing their Pid.

Think of it as: **"I want to reach the 'user-service' wherever it runs"**

### Key Features

- ✅ Global name registration across all cluster nodes
- ✅ Automatic conflict resolution on network partitions
- ✅ Type-safe lookup via `GlobalSubject`
- ✅ Retry with exponential backoff + jitter
- ✅ Supports both `GlobalSubject(msg)` and raw `Pid`

---

## Quick Start

### Basic Registration

```gleam
import distribute/registry

// Register a process by name
let result = registry.register("my-service", my_pid)

case result {
  Ok(Nil) -> logger.info("Registered")
  Error(registry.AlreadyRegistered) -> logger.error("Name taken")
  Error(e) -> logger.error("Registration failed: " <> string.inspect(e))
}
```

### Lookup & Send

```gleam
// Look up the process
case registry.whereis("my-service") {
  Ok(pid) -> {
    // Send a message via subject
    let subject = process.unsafely_create_subject(pid, dynamic.nil())
    process.send(subject, raw_message)
  }
  Error(Nil) -> logger.info("Service not found")
}
```

---

## Type-Safe Pattern: GlobalSubject

### Registration with GlobalSubject

```gleam
import distribute/global
import distribute/registry

// Create a global subject (has encoder/decoder)
let global = global.new(my_encoder(), my_decoder())

// Register it
case registry.register_global(global, "my-service") {
  Ok(Nil) -> logger.info("Service registered")
  Error(e) -> logger.error("Failed: " <> string.inspect(e))
}
```

### Lookup with GlobalSubject

```gleam
// Type-safe lookup
case registry.whereis_global("my-service", my_encoder(), my_decoder()) {
  Ok(global) -> {
    // Now you can safely send/receive typed messages
    case global.send(global, MyMessage) {
      Ok(Nil) -> logger.info("Message sent")
      Error(e) -> logger.error("Send failed")
    }
  }
  Error(Nil) -> logger.info("Service not found")
}
```

---

## Retry with Backoff (Production)

For distributed systems with transient connectivity issues, use **retry with exponential backoff**:

```gleam
import distribute/registry
import distribute/retry
import distribute/global

pub fn register_with_retries(
  global: global.GlobalSubject(msg),
  name: String,
) -> Result(Nil, String) {
  // Use exponential backoff with jitter (recommended)
  let policy = retry.default_with_jitter()
  
  case registry.register_with_strategy(global, name, policy) {
    Ok(Nil) -> {
      logger.info("Registered after retry")
      Ok(Nil)
    }
    Error(registry.AlreadyRegistered) -> {
      logger.info("Name already registered, skipping")
      Ok(Nil)
    }
    Error(registry.NetworkError(msg)) -> {
      logger.error("Network error: " <> msg)
      Error("Network error")
    }
    Error(e) -> {
      logger.error("Registration failed: " <> string.inspect(e))
      Error("Registration failed")
    }
  }
}
```

---

## Registry Errors

| Error | Meaning | Recovery |
|-------|---------|----------|
| `AlreadyRegistered` | Name taken by another process | Choose different name or wait for unregister |
| `InvalidProcess` | Process not alive | Ensure process started before registering |
| `InvalidName(msg)` | Name contains invalid chars | Use alphanumeric + `-._` only |
| `NetworkError(msg)` | Partition or connectivity issue | Retry with backoff |
| `RegisterFailed(msg)` | Generic failure | Log and investigate |

---

## Common Patterns

### Pattern 1: Actor Registration

```gleam
import distribute/actor
import distribute/global
import distribute/registry

pub type MyMsg {
  Ping
}

pub fn start_my_actor() -> Result(global.GlobalSubject(MyMsg), String) {
  let global = global.new(my_encoder(), my_decoder())
  
  // Start the actor
  case actor.start_typed_actor(
    initial_state,
    handler,
  ) {
    Ok(Nil) -> {
      // Register it globally
      case registry.register_global(global, "my-actor") {
        Ok(Nil) -> Ok(global)
        Error(e) -> Error("Registration failed: " <> string.inspect(e))
      }
    }
    Error(e) -> Error("Actor start failed: " <> string.inspect(e))
  }
}
```

### Pattern 2: Service Discovery

```gleam
import distribute/registry

pub fn find_service(name: String) -> Result(global.GlobalSubject(ServiceMsg), Nil) {
  registry.whereis_global(name, service_encoder(), service_decoder())
}

pub fn call_service(name: String, request: ServiceMsg) -> Result(Nil, String) {
  use global <- result.try(
    find_service(name)
    |> result.map_error(fn(_) { "Service not found: " <> name })
  )
  
  global.send(global, request)
  |> result.map_error(fn(e) { "Send failed: " <> string.inspect(e) })
}
```

### Pattern 3: Failover Registration

```gleam
import distribute/registry

pub fn register_or_failover(
  global: global.GlobalSubject(msg),
  primary_name: String,
  fallback_name: String,
) -> Result(Nil, String) {
  // Try primary first
  case registry.register_global(global, primary_name) {
    Ok(Nil) -> {
      logger.info("Registered as primary")
      Ok(Nil)
    }
    Error(registry.AlreadyRegistered) -> {
      logger.info("Primary taken, registering as fallback")
      // Register as fallback
      case registry.register_global(global, fallback_name) {
        Ok(Nil) -> Ok(Nil)
        Error(e) -> Error("Fallback registration failed: " <> string.inspect(e))
      }
    }
    Error(e) -> Error("Registration failed: " <> string.inspect(e))
  }
}
```

---

## Lookup Variants

### 1. Type-Safe (Recommended)

```gleam
// Returns GlobalSubject(msg) with encoder/decoder
registry.whereis_global("service", encoder(), decoder())
```

Best for: Most use cases, guaranteed type safety

### 2. With Custom Tag

```gleam
// For OTP actors with custom tags
registry.whereis_with_tag("service", my_custom_tag)
```

Best for: OTP actors where you control the message tag

### 3. Raw Pid

```gleam
// Just get the Pid, create Subject manually
case registry.whereis("service") {
  Ok(pid) -> {
    let subject = process.unsafely_create_subject(pid, dynamic.nil())
    // Use subject...
  }
  Error(Nil) -> logger.info("Not found")
}
```

Best for: Low-level operations, raw Pid passing

---

## Unregistration

### Simple Unregister

```gleam
case registry.unregister("my-service") {
  Ok(Nil) -> logger.info("Unregistered")
  Error(e) -> logger.error("Unregister failed: " <> string.inspect(e))
}
```

### Automatic Cleanup

Processes that crash are automatically unregistered. No manual cleanup needed.

```gleam
// If actor crashes:
// 1. Process terminates
// 2. Erlang detects dead process
// 3. Global registry auto-cleans the name
// 4. New registration can reuse the name
```

---

## Storage API (Advanced)

For OTP actors with custom tags, you may need to preserve the exact `Subject` including its tag. Use the storage API:

```gleam
import distribute/registry

// Store a Subject with its tag
let subject = process.unsafely_create_subject(pid, my_tag)
case registry.store_subject("my-actor", subject) {
  Ok(Nil) -> logger.info("Subject stored")
  Error(e) -> logger.error("Storage failed: " <> string.inspect(e))
}

// Retrieve the exact Subject later
case registry.lookup_subject("my-actor") {
  Ok(subject) -> {
    // Subject has the original tag intact
    process.send(subject, my_message)
  }
  Error(Nil) -> logger.info("Not found")
}

// Clean up
let _ = registry.remove_stored_subject("my-actor")
```

---

## Network Partitions & Conflicts

When a network partition heals, Erlang's global module automatically detects conflicting registrations.

**Behavior:**

```
Node A: registers "service" → pid_a
Node B: registers "service" → pid_b

[Network partition occurs]

[Partition heals]
→ Erlang detects conflict
→ Applies arbitration (typically the first process wins)
→ One registration kept, the other fails
```

**Mitigation:**

Use **unique names** with node identifiers:

```gleam
// Bad: Same name on all nodes
registry.register_global(service, "user-service")

// Good: Unique per node
let node_name = erlang.node_name() |> result.unwrap("")
let name = "user-service:" <> node_name
registry.register_global(service, name)

// Or use retry logic
let policy = retry.aggressive()
registry.register_with_strategy(service, "user-service", policy)
```

---

## Best Practices

1. **Always use type-safe lookup**
   ```gleam
   // ✅ Good
   registry.whereis_global(name, encoder(), decoder())
   
   // ❌ Avoid
   registry.whereis(name)
   ```

2. **Use retry for production**
   ```gleam
   let policy = retry.default_with_jitter()
   registry.register_with_strategy(global, name, policy)
   ```

3. **Register at startup, not per-call**
   ```gleam
   // ✅ Good: Register once
   pub fn start() {
     registry.register_global(service, "my-service")
   }
   
   // ❌ Bad: Register every call
   pub fn do_something() {
     registry.register_global(service, "my-service")
   }
   ```

4. **Handle lookup failures gracefully**
   ```gleam
   case registry.whereis_global(name, enc, dec) {
     Ok(service) -> use_service(service)
     Error(Nil) -> logger.warn("Service not available")
   }
   ```

5. **Document registry names**
   ```gleam
   // Service registry names (registered by service modules)
   // - "user-service" → User management actor
   // - "auth-service" → Authentication actor
   // - "cache:users" → User cache (per-node)
   ```

---

## Next Steps

- [Global Subjects](../global/README.md) — Type-safe message subjects
- [Messaging](../messaging/README.md) — Request/reply patterns
- [Retry](../retry/README.md) — Backoff strategies with jitter

---

*See also: [Erlang :global documentation](https://erlang.org/doc/man/global.html)*
