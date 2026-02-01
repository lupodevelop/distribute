# Actor Documentation

## Overview

Actors are the **building blocks** of distributed systems in distribute. An actor is a process that:

- ✅ Receives typed messages
- ✅ Maintains internal state
- ✅ Processes messages sequentially (one at a time)
- ✅ Can be registered globally for discovery
- ✅ Can be supervised for automatic restart on crash

This module provides type-safe helpers on top of OTP's standard actor model.

---

## Quick Start

### Simple Stateful Actor

```gleam
import distribute/actor
import distribute/registry
import gleam/erlang/process

pub type CounterMsg {
  Increment
  Decrement
  GetValue(reply: Subject(Int))
}

// Create encoder/decoder (see codec documentation)
fn counter_encoder() -> codec.Encoder(CounterMsg) { ... }
fn counter_decoder() -> codec.Decoder(CounterMsg) { ... }

// Message handler
fn counter_handler(msg: CounterMsg, count: Int) -> receiver.Next(Int) {
  case msg {
    Increment -> receiver.Continue(count + 1)
    Decrement -> receiver.Continue(count - 1)
    GetValue(reply) -> {
      process.send(reply, count)
      receiver.Continue(count)
    }
  }
}

// Start the actor
let counter = actor.start_typed_actor(
  0,  // initial state
  counter_encoder(),
  counter_decoder(),
  counter_handler,
)

// Register it
case registry.register_global(counter, "counter:main") {
  Ok(Nil) -> logger.info("Counter registered")
  Error(e) -> logger.error("Registration failed: " <> string.inspect(e))
}
```

### Using the Actor

```gleam
// Lookup the counter
case registry.whereis_global("counter:main", counter_encoder(), counter_decoder()) {
  Ok(counter) -> {
    // Send a message
    case actor.send(counter, Increment) {
      Ok(Nil) -> logger.info("Sent")
      Error(e) -> logger.error("Failed: " <> string.inspect(e))
    }
  }
  Error(Nil) -> logger.info("Counter not found")
}
```

---

## Core Concepts

### 1. Message Handler

The handler is a function that processes one message at a time:

```gleam
fn handler(message: Msg, state: State) -> receiver.Next(State)
```

It returns one of three actions:

| Return | Meaning |
|--------|---------|
| `receiver.Continue(new_state)` | Process next message |
| `receiver.Stop` | Graceful shutdown |
| `receiver.StopAbnormal(reason)` | Crash with reason |

### 2. GlobalSubject(msg)

Every actor returns a `GlobalSubject(msg)` that:

- ✅ Encodes messages automatically on send
- ✅ Decodes messages automatically on receive
- ✅ Is type-safe at compile time
- ✅ Can be registered and looked up
- ✅ Can be passed to other processes

### 3. State Management

State is private to the actor. It's only accessible via messages:

```gleam
// ❌ Can't do this (state is not shared):
let my_value = get_internal_state(actor)

// ✅ Do this instead (send a message requesting the value):
pub type Request {
  GetValue(reply: Subject(Int))
}

// In handler:
GetValue(reply) -> {
  process.send(reply, internal_value)
  receiver.Continue(state)
}
```

---

## Actor Types

### 1. Simple Actor (Stateful Loop)

```gleam
actor.start_typed_actor(
  initial_state: 0,
  encoder: my_encoder(),
  decoder: my_decoder(),
  handler: fn(msg, state) {
    case msg {
      Increment -> receiver.Continue(state + 1)
      GetValue(reply) -> {
        process.send(reply, state)
        receiver.Continue(state)
      }
    }
  },
)
```

**Use when:** You need a simple actor with internal state and basic message handling.

### 2. Request-Response Server

```gleam
actor.start_server(
  initial_state: Nil,
  encoder: request_encoder(),
  decoder: request_decoder(),
  handler: fn(request, _state) {
    case request {
      Add(a, b, reply) -> {
        process.send(reply, a + b)
        receiver.Continue(Nil)
      }
      Multiply(a, b, reply) -> {
        process.send(reply, a * b)
        receiver.Continue(Nil)
      }
    }
  },
)
```

**Use when:** Your actor is mainly a stateless request handler.

**Note:** `start_server` is semantically identical to `start_typed_actor` but signals intent more clearly.

### 3. Registered Actor (Start + Register)

```gleam
actor.start_typed_actor_registered(
  "my-service",
  initial_state: [],
  encoder: msg_encoder(),
  decoder: msg_decoder(),
  handler: my_handler,
)
```

**Use when:** You want to start and register in one call.

---

## Patterns

### Pattern 1: Counter with RPC

```gleam
pub type CounterMsg {
  Increment
  GetValue(reply: Subject(Int))
}

let counter = actor.start_typed_actor(
  0,
  encoder(),
  decoder(),
  fn(msg, count) {
    case msg {
      Increment -> receiver.Continue(count + 1)
      GetValue(reply) -> {
        process.send(reply, count)
        receiver.Continue(count)
      }
    }
  },
)

// Register it
registry.register_global(counter, "counter")

// Client code: query the counter
case registry.whereis_global("counter", encoder(), decoder()) {
  Ok(counter) -> {
    // Send GetValue request
    let response_subject = process.new_subject()
    case global.send(counter, GetValue(response_subject)) {
      Ok(Nil) -> {
        // Wait for response
        case process.receive(response_subject, 1000) {
          Ok(value) -> logger.info("Counter: " <> int.to_string(value))
          Error(Nil) -> logger.error("Timeout")
        }
      }
      Error(e) -> logger.error("Send failed")
    }
  }
  Error(Nil) -> logger.info("Counter not found")
}
```

### Pattern 2: State-Dependent Behavior

```gleam
pub type ConnectionMsg {
  Connect(host: String)
  Disconnect
  SendData(data: String, reply: Subject(Result(Nil, String)))
}

pub type ConnectionState {
  Disconnected
  Connected(host: String)
}

let actor = actor.start_typed_actor(
  Disconnected,
  encoder(),
  decoder(),
  fn(msg, state) {
    case #(msg, state) {
      #(Connect(host), Disconnected) -> {
        // Actually connect here
        receiver.Continue(Connected(host))
      }
      #(Disconnect, Connected(_)) -> {
        receiver.Continue(Disconnected)
      }
      #(SendData(data, reply), Connected(host)) -> {
        case send_to_host(host, data) {
          Ok(Nil) -> process.send(reply, Ok(Nil))
          Error(e) -> process.send(reply, Error(e))
        }
        receiver.Continue(state)
      }
      #(SendData(_, reply), Disconnected) -> {
        process.send(reply, Error("Not connected"))
        receiver.Continue(state)
      }
      _ -> receiver.Continue(state)
    }
  },
)
```

### Pattern 3: Worker Pool

```gleam
pub type WorkerMsg {
  Process(job: String, reply: Subject(Result(String, String)))
  Shutdown
}

pub fn start_worker_pool(count: Int) {
  list.range(1, count)
  |> list.map(fn(i) {
    let worker = actor.start_typed_actor(
      Nil,
      encoder(),
      decoder(),
      fn(msg, _state) {
        case msg {
          Process(job, reply) -> {
            let result = do_work(job)
            process.send(reply, result)
            receiver.Continue(Nil)
          }
          Shutdown -> receiver.Stop
        }
      },
    )
    registry.register_global(worker, "worker:" <> int.to_string(i))
  })
}

// Client: send work to random worker
pub fn submit_work(job: String) -> Result(String, String) {
  let worker_id = random_worker_id()
  case registry.whereis_global("worker:" <> worker_id, encoder(), decoder()) {
    Ok(worker) -> {
      let reply = process.new_subject()
      case global.send(worker, Process(job, reply)) {
        Ok(Nil) -> {
          case process.receive(reply, 10_000) {
            Ok(result) -> result
            Error(Nil) -> Error("Timeout")
          }
        }
        Error(e) -> Error("Send failed")
      }
    }
    Error(Nil) -> Error("Worker not found")
  }
}
```

---

## Supervision

Actors can be supervised to automatically restart on crash:

```gleam
import gleam/otp/supervision.{supervisor, worker}

pub fn start_supervised() {
  supervisor.start_link(fn(children) {
    children
    |> supervision.add(actor.child_spec_typed_actor(
      initial_state: 0,
      decoder: my_decoder(),
      handler: my_handler,
    ))
    |> supervision.add(actor.child_spec_typed_actor(
      initial_state: [],
      decoder: other_decoder(),
      handler: other_handler,
    ))
  })
}
```

**Restart strategies:**
- ✅ Permanent: Restart always
- ⚠️ Transient: Restart only on abnormal exit
- 🔴 Temporary: Never restart

See [gleam_otp supervision docs](https://hexdocs.pm/gleam_otp/gleam/otp/supervision.html) for details.

---

## Error Handling

### Message Decode Failures

If a message can't be decoded, it's silently ignored (no crash):

```gleam
case global.send(actor, malformed_message) {
  Ok(Nil) -> {
    // Message sent to actor
    // If decoder fails, actor ignores it
  }
  Error(codec.InvalidValue(msg)) -> {
    // Encoding failed BEFORE sending
    logger.error("Encode error: " <> msg)
  }
}
```

### Handler Crashes

If the handler crashes, the actor dies (and supervisor restarts if configured):

```gleam
fn risky_handler(msg, state) {
  // This will crash the actor:
  case msg {
    Process(data) -> {
      let _ = string.slice(data, 0, 1000)  // Can panic if data is small
      receiver.Continue(state)
    }
  }
}
```

**Mitigation:**

```gleam
fn safe_handler(msg, state) {
  case msg {
    Process(data) -> {
      case string.slice(data, 0, 1000) {
        Ok(slice) -> {
          logger.info("Processed: " <> slice)
          receiver.Continue(state)
        }
        Error(_) -> {
          logger.warn("Slice failed")
          receiver.Continue(state)
        }
      }
    }
  }
}
```

---

## Graceful Shutdown

### Explicit Shutdown

```gleam
pub type Msg {
  DoWork
  Shutdown
}

fn handler(msg, state) {
  case msg {
    DoWork -> receiver.Continue(state)
    Shutdown -> receiver.Stop  // Graceful shutdown
  }
}

// Send shutdown signal
case global.send(actor, Shutdown) {
  Ok(Nil) -> logger.info("Shutdown initiated")
  Error(e) -> logger.error("Failed to signal shutdown")
}
```

### Abnormal Shutdown

```gleam
fn handler(msg, state) {
  case msg {
    FatalError -> {
      receiver.StopAbnormal("Something went wrong")
    }
  }
}
```

### Supervised Cleanup

When an actor crashes with supervisor:

1. Supervisor detects crash
2. Logs the crash reason
3. Waits before restarting (based on strategy)
4. Restarts the actor with fresh state

---

## Best Practices

1. **Keep message handling fast**
   ```gleam
   // ✅ Good: Return quickly
   fn handler(msg, state) {
     case msg {
       Work(data) -> {
         spawn_async_task(data)  // Don't block here
         receiver.Continue(state)
       }
     }
   }
   
   // ❌ Bad: Slow blocking operation
   fn handler(msg, state) {
     case msg {
       Work(data) -> {
         do_long_computation(data)  // Blocks all messages
         receiver.Continue(state)
       }
     }
   }
   ```

2. **Handle all message cases**
   ```gleam
   // ✅ Good: Explicit
   fn handler(msg, state) {
     case msg {
       Msg1 -> receiver.Continue(state)
       Msg2 -> receiver.Continue(state)
       _ -> receiver.Continue(state)  // Fallback
     }
   }
   ```

3. **Use pattern matching for state-dependent behavior**
   ```gleam
   // ✅ Good: Clear intent
   case #(msg, state) {
     #(Connect, Disconnected) -> receiver.Continue(Connected)
     #(Disconnect, Connected) -> receiver.Continue(Disconnected)
     _ -> receiver.Continue(state)
   }
   ```

4. **Register after starting**
   ```gleam
   // ✅ Good
   let actor = actor.start_typed_actor(...)
   registry.register_global(actor, "name")
   
   // ❌ Avoid
   actor.start_typed_actor_registered(...)  // Unless you want one-liner
   ```

5. **Reply to requesters promptly**
   ```gleam
   // ✅ Good: Reply immediately
   GetValue(reply) -> {
     process.send(reply, state.value)
     receiver.Continue(state)
   }
   
   // ❌ Bad: Delay reply
   GetValue(reply) -> {
     // Do something else first
     expensive_operation()
     process.send(reply, state.value)
     receiver.Continue(state)
   }
   ```

---

## Deprecations

### Legacy APIs (Removed in v3.0.0)

| Old | New | Notes |
|-----|-----|-------|
| `actor.start(state, decoder, handler)` | `actor.start_typed_actor(state, encoder, decoder, handler)` | Use type-safe version |
| `actor.start_global(state, decoder, handler)` | `actor.start_typed_actor(...)` + `registry.register_typed(...)` | Split concerns |

See [MIGRATION.md](../../MIGRATION.md) for upgrade guide.

---

## Next Steps

- [Global Subjects](../global/README.md) — How actors communicate
- [Registry](../registry/README.md) — How to register actors
- [Messaging](../messaging/README.md) — Request/reply patterns
- [Supervision](../supervision/README.md) — Restart policies

---

*For low-level actor operations, see [gleam_otp documentation](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html).*
