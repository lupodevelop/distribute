# Global Subjects Documentation

## Overview

`GlobalSubject(msg)` is the **type-safe wrapper** around `Subject(BitArray)`. It ensures that every message sent across the cluster is automatically encoded/decoded using your custom codecs.

### Why GlobalSubject?

When a message crosses a node boundary, it becomes `BitArray`. The original type information is lost at runtime. `GlobalSubject` keeps your encoder/decoder paired with the subject so:

- ✅ Encoding happens automatically on send
- ✅ Decoding happens automatically on receive
- ✅ Type mismatches are caught at compile time (or decoded as errors at runtime)
- ✅ No manual `encode()`/`decode()` calls in business logic

---

## Quick Start

### Basic Example: Counter Actor

```gleam
import distribute/global
import distribute/codec

pub type CounterMsg {
  Increment
  GetCount
}

// Create a counter actor that responds globally
fn counter_actor() {
  let encoder = my_counter_encoder()
  let decoder = my_counter_decoder()
  let global = global.new(encoder, decoder)
  
  // Register it
  registry.register(global, "counter:1")
  
  // Loop receiving messages
  loop_counter(global, count: 0)
}

fn loop_counter(global: GlobalSubject(CounterMsg), count: Int) {
  case global.receive(global, 5000) {
    Ok(Increment) -> {
      loop_counter(global, count: count + 1)
    }
    Ok(GetCount) -> {
      // Handle GetCount (would need a RPC pattern)
      loop_counter(global, count)
    }
    Error(codec.DecodeTimeout) -> {
      loop_counter(global, count)
    }
    Error(e) -> {
      logger.error("Decode error: " <> string.inspect(e))
      loop_counter(global, count)
    }
  }
}

// Send a message to the counter
let assert Ok(counter_subject) = registry.lookup("counter:1")
case global.send(counter_subject, Increment) {
  Ok(Nil) -> logger.info("Sent")
  Error(e) -> logger.error("Send failed: " <> string.inspect(e))
}
```

---

## Creating GlobalSubjects

### 1. From Current Process

```gleam
let global = global.new(my_encoder(), my_decoder())
```

Creates a `GlobalSubject` pointing to the **current process** (`process.self()`).

### 2. From a Known Pid

```gleam
let pid = process.subject_owner(some_subject) |> result.unwrap_error(Nil)
let global = global.from_pid(pid, my_encoder(), my_decoder())
```

When you look up a process by name and get its `Pid`.

### 3. From an Existing Subject

```gleam
let subject = process.unsafely_create_subject(pid, dynamic.nil())
let global = global.from_subject(subject, my_encoder(), my_decoder())
```

When you already have a `Subject(BitArray)`.

---

## Sending & Receiving

### Sending

```gleam
pub fn send(
  global: GlobalSubject(msg),
  message: msg,
) -> Result(Nil, codec.EncodeError)
```

Automatically encodes the message and sends it.

```gleam
case global.send(counter, Increment) {
  Ok(Nil) -> {
    // Message sent
  }
  Error(codec.ValueTooLarge(msg)) -> {
    // Message too large
  }
  Error(codec.InvalidValue(msg)) -> {
    // Message validation failed
  }
  Error(e) -> {
    // Other encoding error
  }
}
```

### Receiving

```gleam
pub fn receive(
  global: GlobalSubject(msg),
  timeout_ms: Int,
) -> Result(msg, codec.DecodeError)
```

Blocks waiting for a message and automatically decodes it.

```gleam
case global.receive(counter, 5000) {
  Ok(Increment) -> {
    // Handle message
  }
  Ok(GetCount) -> {
    // Handle message
  }
  Error(codec.DecodeTimeout) -> {
    // No message received in 5 seconds
  }
  Error(codec.InvalidBinary(msg)) -> {
    // Malformed data
  }
  Error(codec.TypeMismatch(msg)) -> {
    // Correct type but wrong data
  }
  Error(e) -> {
    logger.error("Decode failed: " <> string.inspect(e))
  }
}
```

---

## Accessing Internal Values

### Get the Encoder/Decoder

```gleam
let encoder = global.encoder(subject)
let decoder = global.decoder(subject)

// Manually encode without sending
case codec.encode(encoder, my_message) {
  Ok(bits) -> send_elsewhere(bits)
  Error(e) -> handle_error(e)
}
```

### Get the Underlying Subject

```gleam
let subject = global.subject(subject)

// Low-level operations (avoid if possible)
process.send(subject, raw_bits)
```

### Get the Owner Pid

```gleam
case global.owner(global) {
  Ok(pid) -> logger.info("Process: " <> string.inspect(pid))
  Error(Nil) -> logger.info("Unknown owner")
}
```

---

## Pattern: Typed Message Channels

Combining `GlobalSubject` with `GlobalReceiver` gives you **typed channels**:

```gleam
pub type UserCommand {
  CreateUser(name: String)
  DeleteUser(id: Int)
  GetUserCount
}

// Actor that handles user commands
pub fn user_service_actor() {
  let global = global.new(user_command_encoder(), user_command_decoder())
  registry.register(global, "user-service")
  
  loop_handler(global, users: [])
}

fn loop_handler(global: GlobalSubject(UserCommand), users: List(User)) {
  case global.receive(global, 10_000) {
    Ok(CreateUser(name)) -> {
      let new_users = [User(name: name), ..users]
      loop_handler(global, users: new_users)
    }
    Ok(DeleteUser(id)) -> {
      let new_users = list.filter(users, fn(u) { u.id != id })
      loop_handler(global, users: new_users)
    }
    Ok(GetUserCount) -> {
      // This pattern doesn't fit request/reply well
      // Use messaging.call_typed for RPC
      loop_handler(global, users)
    }
    Error(e) -> {
      logger.error("Handler error: " <> string.inspect(e))
      loop_handler(global, users)
    }
  }
}

// Send a command
pub fn create_user(name: String) -> Result(Nil, String) {
  use global <- result.try(
    registry.lookup("user-service")
    |> result.map_error(fn(_) { "Service not found" })
  )
  
  global.send(global, CreateUser(name))
  |> result.map_error(fn(e) { "Send failed: " <> string.inspect(e) })
}
```

---

## Error Handling Patterns

### Pattern 1: Fire-and-Forget

```gleam
// Don't care if it fails
let _ = global.send(global, my_message)
```

### Pattern 2: Log Errors

```gleam
case global.send(global, my_message) {
  Ok(Nil) -> Nil
  Error(e) -> logger.error("Send failed: " <> string.inspect(e))
}
```

### Pattern 3: Retry on Failure

```gleam
import distribute/retry

let policy = retry.default_with_jitter()

case retry.execute_with_strategy(policy, fn() {
  global.send(global, my_message)
  |> result.map_error(fn(_) { Nil })
}) {
  Ok(Nil) -> logger.info("Sent after retry")
  Error(Nil) -> logger.error("Send failed")
}
```

### Pattern 4: Timeout on Receive

```gleam
// With explicit timeout
case global.receive(global, 1000) {
  Ok(msg) -> handle(msg)
  Error(codec.DecodeTimeout) -> logger.warn("Timeout")
  Error(e) -> logger.error("Decode error: " <> string.inspect(e))
}
```

---

## Best Practices

1. **Always define encoder/decoder together** — They're paired for a reason

2. **Register globally** — GlobalSubject is meant to be found by name
   ```gleam
   let global = global.new(enc, dec)
   registry.register(global, "my-service")
   ```

3. **Use at actor boundaries** — GlobalSubject is the interface to your actor
   ```gleam
   pub fn start_my_actor() -> Result(GlobalSubject(MyMsg), Error) {
     let global = global.new(enc, dec)
     actor.start(global, initial_state)
   }
   ```

4. **Handle decode errors** — Messages from untrusted nodes may fail to decode
   ```gleam
   case global.receive(global, timeout) {
     Ok(msg) -> handle(msg)
     Error(e) -> logger.warn("Decode failed: " <> string.inspect(e))
   }
   ```

5. **Don't share raw subjects** — Use GlobalSubject, not `Subject(BitArray)`
   ```gleam
   // ❌ Bad
   pub fn get_raw_subject(g) { global.subject(g) }
   
   // ✅ Good
   pub fn send_msg(g, msg) { global.send(g, msg) }
   ```

6. **Use messaging.call_typed for RPC** — If you need request/reply
   ```gleam
   import distribute/messaging
   
   let response = messaging.call_typed(global, Request, 5000)
   ```

---

## Comparison: GlobalSubject vs Subject

| Aspect | `Subject(BitArray)` | `GlobalSubject(msg)` |
|--------|-------------------|----------------------|
| Type safety | ❌ No | ✅ Yes |
| Encode on send | ❌ Manual | ✅ Automatic |
| Decode on receive | ❌ Manual | ✅ Automatic |
| Registerable | ✅ Yes | ✅ Yes (recommended) |
| Error handling | ⚠️ Limited | ✅ Detailed errors |
| Cross-node | ✅ Yes | ✅ Yes |

---

## Next Steps

- [Registry](../registry/README.md) — How to register and look up global subjects
- [Messaging](../messaging/README.md) — Request/reply patterns and broadcasting
- [Codec](../codec/README.md) — Creating custom encoders/decoders

---

*For low-level process operations, see [gleam_erlang documentation](https://hexdocs.pm/gleam_erlang/).*
