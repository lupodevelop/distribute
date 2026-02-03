# Messaging Documentation

## Overview

The messaging module provides **cross-node communication** utilities. It handles:

- ✅ Sending messages to specific processes
- ✅ Sending messages to globally registered names
- ✅ Type-safe encoding/decoding before sending
- ✅ Batch sending with error aggregation
- ✅ Network error handling

### Core Principle

**All messaging is type-safe.** Messages are encoded before sending, ensuring the receiver can safely decode them.

---

## Quick Start

### Send to a Subject

```gleam
import distribute/messaging

// You have a subject from somewhere
let subject = process.unsafely_create_subject(pid, dynamic.nil())

// Send a message (type-safe)
case messaging.send_typed(subject, "hello", string_encoder()) {
  Ok(Nil) -> logger.info("Sent")
  Error(e) -> logger.error("Failed: " <> string.inspect(e))
}
```

### Send to a Named Service

```gleam
import distribute/messaging
import distribute/registry

// Look up a service by name
case registry.whereis_global("user-service", encoder(), decoder()) {
  Ok(service) -> {
    case messaging.send_global_typed("user-service", CreateUser(name), encoder()) {
      Ok(Nil) -> logger.info("Sent")
      Error(e) -> logger.error("Failed: " <> string.inspect(e))
    }
  }
  Error(Nil) -> logger.info("Service not found")
}
```

---

## Message Types

### SendError

```gleam
pub type SendError {
  NameNotFound(String)          // Registry lookup failed
  ProcessNotAlive               // Process crashed or not running
  NetworkError(String)          // Connection issue
  InvalidMessage(String)        // Message validation failed
  EncodeFailed(codec.EncodeError)  // Encoding failed
  SendFailed(String)            // Generic send failure
}
```

---

## Sending Patterns

### Pattern 1: One-Way Message (Fire and Forget)

```gleam
pub type NotificationMsg {
  UserCreated(user_id: Int)
  UserDeleted(user_id: Int)
}

// Send notification
case messaging.send_global_typed(
  "notifications",
  UserCreated(123),
  notification_encoder(),
) {
  Ok(Nil) -> Nil  // Don't care about response
  Error(_) -> Nil  // Ignore errors
}
```

**Use when:** You don't need a response or acknowledgement.

### Pattern 2: Request-Response (RPC-style)

```gleam
pub type UserRequest {
  GetUser(id: Int, reply: Subject(Result(User, String)))
}

pub type UserResponse {
  User(id: Int, name: String, email: String)
}

// Send request and wait for response
pub fn fetch_user(user_id: Int) -> Result(User, String) {
  case registry.whereis_global("user-service", user_req_encoder(), user_req_decoder()) {
    Ok(service) -> {
      let reply_subject = process.new_subject()
      
      case messaging.send_typed(
        global.subject(service),
        GetUser(user_id, reply_subject),
        user_req_encoder(),
      ) {
        Ok(Nil) -> {
          // Wait for response
          case process.receive(reply_subject, 5000) {
            Ok(result) -> result
            Error(Nil) -> Error("Timeout waiting for response")
          }
        }
        Error(e) -> Error("Send failed: " <> string.inspect(e))
      }
    }
    Error(Nil) -> Error("Service not found")
  }
}
```

**Use when:** You need a response from the remote process.

### Pattern 3: Broadcast to Many

```gleam
pub type BroadcastMsg {
  ServerShuttingDown
  ConfigUpdated
}

// Send to multiple services
pub fn broadcast_shutdown(service_names: List(String)) -> messaging.BatchSendResult {
  let messages = list.map(service_names, fn(name) {
    #(name, ServerShuttingDown)
  })
  
  messaging.send_batch_typed(messages, broadcast_encoder())
}
```

**Use when:** You need to notify multiple services.

---

## Error Handling

### Handling SendError

```gleam
case messaging.send_global_typed(name, msg, encoder) {
  Ok(Nil) -> {
    logger.info("Message sent")
    Ok(Nil)
  }
  Error(messaging.NameNotFound(name)) -> {
    logger.warn("Service not found: " <> name)
    Error("Service unavailable")
  }
  Error(messaging.ProcessNotAlive) -> {
    logger.warn("Remote process not alive")
    Error("Process died")
  }
  Error(messaging.NetworkError(reason)) -> {
    logger.error("Network error: " <> reason)
    Error("Network issue")
  }
  Error(messaging.InvalidMessage(msg)) -> {
    logger.error("Invalid message: " <> msg)
    Error("Bad message")
  }
  Error(messaging.EncodeFailed(e)) -> {
    logger.error("Encode failed: " <> string.inspect(e))
    Error("Encoding error")
  }
  Error(messaging.SendFailed(reason)) -> {
    logger.error("Send failed: " <> reason)
    Error("Send error")
  }
}
```

### With Retry

```gleam
import distribute/retry

pub fn send_with_retry(
  name: String,
  msg: msg_type,
  encoder: codec.Encoder(msg_type),
) -> Result(Nil, String) {
  let policy = retry.default_with_jitter()
  
  case retry.execute_with_strategy(policy, fn() {
    messaging.send_global_typed(name, msg, encoder)
    |> result.map_error(fn(_) { Nil })
  }) {
    Ok(Nil) -> {
      logger.info("Message sent after retry")
      Ok(Nil)
    }
    Error(Nil) -> {
      logger.error("Send failed after retries")
      Error("Max retries exceeded")
    }
  }
}
```

---

## Batch Sending

### Send Multiple Messages

```gleam
pub type UpdateMsg {
  UpdateConfig(key: String, value: String)
}

let messages = [
  #("service-1", UpdateConfig("debug", "true")),
  #("service-2", UpdateConfig("debug", "true")),
  #("service-3", UpdateConfig("debug", "true")),
]

let result = messaging.send_batch_typed(messages, update_msg_encoder())

// Check results
case result.failed {
  0 -> logger.info("All messages sent")
  n -> logger.warn("Failed: " <> int.to_string(n) <> " of " <> int.to_string(result.total))
}

// Log details if needed
list.each(result.errors, fn(err) {
  logger.error("Batch error: " <> string.inspect(err))
})
```

### Batch with Strict Mode

```gleam
case messaging.send_batch_strict(messages) {
  Ok(Nil) -> {
    logger.info("All messages sent")
    Ok(Nil)
  }
  Error(e) -> {
    logger.error("Batch failed: " <> string.inspect(e))
    Error("Batch send failed")
  }
}
```

---

## Common Patterns

### Pattern 1: Service Discovery + Send

```gleam
import distribute/registry
import distribute/messaging

pub fn notify_service(service_name: String, msg: Msg) -> Result(Nil, String) {
  // Look up service
  use service <- result.try(
    registry.whereis_global(service_name, encoder(), decoder())
    |> result.map_error(fn(_) { "Service not found: " <> service_name })
  )
  
  // Send message
  messaging.send_global_typed(service_name, msg, encoder())
  |> result.map_error(fn(e) { "Send failed: " <> string.inspect(e) })
}

// Usage
case notify_service("logger", LogEvent("user_login")) {
  Ok(Nil) -> logger.info("Logged")
  Error(e) -> logger.error(e)
}
```

### Pattern 2: Request/Reply with Timeout

```gleam
pub type RpcRequest {
  Compute(data: String, reply: Subject(Result(String, String)))
}

pub fn rpc_call(target: String, data: String) -> Result(String, String) {
  use service <- result.try(
    registry.whereis_global(target, encoder(), decoder())
    |> result.map_error(fn(_) { "Service not found" })
  )
  
  let reply = process.new_subject()
  
  use Nil <- result.try(
    messaging.send_typed(
      global.subject(service),
      Compute(data, reply),
      encoder(),
    )
    |> result.map_error(fn(e) { "Send failed: " <> string.inspect(e) })
  )
  
  // Wait for response with timeout
  case process.receive(reply, 3000) {
    Ok(result) -> result
    Error(Nil) -> Error("Timeout")
  }
}
```

### Pattern 3: Event Broadcasting

```gleam
pub type Event {
  UserRegistered(user_id: Int)
  UserDeleted(user_id: Int)
}

pub fn broadcast_event(event: Event) -> Result(Nil, String) {
  let subscribers = [
    "email-service",
    "analytics-service",
    "audit-service",
  ]
  
  let messages = list.map(subscribers, fn(name) {
    #(name, event)
  })
  
  let result = messaging.send_batch_typed(messages, event_encoder())
  
  case result.failed {
    0 -> {
      logger.info("Event broadcast complete")
      Ok(Nil)
    }
    n -> {
      logger.warn(
        "Event broadcast: " <> int.to_string(n) <> 
        " of " <> int.to_string(result.total) <> " failed"
      )
      // Decide if this is a failure
      Ok(Nil)  // or Error(...) depending on policy
    }
  }
}
```

---

## Best Practices

1. **Always handle errors**
   ```gleam
   // ✅ Good
   case messaging.send_global_typed(name, msg, encoder) {
     Ok(Nil) -> handle_success()
     Error(e) -> handle_error(e)
   }
   
   // ❌ Bad: Ignoring errors
   let _ = messaging.send_global_typed(name, msg, encoder)
   ```

2. **Use the right timeout for RPC**
   ```gleam
   // ✅ Good: Reasonable timeout
   case process.receive(reply, 5000) {
     Ok(result) -> result
     Error(Nil) -> Error("Timeout")
   }
   
   // ❌ Bad: Too short
   case process.receive(reply, 100) {
     Ok(result) -> result
     Error(Nil) -> Error("Timeout")
   }
   ```

3. **Log interesting send failures**
   ```gleam
   // ✅ Good
   case messaging.send_global_typed(name, msg, encoder) {
     Ok(Nil) -> Ok(Nil)
     Error(messaging.NameNotFound(n)) -> {
       logger.warn("Service not found: " <> n)
       Error("Not found")
     }
     Error(e) -> {
       logger.error("Send failed: " <> string.inspect(e))
       Error("Send error")
     }
   }
   ```

4. **Batch operations for efficiency**
   ```gleam
   // ✅ Good: One batch send
   messaging.send_batch_typed(
     list.map(services, fn(s) { #(s, msg) }),
     encoder
   )
   
   // ❌ Bad: Loop sending
   list.each(services, fn(s) {
     let _ = messaging.send_global_typed(s, msg, encoder)
   })
   ```

5. **Retry on transient failures**
   ```gleam
   import distribute/retry
   
   // ✅ Good: Retry with backoff
   let policy = retry.default_with_jitter()
   retry.execute_with_strategy(policy, fn() {
     messaging.send_global_typed(name, msg, encoder)
     |> result.map_error(fn(_) { Nil })
   })
   ```

---

## Deprecated APIs

| Old | New | Reason |
|-----|-----|--------|
| `send(pid, msg)` | `send_typed(subject, msg, encoder)` | Type safety |
| `send_global(name, msg)` | `send_global_typed(name, msg, encoder)` | Type safety |
| `send_batch(messages)` | `send_batch_typed(messages, encoder)` | Type safety |

See [MIGRATION.md](../../MIGRATION.md) for upgrade guide.

---

## Next Steps

- [Global Subjects](../global/README.md) — How to create subjects for messaging
- [Registry](../registry/README.md) — How to register and discover services
- [Actor](../actor/README.md) — How to build message-receiving actors
- [Retry](../retry/README.md) — How to retry failed sends with backoff

---

*See also: [gleam_erlang process documentation](https://hexdocs.pm/gleam_erlang/)*
