# Retry Documentation

## Overview

The retry module implements **exponential backoff with jitter** for distributed systems. It prevents the "thundering herd" problem where many clients retry simultaneously after a failure.

### Key Features

- ✅ Exponential backoff (customizable multiplier)
- ✅ Four jitter strategies (recommended: `FullJitter`)
- ✅ Configurable max delay cap
- ✅ Preset policies for common scenarios
- ✅ Fluent API for policy customization
- ✅ Observable delay metadata

---

## Quick Start

### Basic Retry Loop

```gleam
import distribute/retry
import gleam/erlang/process

pub fn call_with_retry(
  operation: fn() -> Result(a, String),
) -> Result(a, String) {
  let policy = retry.default_with_jitter()
  do_retry(operation, policy, attempt: 1)
}

fn do_retry(
  operation: fn() -> Result(a, String),
  policy: retry.RetryPolicy,
  attempt: Int,
) -> Result(a, String) {
  case operation() {
    Ok(result) -> Ok(result)
    Error(err) -> {
      case retry.should_retry(policy, attempt) {
        True -> {
          let delay = retry.calculate_delay(policy, attempt)
          logger.info(
            "Retry attempt " <> int.to_string(attempt) <>
            " after " <> int.to_string(delay.delay_ms) <> "ms"
          )
          process.sleep(delay.delay_ms)
          do_retry(operation, policy, attempt + 1)
        }
        False -> Error(err)
      }
    }
  }
}

// Usage
case call_with_retry(fn() { some_risky_operation() }) {
  Ok(result) -> handle_success(result)
  Error(e) -> handle_failure(e)
}
```

### Using execute_with_strategy

```gleam
import distribute/retry

let policy = retry.default_with_jitter()

case retry.execute_with_strategy(policy, fn() {
  risky_operation()
  |> result.map_error(fn(_) { Nil })
}) {
  Ok(result) -> logger.info("Success")
  Error(Nil) -> logger.error("Failed after retries")
}
```

---

## Jitter Strategies

Jitter is randomness added to retry delays to prevent the "thundering herd" problem.

### 1. NoJitter (Deterministic)

```
Delay(ms): 100 → 200 → 400 → 800 → ...
```

**Use:** Testing, debugging, or when deterministic behavior is required.

**Problem:** All clients retry at the same time after a shared failure.

```gleam
let policy = retry.default()
|> retry.with_jitter(retry.NoJitter)
```

### 2. FullJitter (Recommended) ✅

```
Delay(ms): random(0, 100) → random(0, 200) → random(0, 400) → ...
```

**Use:** Production systems (AWS recommended).

**Advantage:** Maximum spread prevents thundering herd.

**Algorithm:** `delay_ms = random(0, calculated_delay)`

```gleam
let policy = retry.default_with_jitter()  // FullJitter by default
```

### 3. EqualJitter (Balanced)

```
Delay(ms): (50 + random(0, 50)) → (100 + random(0, 100)) → ...
```

**Use:** When you need guaranteed minimum progress.

**Advantage:** Always makes some progress (half the calculated delay) plus randomness.

**Algorithm:** `delay_ms = calculated_delay/2 + random(0, calculated_delay/2)`

```gleam
let policy = retry.default_with_jitter()
|> retry.with_jitter(retry.EqualJitter)
```

### 4. DecorrelatedJitter (API Rate Limiting)

```
Delay(ms): random(100, 300) → random(100, 900) → random(100, 2700) → ...
```

**Use:** For APIs with strict rate limiting.

**Advantage:** Each retry is uncorrelated with the previous, creating natural variation.

**Algorithm:** `delay_ms = random(base_delay, previous_delay * 3)`

```gleam
let policy = retry.default_with_jitter()
|> retry.with_jitter(retry.DecorrelatedJitter)
```

---

## Policies

### Preset Policies

| Policy | Max Attempts | Base Delay | Max Delay | Jitter | Use Case |
|--------|--------------|-----------|-----------|--------|----------|
| `default()` | 3 | 100ms | 5000ms | None | Testing |
| `default_with_jitter()` | 3 | 100ms | 5000ms | FullJitter | **Most use cases** |
| `aggressive()` | 5 | 50ms | 3000ms | FullJitter | Critical operations |
| `conservative()` | 2 | 500ms | 10000ms | FullJitter | Non-critical |
| `no_retry()` | 1 | 0ms | 0ms | None | Single attempt |

### Creating Custom Policies

```gleam
import distribute/retry

// Start with a preset
let policy = retry.default_with_jitter()

// Customize with fluent API
let custom = policy
|> retry.with_max_attempts(5)
|> retry.with_base_delay_ms(200)
|> retry.with_max_delay_ms(10_000)
|> retry.with_multiplier(1.5)

// Or build from scratch
let my_policy = retry.RetryPolicy(
  max_attempts: 5,
  base_delay_ms: 100,
  max_delay_ms: 5000,
  multiplier: 2.0,
  jitter: retry.FullJitter,
)
```

---

## Core Functions

### calculate_delay

```gleam
pub fn calculate_delay(
  policy: retry.RetryPolicy,
  attempt: Int,
) -> retry.DelayResult
```

Calculates the delay for a given attempt number.

**Returns `DelayResult` with:**
- `delay_ms` — Actual delay to use (with jitter applied)
- `base_delay_ms` — Delay before jitter
- `attempt` — Attempt number (1-indexed)
- `is_final_attempt` — Whether this is the last attempt

```gleam
let policy = retry.default_with_jitter()
let result = retry.calculate_delay(policy, 1)

logger.info("Delay: " <> int.to_string(result.delay_ms) <> "ms")
logger.info("Base: " <> int.to_string(result.base_delay_ms) <> "ms")
logger.info("Attempt: " <> int.to_string(result.attempt))
logger.info("Final: " <> bool.to_string(result.is_final_attempt))
```

### delay_ms

```gleam
pub fn delay_ms(policy: retry.RetryPolicy, attempt: Int) -> Int
```

Convenience function returning just the delay value.

```gleam
let policy = retry.default_with_jitter()
let delay = retry.delay_ms(policy, attempt)
process.sleep(delay)
```

### should_retry

```gleam
pub fn should_retry(policy: retry.RetryPolicy, attempt: Int) -> Bool
```

Returns `True` if we should retry (attempt < max_attempts).

```gleam
case retry.should_retry(policy, attempt) {
  True -> {
    process.sleep(retry.delay_ms(policy, attempt))
    try_again(attempt + 1)
  }
  False -> Error("Max retries exceeded")
}
```

### execute_with_strategy

```gleam
pub fn execute_with_strategy(
  policy: retry.RetryPolicy,
  operation: fn() -> Result(a, b),
) -> Result(a, b)
```

Executes an operation with automatic retry logic.

```gleam
let policy = retry.aggressive()

case retry.execute_with_strategy(policy, fn() {
  some_operation()
  |> result.map_error(fn(_) { Nil })
}) {
  Ok(result) -> logger.info("Success")
  Error(Nil) -> logger.error("Failed")
}
```

---

## Patterns

### Pattern 1: Network Operation with Retry

```gleam
import distribute/retry

pub fn fetch_from_api(url: String) -> Result(String, String) {
  let policy = retry.default_with_jitter()
  
  case retry.execute_with_strategy(policy, fn() {
    http_client.get(url)
    |> result.map_error(fn(e) {
      logger.warn("HTTP failed: " <> string.inspect(e))
      Nil
    })
  }) {
    Ok(response) -> Ok(response.body)
    Error(Nil) -> Error("Failed after " <> int.to_string(policy.max_attempts) <> " attempts")
  }
}
```

### Pattern 2: Database Connection Retry

```gleam
import distribute/retry

pub fn connect_with_retry(db_url: String) -> Result(Connection, String) {
  let policy = retry.aggressive()  // More retries for db
  
  case retry.execute_with_strategy(policy, fn() {
    database.connect(db_url)
    |> result.map_error(fn(e) {
      logger.warn("DB connect failed: " <> string.inspect(e))
      Nil
    })
  }) {
    Ok(conn) -> {
      logger.info("Connected to database")
      Ok(conn)
    }
    Error(Nil) -> Error("Database connection failed")
  }
}
```

### Pattern 3: Registry Lookup with Retry

```gleam
import distribute/registry
import distribute/retry

pub fn lookup_service_with_retry(name: String) -> Result(ServiceHandle, String) {
  let policy = retry.default_with_jitter()
  
  case retry.execute_with_strategy(policy, fn() {
    registry.whereis_global(name, encoder(), decoder())
    |> result.map_error(fn(Nil) {
      logger.debug("Service not found, retrying...")
      Nil
    })
  }) {
    Ok(service) -> Ok(service)
    Error(Nil) -> Error("Service not found after retries: " <> name)
  }
}
```

### Pattern 4: Manual Retry Loop with Observability

```gleam
import distribute/retry
import gleam/erlang/process

pub fn operation_with_logging(name: String) -> Result(Value, String) {
  let policy = retry.default_with_jitter()
  
  fn loop(attempt: Int) -> Result(Value, String) {
    logger.info("Attempt " <> int.to_string(attempt))
    
    case risky_operation() {
      Ok(value) -> {
        logger.info("Success on attempt " <> int.to_string(attempt))
        Ok(value)
      }
      Error(err) -> {
        case retry.should_retry(policy, attempt) {
          True -> {
            let delay_result = retry.calculate_delay(policy, attempt)
            logger.warn(
              "Retry: " <> err <>
              " (waiting " <> int.to_string(delay_result.delay_ms) <> "ms)"
            )
            process.sleep(delay_result.delay_ms)
            loop(attempt + 1)
          }
          False -> {
            logger.error("Max retries (" <> int.to_string(policy.max_attempts) <> ") exceeded")
            Error("Max retries: " <> err)
          }
        }
      }
    }
  }
  
  loop(1)
}
```

### Pattern 5: Retry with Backoff in Registry

```gleam
import distribute/registry
import distribute/retry

pub fn register_with_strategy(
  global_subject: global.GlobalSubject(msg),
  name: String,
) -> Result(Nil, String) {
  let policy = retry.default_with_jitter()
  
  case retry.execute_with_strategy(policy, fn() {
    registry.register_global(global_subject, name)
    |> result.map_error(fn(e) {
      logger.warn("Registration failed: " <> string.inspect(e))
      Nil
    })
  }) {
    Ok(Nil) -> {
      logger.info("Registered: " <> name)
      Ok(Nil)
    }
    Error(Nil) -> Error("Failed to register: " <> name)
  }
}
```

---

## Best Practices

1. **Use FullJitter for production**
   ```gleam
   // ✅ Good
   let policy = retry.default_with_jitter()
   
   // ❌ Avoid in production
   let policy = retry.default()  // No jitter
   ```

2. **Log delays and attempts**
   ```gleam
   // ✅ Good
   let delay_result = retry.calculate_delay(policy, attempt)
   logger.info("Retrying in " <> int.to_string(delay_result.delay_ms) <> "ms")
   process.sleep(delay_result.delay_ms)
   
   // ❌ Bad: Silent retry
   let _ = retry.delay_ms(policy, attempt)
   process.sleep(...)
   ```

3. **Use appropriate policy for operation type**
   ```gleam
   // ✅ Good
   // Critical: aggressive retry
   let critical_policy = retry.aggressive()
   
   // Non-critical: conservative retry
   let normal_policy = retry.default_with_jitter()
   
   // One-time: no retry
   let once = retry.no_retry()
   ```

4. **Check is_final_attempt before logging**
   ```gleam
   // ✅ Good
   let delay_result = retry.calculate_delay(policy, attempt)
   case delay_result.is_final_attempt {
     True -> logger.error("Final attempt failed")
     False -> logger.warn("Retrying...")
   }
   
   // ❌ Bad
   logger.info("Attempt " <> int.to_string(attempt))
   ```

5. **Use execute_with_strategy for simple cases**
   ```gleam
   // ✅ Good: Simple
   retry.execute_with_strategy(policy, fn() {
     operation()
     |> result.map_error(fn(_) { Nil })
   })
   
   // ✅ Also fine: Manual loop for complex logic
   case operation() {
     Ok(x) -> Ok(x)
     Error(e) if should_retry -> {
       process.sleep(retry.delay_ms(policy, attempt))
       retry_operation(attempt + 1)
     }
     Error(e) -> Error(e)
   }
   ```

---

## Algorithm Reference

### Exponential Backoff

```
base_delay_ms × (multiplier ^ (attempt - 1))

Example (base=100, multiplier=2.0):
- Attempt 1: 100 × 2^0 = 100ms
- Attempt 2: 100 × 2^1 = 200ms
- Attempt 3: 100 × 2^2 = 400ms
- Attempt 4: 100 × 2^3 = 800ms (capped at max_delay_ms)
```

### Full Jitter

```
random(0, capped_delay_ms)

Example (capped=400ms):
- random(0, 400) → could be 127ms, 289ms, 45ms, ...
```

### Equal Jitter

```
(capped_delay_ms / 2) + random(0, capped_delay_ms / 2)

Example (capped=400ms):
- (200) + random(0, 200) → could be 200ms, 389ms, 245ms, ...
```

---

## Next Steps

- [Registry](../registry/README.md) — Registration with retry strategies
- [Messaging](../messaging/README.md) — Messaging with retry on send
- [Actor](../actor/README.md) — Supervised actors that auto-restart

---

## References

- [AWS: Exponential Backoff and Jitter](https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/)
- [Google Cloud: Retry Strategy](https://cloud.google.com/storage/docs/retry-strategy)
- [Thundering Herd Problem](https://en.wikipedia.org/wiki/Thundering_herd_problem)
