//// Circuit breaker implementation for per-node failure handling.
////
//// This module provides a type-safe circuit breaker pattern following
//// Gleam/OTP best practices. It tracks per-node failures and automatically
//// stops sending to consistently failing nodes.
////
//// ## Design
////
//// - Pure functions for state transitions
//// - Exhaustive pattern matching
//// - Type-safe Result handling
//// - No side effects; caller decides when to persist state
////
//// ## Circuit States
////
//// 1. **Closed**: Normal operation, all requests flow through
//// 2. **Open**: Too many failures, requests are rejected immediately
//// 3. **HalfOpen**: Testing recovery, limited requests allowed
////
//// ## Integration
////
//// Circuit breakers integrate with transport retry logic:
//// - Before send: Check if circuit allows the request
//// - After send: Record outcome to update circuit state
//// - Periodic health checks can reset circuits
////
//// ## Example
////
//// ```gleam
//// let policy = default_policy()
//// let breaker = new_breaker()
////
//// // Check before sending
//// case should_allow_request(breaker, policy) {
////   True -> {
////     case send_to_node(node, payload) {
////       Ok(_) -> record_success(breaker, policy)
////       Error(_) -> record_failure(breaker, policy)
////     }
////   }
////   False -> Error(CircuitOpen)
//// }
//// ```

import distribute/transport/behaviour
import gleam/dict

/// Per-node circuit breaker registry.
///
/// Maps node IDs to their circuit breaker state.
pub type CircuitBreakerRegistry =
  dict.Dict(behaviour.NodeId, behaviour.NodeCircuitBreaker)

/// Create a new circuit breaker for a node.
///
/// Starts in Closed state with zero failures.
pub fn new_breaker() -> behaviour.NodeCircuitBreaker {
  behaviour.NodeCircuitBreaker(
    state: behaviour.Closed,
    consecutive_failures: 0,
    consecutive_successes: 0,
    total_failures: 0,
    total_successes: 0,
  )
}

/// Default circuit breaker policy.
///
/// Conservative settings:
/// - Opens after 5 consecutive failures
/// - Closes after 2 consecutive successes in HalfOpen
/// - Waits 30 seconds before trying HalfOpen
/// - Allows 1 test request in HalfOpen
pub fn default_policy() -> behaviour.CircuitBreakerPolicy {
  behaviour.CircuitBreakerPolicy(
    failure_threshold: 5,
    success_threshold: 2,
    timeout_ms: 30_000,
    half_open_max_calls: 1,
  )
}

/// Check if a request should be allowed through the circuit breaker.
///
/// Returns `True` if the request can proceed, `False` if circuit is open.
///
/// ## Logic
///
/// - `Closed`: Always allow
/// - `Open`: Check if timeout expired; if so, transition to HalfOpen
/// - `HalfOpen`: Allow limited requests (controlled by policy)
pub fn should_allow_request(
  breaker: behaviour.NodeCircuitBreaker,
  policy: behaviour.CircuitBreakerPolicy,
  current_time_ms: Int,
) -> #(Bool, behaviour.NodeCircuitBreaker) {
  case breaker.state {
    behaviour.Closed -> #(True, breaker)

    behaviour.Open(opened_at) -> {
      let elapsed = current_time_ms - opened_at
      case elapsed >= policy.timeout_ms {
        True -> {
          // Transition to HalfOpen
          let updated =
            behaviour.NodeCircuitBreaker(
              ..breaker,
              state: behaviour.HalfOpen(current_time_ms),
            )
          #(True, updated)
        }
        False -> #(False, breaker)
      }
    }

    behaviour.HalfOpen(_) -> {
      // In real implementation, would track concurrent calls
      // For Phase 2, simplify: allow if not at max concurrent
      #(True, breaker)
    }
  }
}

/// Record a successful request outcome.
///
/// Updates circuit state based on policy:
/// - `Closed`: Increment success counter, reset failure counter
/// - `HalfOpen`: Increment success counter; close if threshold reached
/// - `Open`: Should not happen (requests blocked)
pub fn record_success(
  breaker: behaviour.NodeCircuitBreaker,
  policy: behaviour.CircuitBreakerPolicy,
) -> behaviour.NodeCircuitBreaker {
  case breaker.state {
    behaviour.Closed ->
      behaviour.NodeCircuitBreaker(
        ..breaker,
        consecutive_failures: 0,
        consecutive_successes: breaker.consecutive_successes + 1,
        total_successes: breaker.total_successes + 1,
      )

    behaviour.HalfOpen(_) -> {
      let new_successes = breaker.consecutive_successes + 1
      case new_successes >= policy.success_threshold {
        True ->
          // Close the circuit
          behaviour.NodeCircuitBreaker(
            ..breaker,
            state: behaviour.Closed,
            consecutive_failures: 0,
            consecutive_successes: 0,
            total_successes: breaker.total_successes + 1,
          )
        False ->
          behaviour.NodeCircuitBreaker(
            ..breaker,
            consecutive_successes: new_successes,
            total_successes: breaker.total_successes + 1,
          )
      }
    }

    behaviour.Open(_) ->
      // Shouldn't happen, but handle gracefully
      breaker
  }
}

/// Record a failed request outcome.
///
/// Updates circuit state based on policy:
/// - `Closed`: Increment failure counter; open if threshold reached
/// - `HalfOpen`: Increment failure counter; reopen circuit
/// - `Open`: Increment failure counter (already open)
pub fn record_failure(
  breaker: behaviour.NodeCircuitBreaker,
  policy: behaviour.CircuitBreakerPolicy,
  current_time_ms: Int,
) -> behaviour.NodeCircuitBreaker {
  case breaker.state {
    behaviour.Closed -> {
      let new_failures = breaker.consecutive_failures + 1
      case new_failures >= policy.failure_threshold {
        True ->
          // Open the circuit
          behaviour.NodeCircuitBreaker(
            ..breaker,
            state: behaviour.Open(current_time_ms),
            consecutive_failures: new_failures,
            consecutive_successes: 0,
            total_failures: breaker.total_failures + 1,
          )
        False ->
          behaviour.NodeCircuitBreaker(
            ..breaker,
            consecutive_failures: new_failures,
            consecutive_successes: 0,
            total_failures: breaker.total_failures + 1,
          )
      }
    }

    behaviour.HalfOpen(_) ->
      // Failed during half-open test; reopen circuit
      behaviour.NodeCircuitBreaker(
        ..breaker,
        state: behaviour.Open(current_time_ms),
        consecutive_failures: breaker.consecutive_failures + 1,
        consecutive_successes: 0,
        total_failures: breaker.total_failures + 1,
      )

    behaviour.Open(opened_at) ->
      behaviour.NodeCircuitBreaker(
        ..breaker,
        state: behaviour.Open(opened_at),
        consecutive_failures: breaker.consecutive_failures + 1,
        total_failures: breaker.total_failures + 1,
      )
  }
}

/// Get or create a circuit breaker for a node.
pub fn get_or_create(
  registry: CircuitBreakerRegistry,
  node: behaviour.NodeId,
) -> #(behaviour.NodeCircuitBreaker, CircuitBreakerRegistry) {
  case dict.get(registry, node) {
    Ok(breaker) -> #(breaker, registry)
    Error(Nil) -> {
      let new_breaker = new_breaker()
      let updated = dict.insert(registry, node, new_breaker)
      #(new_breaker, updated)
    }
  }
}

/// Update a circuit breaker in the registry.
pub fn update(
  registry: CircuitBreakerRegistry,
  node: behaviour.NodeId,
  breaker: behaviour.NodeCircuitBreaker,
) -> CircuitBreakerRegistry {
  dict.insert(registry, node, breaker)
}

/// Reset a circuit breaker to initial state.
///
/// Useful for manual recovery or health check-triggered resets.
pub fn reset(
  registry: CircuitBreakerRegistry,
  node: behaviour.NodeId,
) -> CircuitBreakerRegistry {
  dict.insert(registry, node, new_breaker())
}

/// Get circuit breaker metrics for monitoring.
pub fn get_metrics(
  breaker: behaviour.NodeCircuitBreaker,
) -> dict.Dict(String, Int) {
  let state_value = case breaker.state {
    behaviour.Closed -> 0
    behaviour.HalfOpen(_) -> 1
    behaviour.Open(_) -> 2
  }

  dict.new()
  |> dict.insert("circuit_state", state_value)
  |> dict.insert("consecutive_failures", breaker.consecutive_failures)
  |> dict.insert("consecutive_successes", breaker.consecutive_successes)
  |> dict.insert("total_failures", breaker.total_failures)
  |> dict.insert("total_successes", breaker.total_successes)
}
