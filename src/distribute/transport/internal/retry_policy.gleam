//// Retry policy implementation with exponential backoff.
////
//// This module provides type-safe retry logic for transport operations.
//// It's used internally by transport adapters to handle transient failures.

import gleam/int

/// Retry policy configuration.
///
/// Controls how failed operations are retried with exponential backoff.
///
/// ## Fields
///
/// - `max_attempts`: Maximum number of attempts (1 = no retry)
/// - `base_delay_ms`: Initial delay in milliseconds
/// - `max_delay_ms`: Maximum delay to prevent runaway waits
/// - `multiplier`: Backoff multiplier (typically 2.0 for exponential)
pub type RetryPolicy {
  RetryPolicy(
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    multiplier: Float,
  )
}

/// Default retry policy.
///
/// Conservative settings:
/// - 3 attempts
/// - 100ms base delay
/// - 5000ms max delay
/// - 2.0 multiplier (exponential)
pub fn default() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 3,
    base_delay_ms: 100,
    max_delay_ms: 5000,
    multiplier: 2.0,
  )
}

/// No retry policy (single attempt only).
pub fn no_retry() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 1,
    base_delay_ms: 0,
    max_delay_ms: 0,
    multiplier: 1.0,
  )
}

/// Aggressive retry policy for critical operations.
///
/// - 5 attempts
/// - 50ms base delay
/// - 3000ms max delay
pub fn aggressive() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 5,
    base_delay_ms: 50,
    max_delay_ms: 3000,
    multiplier: 2.0,
  )
}

/// Calculate delay for a given attempt number.
///
/// Uses exponential backoff: delay = base * (multiplier ^ attempt)
/// Capped at max_delay_ms to prevent excessive waits.
pub fn calculate_delay(policy: RetryPolicy, attempt: Int) -> Int {
  let base = int.to_float(policy.base_delay_ms)
  let exponent = int.to_float(attempt - 1)
  let multiplier_pow = float_power(policy.multiplier, exponent)
  let delay_float = base *. multiplier_pow
  let delay = float_truncate(delay_float)

  int.min(delay, policy.max_delay_ms)
}

/// Should we retry after this attempt?
pub fn should_retry(policy: RetryPolicy, attempt: Int) -> Bool {
  attempt < policy.max_attempts
}

// Float power helper (since Gleam stdlib doesn't have it)
fn float_power(base: Float, exponent: Float) -> Float {
  case exponent {
    0.0 -> 1.0
    1.0 -> base
    _ -> {
      // For small integer exponents, use multiplication
      let exp_int = float_truncate(exponent)
      case exp_int {
        0 -> 1.0
        1 -> base
        2 -> base *. base
        3 -> base *. base *. base
        4 -> base *. base *. base *. base
        _ -> base *. base *. base *. base // Cap at 4
      }
    }
  }
}

fn float_truncate(f: Float) -> Int {
  // Use Erlang's trunc BIF via erlang FFI
  do_truncate(f)
}

@external(erlang, "erlang", "trunc")
fn do_truncate(f: Float) -> Int
