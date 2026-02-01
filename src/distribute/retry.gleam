//// Retry policy with exponential backoff and jitter.
////
//// This module provides type-safe retry logic for distributed operations.
//// It implements industry best practices for handling transient failures
//// in distributed systems.
////
//// ## Jitter Strategies
////
//// Jitter is crucial to prevent the "thundering herd" problem where many
//// clients retry simultaneously after a failure. This module supports
//// multiple jitter strategies as recommended by AWS and Google Cloud.
////
//// - `NoJitter`: Deterministic exponential backoff (not recommended)
//// - `FullJitter`: `random(0, delay)` - Best for reducing contention
//// - `EqualJitter`: `delay/2 + random(0, delay/2)` - Balanced approach
//// - `DecorrelatedJitter`: `random(base, prev_delay * 3)` - Good for APIs
////
//// ## References
////
//// - AWS: https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
//// - Google Cloud: https://cloud.google.com/storage/docs/retry-strategy
////
//// ## Example
////
//// ```gleam
//// import distribute/retry
////
//// // Create a policy with full jitter (recommended)
//// let policy = retry.default_with_jitter()
////
//// // Calculate delay for attempt 3
//// let delay = retry.calculate_delay(policy, 3)
//// // delay will be random(0, min(5000, 100 * 2^2)) = random(0, 400)
//// ```

import gleam/int

// ============================================================================
// Types
// ============================================================================

/// Jitter strategy for randomizing retry delays.
///
/// Jitter helps prevent the "thundering herd" problem where many clients
/// retry simultaneously after a shared failure.
pub type JitterStrategy {
  /// No jitter - deterministic delays. NOT recommended for production.
  /// Use only for testing or when deterministic behavior is required.
  NoJitter

  /// Full jitter: `random(0, calculated_delay)`
  /// Recommended for most use cases. Provides maximum spread of retries.
  /// Reference: AWS Architecture Blog
  FullJitter

  /// Equal jitter: `calculated_delay/2 + random(0, calculated_delay/2)`
  /// Guarantees minimum delay while still adding randomness.
  /// Good when you need both progress guarantee and jitter.
  EqualJitter

  /// Decorrelated jitter: `random(base_delay, previous_delay * 3)`
  /// Each delay is based on the previous, creating natural variation.
  /// Good for API rate limiting scenarios.
  DecorrelatedJitter
}

/// Retry policy configuration.
///
/// Controls how failed operations are retried with exponential backoff
/// and optional jitter for distributed systems.
///
/// ## Fields
///
/// - `max_attempts`: Maximum number of attempts (1 = no retry)
/// - `base_delay_ms`: Initial delay in milliseconds before jitter
/// - `max_delay_ms`: Maximum delay to prevent runaway waits
/// - `multiplier`: Backoff multiplier (typically 2.0 for exponential)
/// - `jitter`: Jitter strategy for randomizing delays
///
/// ## Example
///
/// ```gleam
/// // Custom policy for critical operations
/// let policy = RetryPolicy(
///   max_attempts: 5,
///   base_delay_ms: 50,
///   max_delay_ms: 10_000,
///   multiplier: 2.0,
///   jitter: FullJitter,
/// )
/// ```
pub type RetryPolicy {
  RetryPolicy(
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    multiplier: Float,
    jitter: JitterStrategy,
  )
}

/// Result of a delay calculation, includes metadata for observability.
pub type DelayResult {
  DelayResult(
    /// The actual delay to use (in milliseconds)
    delay_ms: Int,
    /// The base delay before jitter was applied
    base_delay_ms: Int,
    /// Current attempt number (1-indexed)
    attempt: Int,
    /// Whether this is the last attempt
    is_final_attempt: Bool,
  )
}

// ============================================================================
// Constructors
// ============================================================================

/// Default retry policy without jitter.
///
/// Conservative settings suitable for general use:
/// - 3 attempts total
/// - 100ms base delay
/// - 5000ms max delay
/// - 2.0 multiplier (exponential)
/// - No jitter
///
/// For distributed systems, prefer `default_with_jitter()`.
pub fn default() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 3,
    base_delay_ms: 100,
    max_delay_ms: 5000,
    multiplier: 2.0,
    jitter: NoJitter,
  )
}

/// Default retry policy with full jitter (RECOMMENDED).
///
/// Same as `default()` but with FullJitter enabled.
/// This is the recommended policy for distributed systems to prevent
/// thundering herd problems.
///
/// Delay progression (example, actual values are randomized):
/// - Attempt 1: random(0, 100ms)
/// - Attempt 2: random(0, 200ms)
/// - Attempt 3: random(0, 400ms)
pub fn default_with_jitter() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 3,
    base_delay_ms: 100,
    max_delay_ms: 5000,
    multiplier: 2.0,
    jitter: FullJitter,
  )
}

/// No retry policy (single attempt only).
///
/// Use when retry is handled at a different layer or for operations
/// that should not be retried (e.g., non-idempotent operations).
pub fn no_retry() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 1,
    base_delay_ms: 0,
    max_delay_ms: 0,
    multiplier: 1.0,
    jitter: NoJitter,
  )
}

/// Aggressive retry policy for critical operations.
///
/// More retries with shorter base delays:
/// - 5 attempts total
/// - 50ms base delay
/// - 3000ms max delay
/// - Full jitter enabled
pub fn aggressive() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 5,
    base_delay_ms: 50,
    max_delay_ms: 3000,
    multiplier: 2.0,
    jitter: FullJitter,
  )
}

/// Conservative retry policy for non-critical operations.
///
/// Fewer retries with longer delays:
/// - 2 attempts total
/// - 500ms base delay
/// - 10000ms max delay
/// - Full jitter enabled
pub fn conservative() -> RetryPolicy {
  RetryPolicy(
    max_attempts: 2,
    base_delay_ms: 500,
    max_delay_ms: 10_000,
    multiplier: 2.0,
    jitter: FullJitter,
  )
}

// ============================================================================
// Policy Builders (Fluent API)
// ============================================================================

/// Set the maximum number of retry attempts.
///
/// ```gleam
/// retry.default()
/// |> retry.with_max_attempts(5)
/// ```
pub fn with_max_attempts(policy: RetryPolicy, attempts: Int) -> RetryPolicy {
  RetryPolicy(..policy, max_attempts: int.max(1, attempts))
}

/// Set the base delay in milliseconds.
///
/// ```gleam
/// retry.default()
/// |> retry.with_base_delay_ms(200)
/// ```
pub fn with_base_delay_ms(policy: RetryPolicy, delay_ms: Int) -> RetryPolicy {
  RetryPolicy(..policy, base_delay_ms: int.max(0, delay_ms))
}

/// Set the maximum delay cap in milliseconds.
///
/// ```gleam
/// retry.default()
/// |> retry.with_max_delay_ms(10_000)
/// ```
pub fn with_max_delay_ms(policy: RetryPolicy, delay_ms: Int) -> RetryPolicy {
  RetryPolicy(..policy, max_delay_ms: int.max(0, delay_ms))
}

/// Set the backoff multiplier.
///
/// ```gleam
/// retry.default()
/// |> retry.with_multiplier(1.5)  // Slower growth
/// ```
pub fn with_multiplier(policy: RetryPolicy, multiplier: Float) -> RetryPolicy {
  let safe_multiplier = case multiplier <. 1.0 {
    True -> 1.0
    False -> multiplier
  }
  RetryPolicy(..policy, multiplier: safe_multiplier)
}

/// Set the jitter strategy.
///
/// ```gleam
/// retry.default()
/// |> retry.with_jitter(retry.FullJitter)
/// ```
pub fn with_jitter(policy: RetryPolicy, jitter: JitterStrategy) -> RetryPolicy {
  RetryPolicy(..policy, jitter: jitter)
}

/// Enable full jitter (convenience method).
///
/// ```gleam
/// retry.default()
/// |> retry.with_full_jitter()
/// ```
pub fn with_full_jitter(policy: RetryPolicy) -> RetryPolicy {
  with_jitter(policy, FullJitter)
}

// ============================================================================
// Core Functions
// ============================================================================

/// Calculate the delay for a given attempt number.
///
/// Returns a `DelayResult` containing the delay in milliseconds and metadata.
/// The attempt number should be 1-indexed (first retry is attempt 1).
///
/// ## Algorithm
///
/// 1. Calculate base exponential delay: `base_delay * (multiplier ^ (attempt - 1))`
/// 2. Cap at `max_delay_ms`
/// 3. Apply jitter strategy
///
/// ## Example
///
/// ```gleam
/// let policy = retry.default_with_jitter()
/// let result = retry.calculate_delay(policy, 1)
/// // result.delay_ms will be random(0, 100)
/// process.sleep(result.delay_ms)
/// ```
pub fn calculate_delay(policy: RetryPolicy, attempt: Int) -> DelayResult {
  let safe_attempt = int.max(1, attempt)

  // Calculate exponential delay
  let base = int.to_float(policy.base_delay_ms)
  let exponent = int.to_float(safe_attempt - 1)
  let multiplier_pow = float_power(policy.multiplier, exponent)
  let delay_float = base *. multiplier_pow
  let exponential_delay = float_truncate(delay_float)

  // Cap at max delay
  let capped_delay = int.min(exponential_delay, policy.max_delay_ms)

  // Apply jitter
  let final_delay =
    apply_jitter(policy.jitter, capped_delay, policy.base_delay_ms)

  DelayResult(
    delay_ms: final_delay,
    base_delay_ms: capped_delay,
    attempt: safe_attempt,
    is_final_attempt: safe_attempt >= policy.max_attempts,
  )
}

/// Get just the delay value in milliseconds.
///
/// Convenience function when you don't need the full DelayResult.
///
/// ```gleam
/// let delay = retry.delay_ms(policy, attempt)
/// process.sleep(delay)
/// ```
pub fn delay_ms(policy: RetryPolicy, attempt: Int) -> Int {
  calculate_delay(policy, attempt).delay_ms
}

/// Check if we should retry after this attempt.
///
/// Returns True if `attempt < max_attempts`.
///
/// ```gleam
/// case retry.should_retry(policy, attempt) {
///   True -> {
///     process.sleep(retry.delay_ms(policy, attempt))
///     try_operation(attempt + 1)
///   }
///   False -> Error(MaxRetriesExceeded)
/// }
/// ```
pub fn should_retry(policy: RetryPolicy, attempt: Int) -> Bool {
  attempt < policy.max_attempts
}

/// Check if this is the final attempt.
///
/// ```gleam
/// case retry.is_final_attempt(policy, attempt) {
///   True -> log.error("Final attempt, no more retries")
///   False -> log.warn("Retrying...")
/// }
/// ```
pub fn is_final_attempt(policy: RetryPolicy, attempt: Int) -> Bool {
  attempt >= policy.max_attempts
}

/// Get the total number of attempts that will be made.
///
/// ```gleam
/// let total = retry.total_attempts(policy)
/// log.info("Will try up to " <> int.to_string(total) <> " times")
/// ```
pub fn total_attempts(policy: RetryPolicy) -> Int {
  policy.max_attempts
}

// ============================================================================
// Jitter Implementation
// ============================================================================

/// Apply jitter strategy to a delay value.
fn apply_jitter(
  strategy: JitterStrategy,
  delay_ms: Int,
  base_delay_ms: Int,
) -> Int {
  case strategy {
    NoJitter -> delay_ms

    FullJitter -> {
      // random(0, delay)
      case delay_ms <= 0 {
        True -> 0
        False -> random_int(0, delay_ms)
      }
    }

    EqualJitter -> {
      // delay/2 + random(0, delay/2)
      let half = delay_ms / 2
      case half <= 0 {
        True -> delay_ms
        False -> half + random_int(0, half)
      }
    }

    DecorrelatedJitter -> {
      // random(base, delay * 3)
      // Capped to prevent runaway
      let upper = int.min(delay_ms * 3, delay_ms + base_delay_ms * 10)
      case upper <= base_delay_ms {
        True -> base_delay_ms
        False -> random_int(base_delay_ms, upper)
      }
    }
  }
}

// ============================================================================
// Helpers
// ============================================================================

/// Generate a random integer in range [min, max] (inclusive).
fn random_int(min: Int, max: Int) -> Int {
  case max <= min {
    True -> min
    False -> min + do_uniform(max - min + 1) - 1
  }
}

/// Erlang's rand:uniform/1 returns 1..N
@external(erlang, "rand", "uniform")
fn do_uniform(n: Int) -> Int

/// Float power helper for small integer exponents.
fn float_power(base: Float, exponent: Float) -> Float {
  let exp_int = float_truncate(exponent)
  case exp_int {
    0 -> 1.0
    1 -> base
    2 -> base *. base
    3 -> base *. base *. base
    4 -> base *. base *. base *. base
    5 -> base *. base *. base *. base *. base
    6 -> base *. base *. base *. base *. base *. base
    7 -> base *. base *. base *. base *. base *. base *. base
    8 -> base *. base *. base *. base *. base *. base *. base *. base
    // For exponents > 8, cap the growth
    _ -> base *. base *. base *. base *. base *. base *. base *. base
  }
}

/// Truncate float to integer.
@external(erlang, "erlang", "trunc")
fn float_truncate(f: Float) -> Int

// ============================================================================
// Conversion helpers for logging/debugging
// ============================================================================

/// Convert jitter strategy to string for logging.
pub fn jitter_to_string(jitter: JitterStrategy) -> String {
  case jitter {
    NoJitter -> "none"
    FullJitter -> "full"
    EqualJitter -> "equal"
    DecorrelatedJitter -> "decorrelated"
  }
}

/// Convert retry policy to a loggable string representation.
pub fn policy_to_string(policy: RetryPolicy) -> String {
  "RetryPolicy(max_attempts="
  <> int.to_string(policy.max_attempts)
  <> ", base_delay_ms="
  <> int.to_string(policy.base_delay_ms)
  <> ", max_delay_ms="
  <> int.to_string(policy.max_delay_ms)
  <> ", jitter="
  <> jitter_to_string(policy.jitter)
  <> ")"
}
