//// Tests for the retry module with exponential backoff and jitter.
////
//// These tests verify:
//// - Exponential delay calculation
//// - Jitter strategies produce valid ranges
//// - Policy builders work correctly
//// - Edge cases and boundary conditions

import distribute/retry
import gleam/int
import gleam/list
import gleeunit/should

// ============================================================================
// Default Policy Tests
// ============================================================================

pub fn default_policy_has_expected_values_test() {
  let policy = retry.default()

  policy.max_attempts
  |> should.equal(3)

  policy.base_delay_ms
  |> should.equal(100)

  policy.max_delay_ms
  |> should.equal(5000)

  policy.jitter
  |> should.equal(retry.NoJitter)
}

pub fn default_with_jitter_has_full_jitter_test() {
  let policy = retry.default_with_jitter()

  policy.max_attempts
  |> should.equal(3)

  policy.jitter
  |> should.equal(retry.FullJitter)
}

pub fn no_retry_policy_has_single_attempt_test() {
  let policy = retry.no_retry()

  policy.max_attempts
  |> should.equal(1)

  retry.should_retry(policy, 1)
  |> should.be_false()
}

pub fn aggressive_policy_has_more_attempts_test() {
  let policy = retry.aggressive()

  policy.max_attempts
  |> should.equal(5)

  policy.base_delay_ms
  |> should.equal(50)

  policy.jitter
  |> should.equal(retry.FullJitter)
}

pub fn conservative_policy_has_longer_delays_test() {
  let policy = retry.conservative()

  policy.max_attempts
  |> should.equal(2)

  policy.base_delay_ms
  |> should.equal(500)

  policy.max_delay_ms
  |> should.equal(10_000)
}

// ============================================================================
// Policy Builder Tests (Fluent API)
// ============================================================================

pub fn with_max_attempts_sets_attempts_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(10)

  policy.max_attempts
  |> should.equal(10)
}

pub fn with_max_attempts_enforces_minimum_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(0)

  // Should enforce minimum of 1
  policy.max_attempts
  |> should.equal(1)
}

pub fn with_base_delay_ms_sets_delay_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(500)

  policy.base_delay_ms
  |> should.equal(500)
}

pub fn with_max_delay_ms_sets_cap_test() {
  let policy =
    retry.default()
    |> retry.with_max_delay_ms(10_000)

  policy.max_delay_ms
  |> should.equal(10_000)
}

pub fn with_multiplier_sets_multiplier_test() {
  let policy =
    retry.default()
    |> retry.with_multiplier(1.5)

  policy.multiplier
  |> should.equal(1.5)
}

pub fn with_multiplier_enforces_minimum_test() {
  let policy =
    retry.default()
    |> retry.with_multiplier(0.5)

  // Should enforce minimum of 1.0
  policy.multiplier
  |> should.equal(1.0)
}

pub fn with_jitter_sets_strategy_test() {
  let policy =
    retry.default()
    |> retry.with_jitter(retry.EqualJitter)

  policy.jitter
  |> should.equal(retry.EqualJitter)
}

pub fn with_full_jitter_convenience_test() {
  let policy =
    retry.default()
    |> retry.with_full_jitter()

  policy.jitter
  |> should.equal(retry.FullJitter)
}

pub fn fluent_api_chains_correctly_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(5)
    |> retry.with_base_delay_ms(200)
    |> retry.with_max_delay_ms(8000)
    |> retry.with_multiplier(2.5)
    |> retry.with_jitter(retry.DecorrelatedJitter)

  policy.max_attempts
  |> should.equal(5)

  policy.base_delay_ms
  |> should.equal(200)

  policy.max_delay_ms
  |> should.equal(8000)

  policy.jitter
  |> should.equal(retry.DecorrelatedJitter)
}

// ============================================================================
// Exponential Delay Calculation Tests (No Jitter)
// ============================================================================

pub fn calculate_delay_exponential_growth_test() {
  // Policy without jitter for deterministic testing
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_max_delay_ms(10_000)
    |> retry.with_multiplier(2.0)
    |> retry.with_jitter(retry.NoJitter)

  // Attempt 1: 100 * 2^0 = 100
  let result1 = retry.calculate_delay(policy, 1)
  result1.delay_ms
  |> should.equal(100)

  result1.attempt
  |> should.equal(1)

  // Attempt 2: 100 * 2^1 = 200
  let result2 = retry.calculate_delay(policy, 2)
  result2.delay_ms
  |> should.equal(200)

  // Attempt 3: 100 * 2^2 = 400
  let result3 = retry.calculate_delay(policy, 3)
  result3.delay_ms
  |> should.equal(400)

  // Attempt 4: 100 * 2^3 = 800
  let result4 = retry.calculate_delay(policy, 4)
  result4.delay_ms
  |> should.equal(800)

  // Attempt 5: 100 * 2^4 = 1600
  let result5 = retry.calculate_delay(policy, 5)
  result5.delay_ms
  |> should.equal(1600)
}

pub fn calculate_delay_respects_max_cap_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(1000)
    |> retry.with_max_delay_ms(3000)
    |> retry.with_multiplier(2.0)
    |> retry.with_jitter(retry.NoJitter)

  // Attempt 1: 1000
  let result1 = retry.calculate_delay(policy, 1)
  result1.delay_ms
  |> should.equal(1000)

  // Attempt 2: 2000
  let result2 = retry.calculate_delay(policy, 2)
  result2.delay_ms
  |> should.equal(2000)

  // Attempt 3: 4000 -> capped to 3000
  let result3 = retry.calculate_delay(policy, 3)
  result3.delay_ms
  |> should.equal(3000)

  // Attempt 4: 8000 -> capped to 3000
  let result4 = retry.calculate_delay(policy, 4)
  result4.delay_ms
  |> should.equal(3000)
}

pub fn delay_ms_convenience_function_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_jitter(retry.NoJitter)

  retry.delay_ms(policy, 1)
  |> should.equal(100)

  retry.delay_ms(policy, 2)
  |> should.equal(200)
}

// ============================================================================
// Full Jitter Tests
// ============================================================================

pub fn full_jitter_produces_values_in_range_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_max_delay_ms(10_000)
    |> retry.with_jitter(retry.FullJitter)

  // Run multiple times to test randomness
  let results =
    list.range(1, 100)
    |> list.map(fn(_) { retry.calculate_delay(policy, 1) })

  // All delays should be in range [0, 100]
  results
  |> list.all(fn(r) { r.delay_ms >= 0 && r.delay_ms <= 100 })
  |> should.be_true()

  // Base delay should still be 100
  results
  |> list.all(fn(r) { r.base_delay_ms == 100 })
  |> should.be_true()
}

pub fn full_jitter_at_higher_attempt_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_max_delay_ms(10_000)
    |> retry.with_jitter(retry.FullJitter)

  // Attempt 4: base delay is 100 * 2^3 = 800
  // Jitter should produce values in [0, 800]
  let results =
    list.range(1, 100)
    |> list.map(fn(_) { retry.calculate_delay(policy, 4) })

  results
  |> list.all(fn(r) { r.delay_ms >= 0 && r.delay_ms <= 800 })
  |> should.be_true()

  results
  |> list.all(fn(r) { r.base_delay_ms == 800 })
  |> should.be_true()
}

pub fn full_jitter_produces_varied_values_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(1000)
    |> retry.with_jitter(retry.FullJitter)

  // Generate many samples
  let delays =
    list.range(1, 50)
    |> list.map(fn(_) { retry.delay_ms(policy, 1) })

  // Check that we get some variation (not all the same value)
  let unique_count = count_unique(delays)

  // With 50 samples from range [0, 1000], we should get at least 5 unique values
  { unique_count >= 5 }
  |> should.be_true()
}

// ============================================================================
// Equal Jitter Tests
// ============================================================================

pub fn equal_jitter_produces_values_in_upper_half_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_jitter(retry.EqualJitter)

  // Equal jitter: delay/2 + random(0, delay/2)
  // For base 100: range is [50, 100]
  let results =
    list.range(1, 100)
    |> list.map(fn(_) { retry.calculate_delay(policy, 1) })

  results
  |> list.all(fn(r) { r.delay_ms >= 50 && r.delay_ms <= 100 })
  |> should.be_true()
}

pub fn equal_jitter_guarantees_minimum_progress_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(200)
    |> retry.with_jitter(retry.EqualJitter)

  // Minimum delay should be base/2 = 100
  let min_delay =
    list.range(1, 100)
    |> list.map(fn(_) { retry.delay_ms(policy, 1) })
    |> list.fold(10_000, int.min)

  { min_delay >= 100 }
  |> should.be_true()
}

// ============================================================================
// Decorrelated Jitter Tests
// ============================================================================

pub fn decorrelated_jitter_produces_valid_values_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_max_delay_ms(10_000)
    |> retry.with_jitter(retry.DecorrelatedJitter)

  // Decorrelated: random(base, delay * 3) capped
  // For attempt 1, delay = 100, range is [100, 300] capped to reasonable
  let results =
    list.range(1, 100)
    |> list.map(fn(_) { retry.calculate_delay(policy, 1) })

  // All should be at least base_delay
  results
  |> list.all(fn(r) { r.delay_ms >= 100 })
  |> should.be_true()
}

// ============================================================================
// Should Retry Tests
// ============================================================================

pub fn should_retry_returns_true_before_max_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(3)

  retry.should_retry(policy, 1)
  |> should.be_true()

  retry.should_retry(policy, 2)
  |> should.be_true()
}

pub fn should_retry_returns_false_at_max_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(3)

  retry.should_retry(policy, 3)
  |> should.be_false()

  retry.should_retry(policy, 4)
  |> should.be_false()
}

pub fn is_final_attempt_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(3)

  retry.is_final_attempt(policy, 1)
  |> should.be_false()

  retry.is_final_attempt(policy, 2)
  |> should.be_false()

  retry.is_final_attempt(policy, 3)
  |> should.be_true()
}

pub fn total_attempts_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(7)

  retry.total_attempts(policy)
  |> should.equal(7)
}

// ============================================================================
// DelayResult Metadata Tests
// ============================================================================

pub fn delay_result_contains_attempt_number_test() {
  let policy = retry.default_with_jitter()

  let result = retry.calculate_delay(policy, 5)

  result.attempt
  |> should.equal(5)
}

pub fn delay_result_is_final_attempt_flag_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(3)

  let result1 = retry.calculate_delay(policy, 1)
  result1.is_final_attempt
  |> should.be_false()

  let result2 = retry.calculate_delay(policy, 2)
  result2.is_final_attempt
  |> should.be_false()

  let result3 = retry.calculate_delay(policy, 3)
  result3.is_final_attempt
  |> should.be_true()
}

// ============================================================================
// Edge Cases
// ============================================================================

pub fn zero_base_delay_works_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(0)
    |> retry.with_jitter(retry.NoJitter)

  retry.delay_ms(policy, 1)
  |> should.equal(0)

  retry.delay_ms(policy, 5)
  |> should.equal(0)
}

pub fn negative_attempt_treated_as_one_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_jitter(retry.NoJitter)

  let result = retry.calculate_delay(policy, -1)
  result.attempt
  |> should.equal(1)

  result.delay_ms
  |> should.equal(100)
}

pub fn zero_attempt_treated_as_one_test() {
  let policy =
    retry.default()
    |> retry.with_base_delay_ms(100)
    |> retry.with_jitter(retry.NoJitter)

  let result = retry.calculate_delay(policy, 0)
  result.attempt
  |> should.equal(1)

  result.delay_ms
  |> should.equal(100)
}

// ============================================================================
// String Conversion Tests
// ============================================================================

pub fn jitter_to_string_test() {
  retry.jitter_to_string(retry.NoJitter)
  |> should.equal("none")

  retry.jitter_to_string(retry.FullJitter)
  |> should.equal("full")

  retry.jitter_to_string(retry.EqualJitter)
  |> should.equal("equal")

  retry.jitter_to_string(retry.DecorrelatedJitter)
  |> should.equal("decorrelated")
}

pub fn policy_to_string_contains_all_fields_test() {
  let policy =
    retry.default()
    |> retry.with_max_attempts(5)
    |> retry.with_base_delay_ms(200)
    |> retry.with_max_delay_ms(5000)
    |> retry.with_jitter(retry.FullJitter)

  let str = retry.policy_to_string(policy)

  { str != "" }
  |> should.be_true()
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Count unique values in a list
fn count_unique(values: List(Int)) -> Int {
  do_count_unique(values, [])
}

fn do_count_unique(values: List(Int), seen: List(Int)) -> Int {
  case values {
    [] -> list.length(seen)
    [first, ..rest] -> {
      case list.contains(seen, first) {
        True -> do_count_unique(rest, seen)
        False -> do_count_unique(rest, [first, ..seen])
      }
    }
  }
}
