/// Tests for the circuit breaker module.
///
/// Verifies the state machine transitions and policy enforcement.
import distribute/transport/internal/circuit_breaker as cb
import gleam/dict
import gleeunit
import gleeunit/should

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// Basic State Tests
// =============================================================================

pub fn new_breaker_starts_closed_test() {
  let breaker = cb.new_breaker()
  case breaker.state {
    cb.Closed -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn new_breaker_has_zero_counters_test() {
  let breaker = cb.new_breaker()
  breaker.consecutive_failures |> should.equal(0)
  breaker.consecutive_successes |> should.equal(0)
  breaker.total_failures |> should.equal(0)
  breaker.total_successes |> should.equal(0)
}

pub fn default_policy_values_test() {
  let policy = cb.default_policy()
  policy.failure_threshold |> should.equal(5)
  policy.success_threshold |> should.equal(2)
  policy.timeout_ms |> should.equal(30_000)
  policy.half_open_max_calls |> should.equal(1)
}

// =============================================================================
// Request Allowance Tests
// =============================================================================

pub fn closed_circuit_allows_requests_test() {
  let breaker = cb.new_breaker()
  let policy = cb.default_policy()
  cb.should_allow_request(breaker, policy) |> should.be_true()
}

pub fn open_circuit_blocks_requests_test() {
  let breaker = cb.new_breaker()
  let policy = cb.default_policy()

  // Open the circuit by recording failures
  let breaker =
    breaker
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)

  // Should be open now (5 failures = threshold)
  case breaker.state {
    cb.Open(_) -> should.be_true(True)
    _ -> should.fail()
  }

  // Requests should be blocked
  cb.should_allow_request(breaker, policy) |> should.be_false()
}

pub fn half_open_allows_requests_test() {
  let breaker =
    cb.NodeCircuitBreaker(
      state: cb.HalfOpen,
      consecutive_failures: 0,
      consecutive_successes: 0,
      total_failures: 5,
      total_successes: 0,
    )
  let policy = cb.default_policy()
  cb.should_allow_request(breaker, policy) |> should.be_true()
}

// =============================================================================
// State Transition Tests
// =============================================================================

pub fn success_in_closed_resets_failures_test() {
  let breaker =
    cb.NodeCircuitBreaker(
      state: cb.Closed,
      consecutive_failures: 3,
      consecutive_successes: 0,
      total_failures: 3,
      total_successes: 0,
    )
  let policy = cb.default_policy()

  let updated = cb.record_success(breaker, policy)
  updated.consecutive_failures |> should.equal(0)
  updated.consecutive_successes |> should.equal(1)
  updated.total_successes |> should.equal(1)
}

pub fn failures_accumulate_in_closed_test() {
  let breaker = cb.new_breaker()
  let policy = cb.default_policy()

  let updated =
    breaker
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)

  updated.consecutive_failures |> should.equal(3)
  updated.total_failures |> should.equal(3)
  case updated.state {
    cb.Closed -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn threshold_failures_open_circuit_test() {
  let breaker = cb.new_breaker()
  let policy = cb.default_policy()

  // 5 failures should open circuit
  let updated =
    breaker
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)
    |> cb.record_failure(policy)

  case updated.state {
    cb.Open(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn success_in_half_open_can_close_circuit_test() {
  let breaker =
    cb.NodeCircuitBreaker(
      state: cb.HalfOpen,
      consecutive_failures: 0,
      consecutive_successes: 1,
      total_failures: 5,
      total_successes: 0,
    )
  let policy = cb.default_policy()

  // One more success should close (threshold is 2)
  let updated = cb.record_success(breaker, policy)
  case updated.state {
    cb.Closed -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn failure_in_half_open_reopens_circuit_test() {
  let breaker =
    cb.NodeCircuitBreaker(
      state: cb.HalfOpen,
      consecutive_failures: 0,
      consecutive_successes: 1,
      total_failures: 5,
      total_successes: 1,
    )
  let policy = cb.default_policy()

  let updated = cb.record_failure(breaker, policy)
  case updated.state {
    cb.Open(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

// =============================================================================
// Registry Tests
// =============================================================================

pub fn get_or_create_creates_new_breaker_test() {
  let registry = dict.new()
  let #(breaker, updated_registry) = cb.get_or_create(registry, "node1")

  case breaker.state {
    cb.Closed -> should.be_true(True)
    _ -> should.fail()
  }

  dict.has_key(updated_registry, "node1") |> should.be_true()
}

pub fn get_or_create_returns_existing_breaker_test() {
  let modified_breaker =
    cb.NodeCircuitBreaker(
      state: cb.HalfOpen,
      consecutive_failures: 3,
      consecutive_successes: 0,
      total_failures: 3,
      total_successes: 0,
    )
  let registry = dict.insert(dict.new(), "node1", modified_breaker)

  let #(breaker, _) = cb.get_or_create(registry, "node1")
  case breaker.state {
    cb.HalfOpen -> should.be_true(True)
    _ -> should.fail()
  }
  breaker.consecutive_failures |> should.equal(3)
}

pub fn reset_creates_fresh_breaker_test() {
  let modified_breaker =
    cb.NodeCircuitBreaker(
      state: cb.Open(12_345),
      consecutive_failures: 10,
      consecutive_successes: 0,
      total_failures: 10,
      total_successes: 0,
    )
  let registry = dict.insert(dict.new(), "node1", modified_breaker)

  let updated = cb.reset(registry, "node1")
  let assert Ok(breaker) = dict.get(updated, "node1")

  case breaker.state {
    cb.Closed -> should.be_true(True)
    _ -> should.fail()
  }
  breaker.consecutive_failures |> should.equal(0)
}

// =============================================================================
// Metrics Tests
// =============================================================================

pub fn get_metrics_returns_all_values_test() {
  let breaker =
    cb.NodeCircuitBreaker(
      state: cb.HalfOpen,
      consecutive_failures: 3,
      consecutive_successes: 1,
      total_failures: 10,
      total_successes: 5,
    )

  let metrics = cb.get_metrics(breaker)

  // HalfOpen = 1
  dict.get(metrics, "circuit_state") |> should.equal(Ok(1))
  dict.get(metrics, "consecutive_failures") |> should.equal(Ok(3))
  dict.get(metrics, "consecutive_successes") |> should.equal(Ok(1))
  dict.get(metrics, "total_failures") |> should.equal(Ok(10))
  dict.get(metrics, "total_successes") |> should.equal(Ok(5))
}

pub fn get_metrics_state_values_test() {
  let closed_breaker = cb.new_breaker()
  let open_breaker =
    cb.NodeCircuitBreaker(..closed_breaker, state: cb.Open(123))
  let half_open_breaker =
    cb.NodeCircuitBreaker(..closed_breaker, state: cb.HalfOpen)

  // Closed = 0
  dict.get(cb.get_metrics(closed_breaker), "circuit_state")
  |> should.equal(Ok(0))

  // HalfOpen = 1
  dict.get(cb.get_metrics(half_open_breaker), "circuit_state")
  |> should.equal(Ok(1))

  // Open = 2
  dict.get(cb.get_metrics(open_breaker), "circuit_state")
  |> should.equal(Ok(2))
}
