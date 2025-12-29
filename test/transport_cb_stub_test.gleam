import distribute/transport/behaviour
import distribute/transport/circuit_breaker as cb
import distribute/transport/stub_actor as stub
import gleam/dict
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// ACTOR LIFECYCLE TESTS
// ============================================================================

pub fn actor_start_test() {
  let result = stub.start_link()
  result
  |> should.be_ok()
}

pub fn actor_send_test() {
  let assert Ok(transport) = stub.start_link()
  let payload = <<"test message">>

  let result = stub.send(transport, "node2@host", payload)
  result
  |> should.be_ok()
}

pub fn actor_broadcast_test() {
  let assert Ok(transport) = stub.start_link()
  let payload = <<"broadcast message">>
  let nodes = ["node2@host", "node3@host"]

  let result = stub.broadcast(transport, nodes, payload)
  result
  |> should.be_ok()
}

pub fn actor_health_test() {
  let assert Ok(transport) = stub.start_link()

  let status = stub.health(transport)
  case status {
    behaviour.Up -> Nil
    _ -> panic as "Expected Up status"
  }
}

pub fn actor_metrics_test() {
  let assert Ok(transport) = stub.start_link()

  let metrics = stub.metrics(transport)
  let known_count = dict.get(metrics, "known_nodes_count")
  known_count
  |> should.be_ok()
  |> should.equal(2)
}

// ============================================================================
// CIRCUIT BREAKER TESTS  
// ============================================================================

pub fn circuit_breaker_new_test() {
  let breaker = cb.new_breaker()

  case breaker.state {
    behaviour.Closed -> Nil
    _ -> panic as "New breaker should be Closed"
  }

  breaker.consecutive_failures
  |> should.equal(0)
}

pub fn circuit_breaker_success_test() {
  let policy = cb.default_policy()
  let breaker = cb.new_breaker()

  let updated = cb.record_success(breaker, policy)

  updated.consecutive_successes
  |> should.equal(1)

  updated.total_successes
  |> should.equal(1)
}

pub fn circuit_breaker_failure_opens_test() {
  let policy = cb.default_policy()
  let breaker = cb.new_breaker()
  let current_time = 1000

  // Fail 5 times (threshold)
  let b1 = cb.record_failure(breaker, policy, current_time)
  let b2 = cb.record_failure(b1, policy, current_time)
  let b3 = cb.record_failure(b2, policy, current_time)
  let b4 = cb.record_failure(b3, policy, current_time)
  let b5 = cb.record_failure(b4, policy, current_time)

  case b5.state {
    behaviour.Open(_) -> Nil
    _ -> panic as "Circuit should be Open after threshold failures"
  }
}

pub fn circuit_breaker_halfopen_test() {
  let policy = cb.default_policy()
  let breaker = cb.new_breaker()
  let current_time = 1000

  // Open the circuit
  let b1 = cb.record_failure(breaker, policy, current_time)
  let b2 = cb.record_failure(b1, policy, current_time)
  let b3 = cb.record_failure(b2, policy, current_time)
  let b4 = cb.record_failure(b3, policy, current_time)
  let b5 = cb.record_failure(b4, policy, current_time)

  // Wait for timeout (30s)
  let future_time = current_time + 31_000
  let #(_allowed, b6) = cb.should_allow_request(b5, policy, future_time)

  case b6.state {
    behaviour.HalfOpen(_) -> Nil
    _ -> panic as "Circuit should be HalfOpen after timeout"
  }
}

pub fn circuit_breaker_closes_after_success_test() {
  let policy = cb.default_policy()
  let current_time = 1000

  // Create HalfOpen breaker
  let breaker =
    behaviour.NodeCircuitBreaker(
      state: behaviour.HalfOpen(current_time),
      consecutive_failures: 5,
      consecutive_successes: 0,
      total_failures: 5,
      total_successes: 0,
    )

  // Succeed threshold times (2)
  let b1 = cb.record_success(breaker, policy)
  let b2 = cb.record_success(b1, policy)

  case b2.state {
    behaviour.Closed -> Nil
    _ -> panic as "Circuit should close after success threshold in HalfOpen"
  }
}

pub fn circuit_breaker_blocks_when_open_test() {
  let policy = cb.default_policy()
  let breaker = cb.new_breaker()
  let current_time = 1000

  // Open the circuit
  let b1 = cb.record_failure(breaker, policy, current_time)
  let b2 = cb.record_failure(b1, policy, current_time)
  let b3 = cb.record_failure(b2, policy, current_time)
  let b4 = cb.record_failure(b3, policy, current_time)
  let b5 = cb.record_failure(b4, policy, current_time)

  // Try immediately (before timeout)
  let #(allowed, _) = cb.should_allow_request(b5, policy, current_time + 100)

  allowed
  |> should.be_false()
}

pub fn circuit_breaker_registry_test() {
  let registry = dict.new()
  let #(breaker, updated_registry) = cb.get_or_create(registry, "node1@host")

  case breaker.state {
    behaviour.Closed -> Nil
    _ -> panic as "New breaker should be Closed"
  }

  dict.size(updated_registry)
  |> should.equal(1)
}

pub fn circuit_breaker_reset_test() {
  let policy = cb.default_policy()
  let current_time = 1000

  // Create a breaker with failures
  let breaker = cb.new_breaker()
  let b1 = cb.record_failure(breaker, policy, current_time)
  let b2 = cb.record_failure(b1, policy, current_time)

  // Add to registry
  let registry = dict.new() |> dict.insert("node1@host", b2)

  // Reset
  let updated = cb.reset(registry, "node1@host")
  let assert Ok(reset_breaker) = dict.get(updated, "node1@host")

  reset_breaker.consecutive_failures
  |> should.equal(0)

  case reset_breaker.state {
    behaviour.Closed -> Nil
    _ -> panic as "Reset breaker should be Closed"
  }
}

// ============================================================================
// INTEGRATION TESTS - ACTOR WITH CIRCUIT BREAKER
// ============================================================================

pub fn actor_circuit_breaker_integration_test() {
  let assert Ok(transport) = stub.start_link()
  let payload = <<"test">>

  // Send to unknown node multiple times
  let _r1 = stub.send(transport, "unknown@host", payload)
  let _r2 = stub.send(transport, "unknown@host", payload)
  let _r3 = stub.send(transport, "unknown@host", payload)
  let _r4 = stub.send(transport, "unknown@host", payload)
  let _r5 = stub.send(transport, "unknown@host", payload)

  // Circuit should be open now, next request blocked
  let result = stub.send(transport, "unknown@host", payload)

  case result {
    Error(behaviour.Backpressure) -> Nil
    _ -> panic as "Expected Backpressure error when circuit is open"
  }
}

pub fn actor_get_circuit_state_test() {
  let assert Ok(transport) = stub.start_link()

  let state = stub.get_circuit_state(transport, "nonexistent@host")

  case state {
    None -> Nil
    Some(_) -> panic as "Should not have circuit for nonexistent node"
  }
}

pub fn actor_reset_circuit_test() {
  let assert Ok(transport) = stub.start_link()
  let payload = <<"test">>

  // Fail a few times
  let _r1 = stub.send(transport, "unknown@host", payload)
  let _r2 = stub.send(transport, "unknown@host", payload)

  // Reset
  stub.reset_circuit(transport, "unknown@host")

  // Check state
  let state = stub.get_circuit_state(transport, "unknown@host")

  case state {
    Some(breaker) -> {
      breaker.consecutive_failures
      |> should.equal(0)
    }
    None -> Nil
    // Could be None if not yet created
  }
}
