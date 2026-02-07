import distribute/transport/behaviour
import gleam/dict
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Type construction tests
// ============================================================================

pub fn transport_state_construction_test() {
  let state = behaviour.TransportState
  should.equal(state, behaviour.TransportState)
}

pub fn transport_error_variants_test() {
  let e1 = behaviour.AdapterFailure("reason")
  let e2 = behaviour.InvalidNode
  let e3 = behaviour.ConnectionClosed
  let e4 = behaviour.Backpressure
  let e5 = behaviour.PayloadTooLarge
  let e6: behaviour.TransportError = behaviour.Timeout
  let e7 = behaviour.ShutdownTimeout

  case e1 {
    behaviour.AdapterFailure(r) -> should.equal(r, "reason")
    _ -> should.be_true(False)
  }
  case e2 {
    behaviour.InvalidNode -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case e3 {
    behaviour.ConnectionClosed -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case e4 {
    behaviour.Backpressure -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case e5 {
    behaviour.PayloadTooLarge -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case e6 {
    behaviour.Timeout -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case e7 {
    behaviour.ShutdownTimeout -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn health_status_variants_test() {
  let h1 = behaviour.Up
  let h2 = behaviour.Degraded("high latency")
  let h3 = behaviour.Down("connection lost")

  case h1 {
    behaviour.Up -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case h2 {
    behaviour.Degraded(msg) -> should.equal(msg, "high latency")
    _ -> should.be_true(False)
  }
  case h3 {
    behaviour.Down(msg) -> should.equal(msg, "connection lost")
    _ -> should.be_true(False)
  }
}

pub fn transport_opts_construction_test() {
  let opts =
    behaviour.TransportOpts(
      name: Some("my_transport"),
      bind_address: Some("0.0.0.0"),
      port: Some(9000),
      max_payload_bytes: Some(1_048_576),
      connect_timeout_ms: Some(5000),
      heartbeat_interval_ms: Some(30_000),
      retry_policy: None,
      circuit_breaker_policy: None,
      fallback_strategy: None,
      delivery_guarantee: None,
      capabilities: [],
    )
  should.equal(opts.name, Some("my_transport"))
  should.equal(opts.port, Some(9000))
  should.equal(opts.capabilities, [])
}

pub fn circuit_breaker_policy_test() {
  let policy =
    behaviour.CircuitBreakerPolicy(
      failure_threshold: 5,
      success_threshold: 3,
      timeout_ms: 30_000,
      half_open_max_calls: 1,
    )
  should.equal(policy.failure_threshold, 5)
  should.equal(policy.half_open_max_calls, 1)
}

pub fn fallback_strategy_variants_test() {
  let f1 = behaviour.NoFallback
  let f2 = behaviour.FailoverList(fallback_transports: ["tcp", "udp"])

  case f1 {
    behaviour.NoFallback -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case f2 {
    behaviour.FailoverList(transports) ->
      should.equal(transports, ["tcp", "udp"])
    _ -> should.be_true(False)
  }
}

pub fn delivery_guarantee_variants_test() {
  let g1 = behaviour.AtMostOnce
  let g2 = behaviour.AtLeastOnce(ack_timeout_ms: 5000, max_retries: 3)
  let g3 =
    behaviour.ExactlyOnce(
      ack_timeout_ms: 5000,
      max_retries: 3,
      dedup_window_ms: 60_000,
    )

  case g1 {
    behaviour.AtMostOnce -> should.be_true(True)
    _ -> should.be_true(False)
  }
  case g2 {
    behaviour.AtLeastOnce(t, r) -> {
      should.equal(t, 5000)
      should.equal(r, 3)
    }
    _ -> should.be_true(False)
  }
  case g3 {
    behaviour.ExactlyOnce(t, r, d) -> {
      should.equal(t, 5000)
      should.equal(r, 3)
      should.equal(d, 60_000)
    }
    _ -> should.be_true(False)
  }
}

pub fn ack_message_variants_test() {
  let a1 = behaviour.Ack(message_id: "msg_001")
  let a2 = behaviour.Nack(message_id: "msg_002", reason: "decode error")

  case a1 {
    behaviour.Ack(id) -> should.equal(id, "msg_001")
    _ -> should.be_true(False)
  }
  case a2 {
    behaviour.Nack(id, reason) -> {
      should.equal(id, "msg_002")
      should.equal(reason, "decode error")
    }
    _ -> should.be_true(False)
  }
}

// ============================================================================
// Stub function tests
// ============================================================================

pub fn init_stub_test() {
  let state = behaviour.init()
  should.equal(state, behaviour.TransportState)
}

pub fn send_stub_returns_error_test() {
  let result = behaviour.send("node@host", <<"data":utf8>>)
  should.be_error(result)
}

pub fn broadcast_stub_returns_error_test() {
  let result =
    behaviour.broadcast(["node1@host", "node2@host"], <<"data":utf8>>)
  should.be_error(result)
}

pub fn health_stub_returns_down_test() {
  let result = behaviour.health()
  case result {
    behaviour.Down(_) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn metrics_stub_returns_empty_dict_test() {
  let m = behaviour.metrics()
  should.equal(m, dict.new())
}
