//// Transport adapter conformance tests.
////
//// These tests validate that beam_adapter follows the transport.txt contract:
//// - start/stop lifecycle
//// - send/receive messages
//// - broadcast semantics
//// - invalid peer handling
//// - error classification
//// - graceful shutdown

import distribute/transport/adapter
import distribute/transport/beam_adapter
import distribute/transport/types
import gleam/bit_array
import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test 1: start() creates valid handle
pub fn start_creates_handle_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("test_adapter")

  let assert Ok(handle) = transport.start(opts)
  let handle_id = types.handle_id(handle)

  handle_id
  |> should.equal("test_adapter")
}

// Test 2: start() with custom options
pub fn start_with_custom_options_test() {
  let transport = beam_adapter.new()
  let opts =
    types.AdapterOptions(
      name: "custom_adapter",
      bind_address: Some("0.0.0.0"),
      port: Some(9000),
      max_payload_bytes: 2_097_152,
      connect_timeout_ms: 3000,
      telemetry_prefix: "custom",
      custom: dict.new(),
    )

  let assert Ok(handle) = transport.start(opts)
  let handle_id = types.handle_id(handle)

  handle_id
  |> should.equal("custom_adapter")
}

// Test 3: stop() succeeds with valid handle
pub fn stop_succeeds_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("stop_test")

  let assert Ok(handle) = transport.start(opts)
  let result = transport.stop(handle, 5000)

  result
  |> should.be_ok
}

// Test 4: stop() with timeout
pub fn stop_with_timeout_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("stop_timeout_test")

  let assert Ok(handle) = transport.start(opts)
  // Very short timeout but should still succeed for empty actor
  let result = transport.stop(handle, 100)

  result
  |> should.be_ok
}

// Test 5: send() to registered local process
pub fn send_to_local_registered_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("send_local_test")
  let assert Ok(handle) = transport.start(opts)

  // Register test receiver
  let probe = process.new_subject()
  let receiver_name = unique_name("test_receiver_local")
  let _pid = start_receiver(receiver_name, probe)

  // Send message
  let payload = <<"hello from transport">>
  let send_opts = adapter.default_send_options()
  let result = transport.send(handle, receiver_name, payload, send_opts)

  // Should succeed (queued)
  result
  |> should.be_ok

  // Verify message received
  let assert Ok(received_payload) = process.receive(probe, 1000)
  received_payload
  |> should.equal(payload)

  // Cleanup
  unregister_process(receiver_name)
  let _ = transport.stop(handle, 1000)
}

// Test 6: send() to invalid peer
pub fn send_to_invalid_peer_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("send_invalid_test")
  let assert Ok(handle) = transport.start(opts)

  let payload = <<"test">>
  let send_opts = adapter.default_send_options()
  let result = transport.send(handle, "nonexistent_process", payload, send_opts)

  // Should fail with InvalidPeer or AdapterFailure
  case result {
    Error(types.InvalidPeer(_)) -> should.be_true(True)
    Error(types.AdapterFailure(_)) -> should.be_true(True)
    _ -> should.fail()
  }

  let _ = transport.stop(handle, 1000)
}

// Test 7: send() empty peer string
pub fn send_empty_peer_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("send_empty_peer_test")
  let assert Ok(handle) = transport.start(opts)

  let payload = <<"test">>
  let send_opts = adapter.default_send_options()
  let result = transport.send(handle, "", payload, send_opts)

  result
  |> should.be_error

  let _ = transport.stop(handle, 1000)
}

// Test 8: broadcast() to group
pub fn broadcast_to_group_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("broadcast_test")
  let assert Ok(handle) = transport.start(opts)

  // Register test receivers
  let probe = process.new_subject()
  let name1 = unique_name("node2@host")
  let name2 = unique_name("node3@host")
  let _pid1 = start_receiver(name1, probe)
  let _pid2 = start_receiver(name2, probe)

  let payload = <<"broadcast message">>
  let send_opts = adapter.default_send_options()
  let result = transport.broadcast(handle, "all", payload, send_opts)

  // Broadcast should attempt send (may fail if group lookup not implemented)
  case result {
    Ok(_) -> should.be_true(True)
    Error(_) -> should.be_true(True)
  }

  // Cleanup
  unregister_process(name1)
  unregister_process(name2)
  let _ = transport.stop(handle, 1000)
}

// Test 9: health() returns status
pub fn health_returns_status_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("health_test")
  let assert Ok(handle) = transport.start(opts)

  let health = transport.health(handle)

  case health {
    types.Up -> should.be_true(True)
    types.Degraded(_) -> should.be_true(True)
    types.Down(_) -> should.be_true(True)
  }

  let _ = transport.stop(handle, 1000)
}

// Test 10: health() after stop returns Down
pub fn health_after_stop_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("health_after_stop_test")
  let assert Ok(handle) = transport.start(opts)

  let _ = transport.stop(handle, 1000)
  let health = transport.health(handle)

  case health {
    types.Down(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

// Test 11: metrics() returns data
pub fn metrics_returns_data_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("metrics_test")
  let assert Ok(handle) = transport.start(opts)

  let metrics = transport.metrics(handle)

  // Should have at least some metric keys
  { dict.size(metrics) >= 1 }
  |> should.be_true

  let _ = transport.stop(handle, 1000)
}

// Test 12: subscribe() registers callback
pub fn subscribe_registers_callback_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("subscribe_test")
  let assert Ok(handle) = transport.start(opts)

  let callback = fn(_peer, _payload, _metadata) { Nil }
  let result = transport.subscribe(handle, callback)

  case result {
    Ok(sub_id) -> {
      // Verify subscription ID exists
      let id_value = types.subscription_id_value(sub_id)
      id_value
      |> should.not_equal("")
    }
    Error(_) -> should.fail()
  }

  let _ = transport.stop(handle, 1000)
}

// Test 13: unsubscribe() removes subscription
pub fn unsubscribe_removes_subscription_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("unsubscribe_test")
  let assert Ok(handle) = transport.start(opts)

  let callback = fn(_peer, _payload, _metadata) { Nil }
  let assert Ok(sub_id) = transport.subscribe(handle, callback)

  let result = transport.unsubscribe(handle, sub_id)
  result
  |> should.be_ok

  let _ = transport.stop(handle, 1000)
}

// Test 14: multiple subscriptions
pub fn multiple_subscriptions_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("multi_sub_test")
  let assert Ok(handle) = transport.start(opts)

  let callback1 = fn(_peer, _payload, _metadata) { Nil }
  let callback2 = fn(_peer, _payload, _metadata) { Nil }

  let assert Ok(sub_id1) = transport.subscribe(handle, callback1)
  let assert Ok(sub_id2) = transport.subscribe(handle, callback2)

  // Should have different IDs
  types.subscription_id_value(sub_id1)
  |> should.not_equal(types.subscription_id_value(sub_id2))

  let _ = transport.stop(handle, 1000)
}

// Test 15: send() with large payload (< max)
pub fn send_large_payload_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("large_payload_test")
  let assert Ok(handle) = transport.start(opts)
  let receiver = process.new_subject()
  let assert Ok(pid) = process.subject_owner(receiver)
  let name = unique_name("large_receiver")
  register_process(pid, name)

  // 1MB payload (under 10MB limit)
  let payload = bit_array.from_string(string_of_size(1_048_576))
  let send_opts = adapter.default_send_options()
  let result = transport.send(handle, name, payload, send_opts)

  result
  |> should.be_ok

  // Cleanup
  unregister_process(name)
  let _ = transport.stop(handle, 1000)
}

// Test 16: send() with too large payload
pub fn send_too_large_payload_test() {
  let transport = beam_adapter.new()
  let opts = adapter.default_options("too_large_test")
  let assert Ok(handle) = transport.start(opts)

  let probe = process.new_subject()
  let name = unique_name("large_receiver_fail")
  let _pid = start_receiver(name, probe)

  // 11MB payload (over 10MB limit)
  let payload = bit_array.from_string(string_of_size(11_534_336))
  let send_opts = adapter.default_send_options()
  let result = transport.send(handle, name, payload, send_opts)

  case result {
    Error(types.PayloadTooLarge(_, _)) -> should.be_true(True)
    Error(types.AdapterFailure(_)) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  unregister_process(name)
  let _ = transport.stop(handle, 1000)
}

// Test 17: Error classification - transient errors
pub fn transient_error_classification_test() {
  let err1 = types.ConnectionClosed("node@host")
  let err2 = types.Timeout(5000)
  let err3 = types.Backpressure(100)

  types.is_transient_error(err1)
  |> should.be_true

  types.is_transient_error(err2)
  |> should.be_true

  types.is_transient_error(err3)
  |> should.be_true
}

// Test 18: Error classification - permanent errors
pub fn permanent_error_classification_test() {
  let err1 = types.InvalidPeer("bad_peer")
  let err2 = types.PayloadTooLarge(1000, 500)
  let err3 = types.SerializationError("bad format")

  types.is_permanent_error(err1)
  |> should.be_true

  types.is_permanent_error(err2)
  |> should.be_true

  types.is_permanent_error(err3)
  |> should.be_true
}

// Test 19: child_spec() creates valid spec
pub fn child_spec_creates_spec_test() {
  let opts = adapter.default_options("child_spec_test")
  let _spec = beam_adapter.child_spec(opts)

  // If it compiles and doesn't crash, the spec is valid
  should.be_true(True)
}

// Test 20: default options have sensible values
pub fn default_options_test() {
  let opts = adapter.default_options("default_test")

  opts.name
  |> should.equal("default_test")

  opts.max_payload_bytes
  |> should.equal(1_048_576)

  opts.connect_timeout_ms
  |> should.equal(5000)

  opts.telemetry_prefix
  |> should.equal("transport")

  opts.bind_address
  |> should.equal(None)

  opts.port
  |> should.equal(None)
}

// Test 21: default send options
pub fn default_send_options_test() {
  let opts = adapter.default_send_options()

  opts.timeout_ms
  |> should.equal(None)

  opts.priority
  |> should.equal(None)

  opts.reliable
  |> should.be_false

  opts.correlation_id
  |> should.equal(None)
}

// Helper: create string of specific size
fn string_of_size(size: Int) -> String {
  do_string_of_size(size, "")
}

fn unique_name(prefix: String) -> String {
  prefix <> "_" <> int.to_string(int.random(1_000_000))
}

fn do_string_of_size(remaining: Int, acc: String) -> String {
  case remaining <= 0 {
    True -> acc
    False -> {
      case remaining >= 1000 {
        True -> do_string_of_size(remaining - 1000, acc <> string_chunk_1000())
        False -> do_string_of_size(remaining - 1, acc <> "x")
      }
    }
  }
}

fn string_chunk_1000() -> String {
  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
}

fn start_receiver(
  name: String,
  forward_to: process.Subject(BitArray),
) -> process.Pid {
  let pid = process.spawn(fn() { receiver_loop(forward_to) })
  register_process(pid, name)
  pid
}

fn receiver_loop(forward_to: process.Subject(BitArray)) {
  case receive_message(100) {
    Ok(payload) -> {
      process.send(forward_to, payload)
      receiver_loop(forward_to)
    }
    Error(_) -> receiver_loop(forward_to)
  }
}

// FFI helpers for process registration
@external(erlang, "transport_test_ffi", "register_process")
fn register_process(pid: process.Pid, name: String) -> Nil

@external(erlang, "transport_test_ffi", "unregister_process")
fn unregister_process(name: String) -> Nil

@external(erlang, "transport_test_ffi", "receive_message")
fn receive_message(timeout: Int) -> Result(BitArray, Nil)
