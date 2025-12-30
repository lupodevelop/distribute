//// Integration tests for the transport layer and messaging.
////
//// These tests verify that:
//// 1. The transport facade works correctly (singleton pattern).
//// 2. The messaging module correctly routes through the transport layer.
//// 3. Global name resolution works via the transport adapter.

import distribute/codec
import distribute/messaging
import distribute/transport
import distribute/transport/adapter
import distribute/transport/types
import gleam/erlang/process
import gleam/otp/static_supervisor as supervisor
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Helper to start the transport singleton
fn start_transport() -> process.Pid {
  // We use a supervisor to start the child_spec, mimicking the real app
  let worker = transport.child_spec()
  let assert Ok(sup) = 
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(worker)
    |> supervisor.start()
  
  // Give the process time to register
  process.sleep(50)
  
  sup.pid
}

// Helper to stop the transport
fn stop_transport(pid: process.Pid) {
  process.send_exit(pid)
}

// Test 1: transport.send via singleton
pub fn transport_singleton_send_test() {
  let sup = start_transport()
  
  let probe = process.new_subject()
  let name = "transport_singleton_test"
  let _pid = start_receiver(name, probe)

  let payload = <<"singleton test">>
  let opts = adapter.default_send_options()
  
  // Send using the facade
  let result = transport.send(name, payload, opts)
  
  result
  |> should.be_ok

  // Verify receipt
  let assert Ok(received) = process.receive(probe, 1000)
  received
  |> should.equal(payload)

  stop_transport(sup)
  unregister_process(name)
}

// Test 2: messaging.send_global_typed integration
pub fn messaging_integration_test() {
  let sup = start_transport()

  let probe = process.new_subject()
  let name = "messaging_integration_test"
  let _pid = start_receiver(name, probe)

  let msg = "hello typed world"
  let encoder = codec.string_encoder()

  // Send using messaging API (which should use transport)
  let result = messaging.send_global_typed(name, msg, encoder)

  result
  |> should.be_ok

  // Verify receipt (raw binary)
  let assert Ok(received_binary) = process.receive(probe, 1000)
  
  // Decode to verify content
  let assert Ok(decoded) = codec.decode(codec.string_decoder(), received_binary)
  decoded
  |> should.equal(msg)

  stop_transport(sup)
  unregister_process(name)
}

// Test 3: transport.health via singleton
pub fn transport_health_test() {
  let sup = start_transport()

  let status = transport.health()
  
  case status {
    types.Up -> should.be_true(True)
    _ -> should.fail()
  }

  stop_transport(sup)
}

// --- Helpers copied/adapted from conformance test ---

fn start_receiver(
  name: String,
  forward_to: process.Subject(BitArray),
) -> process.Pid {
  let pid = process.spawn(fn() { receiver_loop(forward_to) })
  let _ = register_process(pid, name)
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

@external(erlang, "transport_test_ffi", "register_process")
fn register_process(pid: process.Pid, name: String) -> Nil

@external(erlang, "transport_test_ffi", "unregister_process")
fn unregister_process(name: String) -> Nil

@external(erlang, "transport_test_ffi", "receive_message")
fn receive_message(timeout: Int) -> Result(BitArray, Nil)
