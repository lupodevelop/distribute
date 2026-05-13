/// Cross-node decode simulation tests.
///
/// These tests verify the core distributed claim WITHOUT requiring a real
/// peer node or epmd: a raw encoded binary sent to an actor's Subject
/// (exactly as a remote node would send it) is correctly decoded and
/// dispatched by the actor's handler.
///
/// What actually happens in distribute over a real network:
///   remote node  →  encodes msg  →  sends {actor_name, bytes} to Pid
///   local actor  →  receives bytes  →  decodes  →  calls handler
///
/// The `dev/peer_support.gleam` module and `src/peer_ffi.erl` provide the
/// infrastructure for actual peer tests. For an automated real-peer smoke
/// test see `test/multinode_peer_test.gleam`. You can also run the manual
/// playground with:
///
///   gleam dev
///
/// which starts a real two-node scenario using epmd.
import distribute/actor
import distribute/codec
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_int() -> Int

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String

fn unique_id() -> String {
  let n = erlang_unique_int()
  int_to_string(case n < 0 {
    True -> 0 - n
    False -> n
  })
}

// ---------------------------------------------------------------------------
// Simulated cross-node decode tests
// ---------------------------------------------------------------------------

/// A remote node encodes a message and sends the raw bytes directly to the
/// actor's Subject PID. The actor decodes and dispatches it.
/// This is bit-for-bit what happens in production over the network.
pub fn simulated_remote_send_int_test() {
  let tn = registry.named("sim_int_" <> unique_id(), codec.int())
  let result_subj = process.new_subject()

  let assert Ok(gs) =
    actor.start_registered(
      tn,
      0,
      fn(msg, state) {
        process.send(result_subj, msg)
        receiver.Continue(state)
      },
      5000,
    )

  // Simulate the remote node: encode directly and send the raw binary.
  // This bypasses global.send so it tests the actor's decode path end-to-end.
  let assert Ok(encoded) = codec.encode(codec.int_encoder(), 42)
  process.send(global.subject(gs), encoded)

  let assert Ok(received) = process.receive(result_subj, 1000)
  should.equal(received, 42)

  let _ = registry.unregister(registry.typed_name_to_string(tn))
}

/// Verify that multiple sequential raw sends accumulate state correctly,
/// as they would during a sustained multi-node message exchange.
pub fn simulated_stateful_accumulation_test() {
  let tn = registry.named("sim_acc_" <> unique_id(), codec.int())
  let done_subj = process.new_subject()

  let assert Ok(gs) =
    actor.start_registered(
      tn,
      0,
      fn(msg, state) {
        let new_state = state + msg
        case new_state >= 10 {
          True -> {
            process.send(done_subj, new_state)
            receiver.Stop
          }
          False -> receiver.Continue(new_state)
        }
      },
      5000,
    )

  // Three raw sends, as if from different remote nodes
  let assert Ok(enc3) = codec.encode(codec.int_encoder(), 3)
  let assert Ok(enc4) = codec.encode(codec.int_encoder(), 4)
  let assert Ok(enc5) = codec.encode(codec.int_encoder(), 5)

  process.send(global.subject(gs), enc3)
  process.send(global.subject(gs), enc4)
  process.send(global.subject(gs), enc5)

  let assert Ok(total) = process.receive(done_subj, 1000)
  should.equal(total, 12)

  let _ = registry.unregister(registry.typed_name_to_string(tn))
}

/// Verify that a subject reconstructed from name+pid (which is exactly
/// what a remote node does via registry.lookup) delivers messages correctly.
pub fn simulated_lookup_and_send_test() {
  let tn = registry.named("sim_lk_" <> unique_id(), codec.string())
  let result_subj = process.new_subject()

  let assert Ok(_) =
    actor.start_registered(
      tn,
      Nil,
      fn(msg, _state) {
        process.send(result_subj, msg)
        receiver.Continue(Nil)
      },
      5000,
    )

  // Simulate the remote side: reconstruct subject from name+pid (no codec needed)
  let assert Ok(gs) = registry.lookup(tn)
  let assert Ok(Nil) = global.send(gs, "hello from remote")

  let assert Ok(msg) = process.receive(result_subj, 1000)
  should.equal(msg, "hello from remote")

  let _ = registry.unregister(registry.typed_name_to_string(tn))
}

/// A raw binary that fails decoding should be silently dropped by the actor
/// and NOT crash it. Subsequent valid messages must still be processed.
pub fn simulated_malformed_binary_does_not_crash_actor_test() {
  let tn = registry.named("sim_bad_" <> unique_id(), codec.int())
  let result_subj = process.new_subject()

  let assert Ok(gs) =
    actor.start_registered(
      tn,
      0,
      fn(msg, state) {
        process.send(result_subj, msg)
        receiver.Continue(state)
      },
      5000,
    )

  // Send garbage (e.g. from a node with a different/wrong codec)
  process.send(global.subject(gs), <<0xFF, 0x00>>)

  // Actor must still be alive and process the next valid message
  let assert Ok(encoded) = codec.encode(codec.int_encoder(), 7)
  process.send(global.subject(gs), encoded)

  let assert Ok(received) = process.receive(result_subj, 1000)
  should.equal(received, 7)

  let _ = registry.unregister(registry.typed_name_to_string(tn))
}
