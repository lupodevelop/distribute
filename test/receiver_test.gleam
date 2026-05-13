import distribute/codec
import distribute/config
import distribute/global
import distribute/receiver
import gleam/erlang/process
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

pub fn receive_typed_test() {
  let subj = process.new_subject()
  let enc = codec.string_encoder()
  let dec = codec.string_decoder()

  // Send a binary message
  let assert Ok(bin) = codec.encode(enc, "typed-msg")
  process.send(subj, bin)

  // Receive and decode
  let assert Ok(val) = receiver.receive_typed(subj, dec, 1000)
  should.equal(val, "typed-msg")
}

pub fn receive_typed_timeout_test() {
  let subj = process.new_subject()
  let dec = codec.int_decoder()
  let result = receiver.receive_typed(subj, dec, 10)
  should.be_error(result)
}

pub fn start_receiver_test() {
  let dec = codec.int_decoder()
  let enc = codec.int_encoder()

  // Start an actor that sums integers
  let assert Ok(subj) =
    receiver.start_receiver(0, dec, fn(_msg, state) {
      receiver.Continue(state + 1)
    })

  // Send some messages
  let assert Ok(bin1) = codec.encode(enc, 1)
  let assert Ok(bin2) = codec.encode(enc, 2)
  process.send(subj, bin1)
  process.send(subj, bin2)

  // Give it time to process
  process.sleep(50)
  should.be_true(True)
}

pub fn start_distributed_worker_test() {
  let enc = codec.string_encoder()
  let dec = codec.string_decoder()

  let assert Ok(started) =
    receiver.start_distributed_worker(
      "recv_worker_" <> test_helpers.unique_id(),
      Nil,
      enc,
      dec,
      fn(_msg, state) { receiver.Continue(state) },
      5000,
    )

  // Should be alive (real OTP actor)
  should.be_true(process.is_alive(started.pid))

  // Should be able to send
  let assert Ok(Nil) = global.send(started.data, "hello")
  should.be_true(True)
}

pub fn start_distributed_worker_with_state_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()

  // Create a subject to receive results
  let result_subj = process.new_subject()

  let assert Ok(started) =
    receiver.start_distributed_worker(
      "recv_state_" <> test_helpers.unique_id(),
      0,
      enc,
      dec,
      fn(msg, state) {
        let new = state + msg
        // After accumulating >= 3, signal done
        case new >= 3 {
          True -> {
            process.send(result_subj, new)
            receiver.Stop
          }
          False -> receiver.Continue(new)
        }
      },
      5000,
    )

  // Send messages through the GlobalSubject
  let assert Ok(Nil) = global.send(started.data, 1)
  let assert Ok(Nil) = global.send(started.data, 2)

  let assert Ok(total) = process.receive(result_subj, 1000)
  should.equal(total, 3)
}

pub fn start_receiver_observed_error_hook_test() {
  // Verify that on_decode_error is called when a malformed binary arrives.
  let error_subj = process.new_subject()

  let assert Ok(subj) =
    receiver.start_receiver_observed(
      Nil,
      // Decoder that always fails
      fn(_) { Error(codec.InvalidBinary("bad")) },
      fn(_msg, state) { receiver.Continue(state) },
      fn(err) { process.send(error_subj, err) },
    )

  // Send a binary that will fail decoding
  process.send(subj, <<1, 2, 3>>)

  // The error hook should fire
  let assert Ok(err) = process.receive(error_subj, 500)
  should.equal(err, codec.InvalidBinary("bad"))
}

pub fn start_distributed_worker_observed_error_hook_test() {
  let error_subj = process.new_subject()

  let assert Ok(started) =
    receiver.start_distributed_worker_observed(
      "obs_worker_" <> test_helpers.unique_id(),
      Nil,
      codec.string_encoder(),
      // Decoder that always fails
      fn(_) { Error(codec.DecodeFailed("intentional")) },
      fn(_msg, state) { receiver.Continue(state) },
      fn(err) { process.send(error_subj, err) },
      5000,
    )

  // Send a binary that will fail decoding
  let assert Ok(bin) = codec.encode(codec.string_encoder(), "trigger")
  process.send(global.subject(started.data), bin)

  let assert Ok(err) = process.receive(error_subj, 500)
  should.equal(err, codec.DecodeFailed("intentional"))
}

// ---------------------------------------------------------------------------
// Payload size enforcement. actor handler + selector paths
// ---------------------------------------------------------------------------

pub fn start_receiver_observed_rejects_oversized_payload_test() {
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_payload_size_bytes: 4,
      ),
    )

  let error_subj = process.new_subject()
  let handler_subj = process.new_subject()

  let assert Ok(subj) =
    receiver.start_receiver_observed(
      Nil,
      codec.int_decoder(),
      fn(_msg, state) {
        process.send(handler_subj, Nil)
        receiver.Continue(state)
      },
      fn(err) { process.send(error_subj, err) },
    )

  // Encoded int = 8 bytes, > limit 4
  let assert Ok(bin) = codec.encode(codec.int_encoder(), 99)
  process.send(subj, bin)

  // Capture results BEFORE asserting so config is always restored.
  let err_result = process.receive(error_subj, 500)
  let handler_result = process.receive(handler_subj, 100)

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())

  let assert Ok(codec.PayloadTooLarge(size)) = err_result
  should.equal(size, 8)
  should.be_error(handler_result)
}

pub fn start_distributed_worker_observed_rejects_oversized_payload_test() {
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_payload_size_bytes: 4,
      ),
    )

  let error_subj = process.new_subject()
  let handler_subj = process.new_subject()

  let assert Ok(started) =
    receiver.start_distributed_worker_observed(
      "obs_oversized_" <> test_helpers.unique_id(),
      Nil,
      codec.int_encoder(),
      codec.int_decoder(),
      fn(_msg, state) {
        process.send(handler_subj, Nil)
        receiver.Continue(state)
      },
      fn(err) { process.send(error_subj, err) },
      5000,
    )

  // Encoded int = 8 bytes, > limit 4
  let assert Ok(bin) = codec.encode(codec.int_encoder(), 42)
  process.send(global.subject(started.data), bin)

  let err_result = process.receive(error_subj, 500)
  let handler_result = process.receive(handler_subj, 100)

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())

  let assert Ok(codec.PayloadTooLarge(size)) = err_result
  should.equal(size, 8)
  should.be_error(handler_result)
}

pub fn selecting_typed_rejects_oversized_payload_test() {
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_payload_size_bytes: 4,
      ),
    )

  let subj = process.new_subject()
  let selector =
    process.new_selector()
    |> receiver.selecting_typed(subj, codec.int_decoder(), fn(r) { r })

  let assert Ok(bin) = codec.encode(codec.int_encoder(), 7)
  process.send(subj, bin)

  let recv_result = process.selector_receive(selector, 500)

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())

  let assert Ok(Error(receiver.DecodeError(codec.PayloadTooLarge(size)))) =
    recv_result
  should.equal(size, 8)
}

pub fn start_receiver_observed_accepts_within_limit_test() {
  // Sanity check: under-limit payload still reaches the handler.
  let result_subj = process.new_subject()

  let assert Ok(subj) =
    receiver.start_receiver_observed(
      0,
      codec.int_decoder(),
      fn(msg, _state) {
        process.send(result_subj, msg)
        receiver.Continue(msg)
      },
      fn(_) { Nil },
    )

  let assert Ok(bin) = codec.encode(codec.int_encoder(), 13)
  process.send(subj, bin)

  let assert Ok(received) = process.receive(result_subj, 500)
  should.equal(received, 13)
}

// ---------------------------------------------------------------------------
// Anti-DoS drain: any process can `erlang:send` raw garbage to a
// globally-registered actor's PID. Without `select_other`, those terms
// would accumulate in the mailbox forever and slow every subsequent
// receive (gleam/otp/actor does NOT drop unmatched messages). The actor
// must drain garbage actively and stay responsive on its real Subject.
// ---------------------------------------------------------------------------

pub fn distributed_worker_drains_garbage_test() {
  let result_subj = process.new_subject()

  let assert Ok(started) =
    receiver.start_distributed_worker(
      "drain_dist_" <> test_helpers.unique_id(),
      0,
      codec.int_encoder(),
      codec.int_decoder(),
      fn(msg, _state) {
        process.send(result_subj, msg)
        receiver.Continue(msg)
      },
      5000,
    )

  // Bombard the worker's PID with foreign terms.
  inject_garbage_term(started.pid)
  inject_garbage_term(started.pid)
  inject_garbage_term(started.pid)

  // Real Subject message must still be processed.
  let assert Ok(Nil) = global.send(started.data, 7)
  let assert Ok(received) = process.receive(result_subj, 500)
  should.equal(received, 7)
}

pub fn local_receiver_drains_garbage_test() {
  let result_subj = process.new_subject()

  let assert Ok(subj) =
    receiver.start_receiver(0, codec.int_decoder(), fn(msg, _state) {
      process.send(result_subj, msg)
      receiver.Continue(msg)
    })

  let assert Ok(owner_pid) = process.subject_owner(subj)
  inject_garbage_term(owner_pid)
  inject_garbage_term(owner_pid)

  let assert Ok(bin) = codec.encode(codec.int_encoder(), 11)
  process.send(subj, bin)
  let assert Ok(received) = process.receive(result_subj, 500)
  should.equal(received, 11)
}

@external(erlang, "cluster_monitor_test_ffi", "send_garbage_term")
fn inject_garbage_term(pid: process.Pid) -> Nil

// ---------------------------------------------------------------------------
// Type renames (v4.0.0): `Next` → `HandlerStep`, `Timeout` → `ReceiveTimeout`.
// These names no longer collide with `gleam/otp/actor.Next` or
// `distribute/global.CallError.Timeout` when both are imported unqualified.
// ---------------------------------------------------------------------------

pub fn handler_step_type_compiles_test() {
  // Type-level usage. if this compiles, the type exists with the
  // right shape. Pattern-matching on the result avoids the
  // "constructed but never used" compiler warning.
  let handler: fn(Int, Int) -> receiver.HandlerStep(Int) = fn(_msg, state) {
    receiver.Continue(state)
  }
  let receiver.Continue(s) = handler(1, 2)
  should.equal(s, 2)
}

pub fn receive_timeout_variant_compiles_test() {
  // Exhaustive match through a fn parameter so the compiler cannot
  // narrow the type and warn about "unreachable" branches.
  let classify = fn(err: receiver.ReceiveError) -> Bool {
    case err {
      receiver.ReceiveTimeout -> True
      receiver.DecodeError(_) -> True
    }
  }
  should.be_true(classify(receiver.ReceiveTimeout))
  should.be_true(classify(receiver.DecodeError(codec.DecodeTimeout)))
}

pub fn selecting_typed_test() {
  let subj = process.new_subject()
  let enc = codec.string_encoder()
  let dec = codec.string_decoder()

  let selector =
    process.new_selector()
    |> receiver.selecting_typed(subj, dec, fn(result) {
      case result {
        Ok(val) -> val
        Error(_) -> "error"
      }
    })

  // Send a valid message
  let assert Ok(bin) = codec.encode(enc, "test-select")
  process.send(subj, bin)

  let val = process.selector_receive(selector, 1000)
  should.equal(val, Ok("test-select"))
}

// ---------------------------------------------------------------------------
// Z5. garbage-flood resilience
//
// `select_other` drains foreign mailbox terms in a single reduction
// each, so a hostile peer that floods the actor's PID with raw
// `erlang:send` cannot accumulate orphans and degrade the selective-
// receive performance for legit traffic.
//
// We bombard the actor with N garbage terms, then send a legit
// message and assert that:
//   * the actor still processes the legit message (selector matched);
//   * latency between legit-send and handler-execution stays bounded
//     (here: well under 1s on dev hardware for N=1_000).
// ---------------------------------------------------------------------------

pub fn z5_garbage_flood_does_not_starve_legit_traffic_test() {
  let result_subj = process.new_subject()

  let assert Ok(started) =
    receiver.start_distributed_worker(
      "z5_flood_" <> test_helpers.unique_id(),
      0,
      codec.int_encoder(),
      codec.int_decoder(),
      fn(msg, _state) {
        process.send(result_subj, msg)
        receiver.Continue(msg)
      },
      5000,
    )

  // Flood 1_000 raw garbage terms.
  z5_flood(started.pid, 1000)

  // Legit message must still be processed promptly.
  let assert Ok(bin) = codec.encode(codec.int_encoder(), 99)
  process.send(global.subject(started.data), bin)

  let assert Ok(received) = process.receive(result_subj, 1000)
  should.equal(received, 99)
}

fn z5_flood(pid: process.Pid, remaining: Int) -> Nil {
  case remaining {
    0 -> Nil
    _ -> {
      inject_garbage_term(pid)
      z5_flood(pid, remaining - 1)
    }
  }
}
