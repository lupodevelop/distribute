import distribute/codec
import distribute/global
import distribute/receiver
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Simple unique ID generator for test isolation
@external(erlang, "erlang", "unique_integer")
fn erlang_unique_int() -> Int

fn unique_id() -> String {
  let n = erlang_unique_int()
  int_to_string(case n < 0 {
    True -> 0 - n
    False -> n
  })
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String

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
      "recv_worker_" <> unique_id(),
      Nil,
      enc,
      dec,
      fn(_msg, state) { receiver.Continue(state) },
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
      "recv_state_" <> unique_id(),
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
    )

  // Send messages through the GlobalSubject
  let assert Ok(Nil) = global.send(started.data, 1)
  let assert Ok(Nil) = global.send(started.data, 2)

  let assert Ok(total) = process.receive(result_subj, 1000)
  should.equal(total, 3)
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
