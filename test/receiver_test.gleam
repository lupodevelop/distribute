import distribute/codec
import distribute/receiver
import gleam/erlang/process
import gleeunit/should

pub fn receive_typed_timeout_test() {
  let subject = process.new_subject()
  let decoder = codec.string_decoder()

  // No message sent, should timeout
  case receiver.receive_typed(subject, decoder, 10) {
    Error(receiver.Timeout) -> Nil
    _ -> panic as "Expected timeout"
  }
}

pub fn receive_typed_success_test() {
  let subject = process.new_subject()
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()

  // Send a message
  let assert Ok(encoded) = encoder("Hello, World!")
  process.send(subject, encoded)

  // Receive it
  case receiver.receive_typed(subject, decoder, 100) {
    Ok(msg) -> should.equal(msg, "Hello, World!")
    Error(_) -> panic as "Expected success"
  }
}

pub fn receive_typed_decode_error_test() {
  let subject = process.new_subject()
  let decoder = codec.int_decoder()

  // Send invalid data (not a proper int encoding)
  process.send(subject, <<99, 88, 77>>)

  // Should get decode error
  case receiver.receive_typed(subject, decoder, 100) {
    Error(receiver.DecodeError(_)) -> Nil
    _ -> panic as "Expected decode error"
  }
}

pub fn receive_typed_multiple_messages_test() {
  let subject = process.new_subject()
  let encoder = codec.int_encoder()
  let decoder = codec.int_decoder()

  // Send three messages
  let assert Ok(msg1) = encoder(1)
  let assert Ok(msg2) = encoder(2)
  let assert Ok(msg3) = encoder(3)
  process.send(subject, msg1)
  process.send(subject, msg2)
  process.send(subject, msg3)

  // Receive them in order
  let assert Ok(val1) = receiver.receive_typed(subject, decoder, 100)
  let assert Ok(val2) = receiver.receive_typed(subject, decoder, 100)
  let assert Ok(val3) = receiver.receive_typed(subject, decoder, 100)

  should.equal(val1, 1)
  should.equal(val2, 2)
  should.equal(val3, 3)
}
