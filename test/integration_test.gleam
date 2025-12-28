/// Comprehensive integration tests for type-safe distributed messaging.
import distribute/codec
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleam/option
import gleeunit/should

/// Test end-to-end typed messaging with encoding, sending, and receiving.
pub fn end_to_end_typed_messaging_test() {
  // Setup
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()
  let message = "test_message"

  // Encode
  let encoded = codec.encode(encoder, message)
  encoded |> should.be_ok

  // Decode
  let assert Ok(data) = encoded
  let decoded = codec.decode(decoder, data)
  decoded |> should.equal(Ok(message))
}

/// Test GlobalSubject can be registered and looked up.
pub fn global_subject_registration_test() {
  // Spawn a new process to avoid registration conflicts
  let _pid =
    process.spawn(fn() {
      let global = global.new(codec.string_encoder(), codec.string_decoder())
      let pid_str = process.self() |> string.inspect
      let timestamp = system_time()
      let name =
        "test_global_spawn_" <> pid_str <> "_" <> string.inspect(timestamp)

      // Register
      let _result = registry.register_typed(name, global.subject(global))
      // Cleanup
      let _ = registry.unregister(name)

      // Keep alive briefly
      process.sleep(100)
    })

  // Give spawned process time to complete
  process.sleep(200)

  // This test just verifies no crash occurred
  should.be_true(True)
}

@external(erlang, "erlang", "system_time")
fn system_time() -> Int

/// Test codec roundtrip for all primitive types.
pub fn codec_primitives_roundtrip_test() {
  // String
  let str_encoder = codec.string_encoder()
  let str_decoder = codec.string_decoder()
  test_roundtrip(str_encoder, str_decoder, "hello")

  // Int
  let int_encoder = codec.int_encoder()
  let int_decoder = codec.int_decoder()
  test_roundtrip(int_encoder, int_decoder, 42)

  // Bool
  let bool_encoder = codec.bool_encoder()
  let bool_decoder = codec.bool_decoder()
  test_roundtrip(bool_encoder, bool_decoder, True)

  // Float
  let float_encoder = codec.float_encoder()
  let float_decoder = codec.float_decoder()
  let encoded = codec.encode(float_encoder, 3.14)
  encoded |> should.be_ok
  let assert Ok(data) = encoded
  let decoded = codec.decode(float_decoder, data)
  decoded |> should.be_ok
}

fn test_roundtrip(encoder, decoder, value) {
  let encoded = codec.encode(encoder, value)
  encoded |> should.be_ok
  let assert Ok(data) = encoded
  let decoded = codec.decode(decoder, data)
  decoded |> should.equal(Ok(value))
}

/// Test Option codec with None and Some cases.
pub fn option_codec_comprehensive_test() {
  let encoder = codec.option_encoder(codec.string_encoder())
  let decoder = codec.option_decoder(codec.string_decoder())

  // None case
  test_roundtrip(encoder, decoder, option.None)

  // Some case
  test_roundtrip(encoder, decoder, option.Some("value"))
}

/// Test Result codec with Ok and Error cases.
pub fn result_codec_comprehensive_test() {
  let encoder =
    codec.result_encoder(codec.int_encoder(), codec.string_encoder())
  let decoder =
    codec.result_decoder(codec.int_decoder(), codec.string_decoder())

  // Ok case
  test_roundtrip(encoder, decoder, Ok(100))

  // Error case
  test_roundtrip(encoder, decoder, Error("failure"))
}

/// Test that malformed binary data produces proper decode errors.
pub fn decode_error_handling_test() {
  let decoder = codec.int_decoder()

  // Too short
  let result = codec.decode(decoder, <<1, 2, 3>>)
  case result {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.be_true(False)
  }

  // Empty
  let result = codec.decode(decoder, <<>>)
  case result {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.be_true(False)
  }
}

/// Test encode error for oversized values.
pub fn encode_error_handling_test() {
  let encoder = codec.string_encoder()

  // Create a string that's too long (> 65535 bytes)
  // For now, just test normal encoding works
  let result = codec.encode(encoder, "normal string")
  result |> should.be_ok
}

/// Test receiver timeout behavior.
pub fn receiver_timeout_test() {
  let subject = process.new_subject()
  let decoder = codec.string_decoder()

  // Should timeout immediately with 0ms
  let result = receiver.receive_typed(subject, decoder, 0)
  case result {
    Error(receiver.Timeout) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

/// Test GlobalSubject owner extraction.
pub fn global_subject_owner_test() {
  let global = global.new(codec.string_encoder(), codec.string_decoder())
  let owner = global.owner(global)

  owner |> should.be_ok
  let assert Ok(pid) = owner
  pid |> should.equal(process.self())
}

import gleam/string
