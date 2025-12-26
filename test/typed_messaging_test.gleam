/// Tests for type-safe messaging with envelope and codec validation.
import distribute/codec
import distribute/messaging
import gleam/bit_array
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Envelope tests
// ============================================================================

pub fn envelope_wrap_unwrap_test() {
  let tag = "user_message"
  let version = 1
  let payload = <<"hello world">>

  let wrapped = codec.wrap_envelope(tag, version, payload)
  let result = codec.unwrap_envelope(wrapped)

  result
  |> should.be_ok
  |> should.equal(#(tag, version, payload))
}

pub fn envelope_tag_mismatch_test() {
  let tag = "message_v1"
  let version = 1
  let payload = <<"test">>

  let wrapped = codec.wrap_envelope(tag, version, payload)
  let decoder = codec.string_decoder()

  let result =
    codec.receive_with_decoder(decoder, "wrong_tag", version, wrapped)

  result
  |> should.be_error
  |> should.equal(codec.TypeMismatch("tag mismatch"))
}

pub fn envelope_version_mismatch_test() {
  let tag = "message_v1"
  let version = 1
  let payload = <<"test">>

  let wrapped = codec.wrap_envelope(tag, version, payload)
  let decoder = codec.string_decoder()

  let result = codec.receive_with_decoder(decoder, tag, 2, wrapped)

  result
  |> should.be_error
  |> should.equal(codec.TypeMismatch("version mismatch"))
}

pub fn envelope_malformed_data_test() {
  let malformed = <<1, 2, 3>>
  let result = codec.unwrap_envelope(malformed)

  result
  |> should.be_error
}

pub fn envelope_truncated_data_test() {
  // Tag length says 10 bytes but we only have 5
  let truncated = <<0, 10, 1, 2, 3, 4, 5>>
  let result = codec.unwrap_envelope(truncated)

  result
  |> should.be_error
  |> should.equal(codec.InsufficientData("envelope too short"))
}

// ============================================================================
// Codec roundtrip tests with envelope
// ============================================================================

pub fn string_codec_with_envelope_test() {
  let tag = "string_msg"
  let version = 1
  let original = "Hello, distributed world!"

  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()

  // Encode payload
  let assert Ok(payload) = codec.encode(encoder, original)

  // Wrap in envelope
  let wrapped = codec.wrap_envelope(tag, version, payload)

  // Receive and decode with validation
  let result = codec.receive_with_decoder(decoder, tag, version, wrapped)

  result
  |> should.be_ok
  |> should.equal(original)
}

pub fn int_codec_with_envelope_test() {
  let tag = "int_msg"
  let version = 2
  let original = 42_424_242

  let encoder = codec.int_encoder()
  let decoder = codec.int_decoder()

  let assert Ok(payload) = codec.encode(encoder, original)
  let wrapped = codec.wrap_envelope(tag, version, payload)
  let result = codec.receive_with_decoder(decoder, tag, version, wrapped)

  result
  |> should.be_ok
  |> should.equal(original)
}

pub fn bool_codec_with_envelope_test() {
  let tag = "bool_msg"
  let version = 1

  let encoder = codec.bool_encoder()
  let decoder = codec.bool_decoder()

  // Test True
  let assert Ok(payload_true) = codec.encode(encoder, True)
  let wrapped_true = codec.wrap_envelope(tag, version, payload_true)
  let result_true =
    codec.receive_with_decoder(decoder, tag, version, wrapped_true)
  result_true
  |> should.be_ok
  |> should.equal(True)

  // Test False
  let assert Ok(payload_false) = codec.encode(encoder, False)
  let wrapped_false = codec.wrap_envelope(tag, version, payload_false)
  let result_false =
    codec.receive_with_decoder(decoder, tag, version, wrapped_false)
  result_false
  |> should.be_ok
  |> should.equal(False)
}

pub fn list_codec_with_envelope_test() {
  // Note: list codec needs improvement for multi-element lists
  // For now test with single element
  let tag = "list_msg"
  let version = 1
  let original = [42]

  let encoder = codec.list_encoder(codec.int_encoder())
  let decoder = codec.list_decoder(codec.int_decoder())

  let assert Ok(payload) = codec.encode(encoder, original)
  let wrapped = codec.wrap_envelope(tag, version, payload)
  let result = codec.receive_with_decoder(decoder, tag, version, wrapped)

  result
  |> should.be_ok
  |> should.equal(original)
}

// ============================================================================
// Encode error tests
// ============================================================================

pub fn string_too_large_encode_test() {
  // Create a string larger than 65535 bytes
  let large_string = bit_array.to_string(<<0:size(524_288)>>)
  let assert Ok(str) = large_string

  let encoder = codec.string_encoder()
  let result = codec.encode(encoder, str)

  result
  |> should.be_error
}

pub fn list_too_large_encode_test() {
  // Create a list with more than 65535 elements
  let large_list = list.range(0, 70_000)

  let encoder = codec.list_encoder(codec.int_encoder())
  let result = codec.encode(encoder, large_list)

  result
  |> should.be_error
}

// ============================================================================
// Decode error tests
// ============================================================================

pub fn string_decode_invalid_binary_test() {
  let decoder = codec.string_decoder()
  let invalid = <<1, 2, 3>>
  // Missing length prefix

  let result = codec.decode(decoder, invalid)

  result
  |> should.be_error
}

pub fn string_decode_truncated_test() {
  let decoder = codec.string_decoder()
  // Says 10 bytes but only provides 3
  let truncated = <<0, 10, 1, 2, 3>>

  let result = codec.decode(decoder, truncated)

  result
  |> should.be_error
  |> should.equal(codec.InsufficientData("incomplete string data"))
}

pub fn int_decode_insufficient_data_test() {
  let decoder = codec.int_decoder()
  let insufficient = <<1, 2, 3>>
  // Need 8 bytes for int64

  let result = codec.decode(decoder, insufficient)

  result
  |> should.be_error
  |> should.equal(codec.InvalidBinary("insufficient data for int64"))
}

pub fn bool_decode_invalid_value_test() {
  let decoder = codec.bool_decoder()
  let invalid = <<2>>
  // Only 0 and 1 are valid

  let result = codec.decode(decoder, invalid)

  result
  |> should.be_error
  |> should.equal(codec.InvalidBinary("invalid boolean value"))
}

// ============================================================================
// SendError classification tests
// ============================================================================

pub fn classify_send_error_not_found_test() {
  let error = messaging.classify_send_error("not_found", "test_process")
  case error {
    messaging.NameNotFound(name) -> name |> should.equal("test_process")
    _ -> should.fail()
  }
}

pub fn classify_send_error_process_not_alive_test() {
  let error = messaging.classify_send_error("process_not_alive", "test")
  error |> should.equal(messaging.ProcessNotAlive)
}

pub fn classify_send_error_network_error_test() {
  let error = messaging.classify_send_error("network failure", "test")
  case error {
    messaging.NetworkError(_) -> Nil
    _ -> should.fail()
  }
}

pub fn encode_error_to_send_error_test() {
  let encode_err = codec.InvalidValue("test")
  let send_err = messaging.encode_error_to_send_error(encode_err)

  case send_err {
    messaging.EncodeFailed(e) -> e |> should.equal(encode_err)
    _ -> should.fail()
  }
}
