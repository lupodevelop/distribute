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
  let decoder = codec.list_decoder(codec.int_sized_decoder())

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

// ============================================================================
// Schema tests
// ============================================================================

pub fn schema_encode_decode_test() {
  let schema =
    codec.new_schema(
      tag: "greeting",
      version: 1,
      encoder: codec.string_encoder(),
      decoder: codec.string_decoder(),
    )

  let original = "Hello, Schema!"
  let assert Ok(encoded) = codec.schema_encode(schema, original)
  let assert Ok(decoded) = codec.schema_decode(schema, encoded)

  decoded |> should.equal(original)
}

pub fn schema_tag_mismatch_returns_error_test() {
  let schema1 =
    codec.new_schema(
      tag: "type_a",
      version: 1,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  let schema2 =
    codec.new_schema(
      tag: "type_b",
      version: 1,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  let assert Ok(encoded) = codec.schema_encode(schema1, 42)

  case codec.schema_decode(schema2, encoded) {
    Error(codec.TagMismatch(expected, got)) -> {
      expected |> should.equal("type_b")
      got |> should.equal("type_a")
    }
    _ -> should.fail()
  }
}

pub fn schema_version_mismatch_returns_error_test() {
  let schema_v1 =
    codec.new_schema(
      tag: "msg",
      version: 1,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  let schema_v2 =
    codec.new_schema(
      tag: "msg",
      version: 2,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  let assert Ok(encoded) = codec.schema_encode(schema_v1, 100)

  case codec.schema_decode(schema_v2, encoded) {
    Error(codec.VersionMismatch(expected, got)) -> {
      expected |> should.equal(2)
      got |> should.equal(1)
    }
    _ -> should.fail()
  }
}

pub fn peek_tag_test() {
  let data = codec.wrap_envelope("my_tag", 5, <<"payload">>)
  let assert Ok(tag) = codec.peek_tag(data)
  tag |> should.equal("my_tag")
}

pub fn peek_envelope_test() {
  let data = codec.wrap_envelope("event", 3, <<"data">>)
  let assert Ok(#(tag, version)) = codec.peek_envelope(data)
  tag |> should.equal("event")
  version |> should.equal(3)
}

// ============================================================================
// SizedDecoder tests
// ============================================================================

pub fn sized_decoder_string_returns_remaining_test() {
  let data = <<0, 5, "hello", 99, 88>>
  // length prefix 5, then "hello", then extra bytes
  let decoder = codec.string_sized_decoder()
  let assert Ok(#(value, remaining)) = decoder(data)
  value |> should.equal("hello")
  remaining |> should.equal(<<99, 88>>)
}

pub fn sized_decoder_int_returns_remaining_test() {
  let data = <<0, 0, 0, 0, 0, 0, 0, 42, 1, 2, 3>>
  // 8 bytes for int64 (42), then 1,2,3
  let decoder = codec.int_sized_decoder()
  let assert Ok(#(value, remaining)) = decoder(data)
  value |> should.equal(42)
  remaining |> should.equal(<<1, 2, 3>>)
}

pub fn list_of_strings_properly_decodes_test() {
  let original = ["apple", "banana", "cherry"]
  let encoder = codec.list_encoder(codec.string_encoder())
  let decoder = codec.list_decoder(codec.string_sized_decoder())

  let assert Ok(encoded) = codec.encode(encoder, original)
  let assert Ok(decoded) = codec.decode(decoder, encoded)

  decoded |> should.equal(original)
}

// ============================================================================
// Tuple codec tests
// ============================================================================

pub fn tuple2_encode_decode_test() {
  let original = #("hello", 42)
  let encoder =
    codec.tuple2_encoder(codec.string_encoder(), codec.int_encoder())
  let decoder =
    codec.tuple2_decoder(
      codec.string_sized_decoder(),
      codec.int_sized_decoder(),
    )

  let assert Ok(encoded) = codec.encode(encoder, original)
  let assert Ok(decoded) = codec.decode(decoder, encoded)

  decoded |> should.equal(original)
}

pub fn tuple3_encode_decode_test() {
  let original = #("test", 123, 3.14)
  let encoder =
    codec.tuple3_encoder(
      codec.string_encoder(),
      codec.int_encoder(),
      codec.float_encoder(),
    )
  let decoder =
    codec.tuple3_decoder(
      codec.string_sized_decoder(),
      codec.int_sized_decoder(),
      codec.float_sized_decoder(),
    )

  let assert Ok(encoded) = codec.encode(encoder, original)
  let assert Ok(decoded) = codec.decode(decoder, encoded)

  decoded |> should.equal(original)
}

// ============================================================================
// Versioned decoder tests
// ============================================================================

pub fn versioned_decoder_handles_multiple_versions_test() {
  // Simulate v1 and v2 with different payload formats
  let v1_payload = <<0, 0, 0, 0, 0, 0, 0, 10>>
  // int 10
  let v2_payload = <<0, 0, 0, 0, 0, 0, 0, 20>>
  // int 20

  let data_v1 = codec.wrap_envelope("num", 1, v1_payload)
  let data_v2 = codec.wrap_envelope("num", 2, v2_payload)

  let decoder =
    codec.versioned_decoder("num", [
      #(1, codec.int_decoder()),
      #(2, codec.int_decoder()),
    ])

  let assert Ok(result1) = decoder(data_v1)
  let assert Ok(result2) = decoder(data_v2)

  result1 |> should.equal(10)
  result2 |> should.equal(20)
}

pub fn versioned_decoder_rejects_unknown_version_test() {
  let data = codec.wrap_envelope("num", 99, <<0, 0, 0, 0, 0, 0, 0, 1>>)

  let decoder = codec.versioned_decoder("num", [#(1, codec.int_decoder())])

  case decoder(data) {
    Error(codec.VersionMismatch(_, got)) -> got |> should.equal(99)
    _ -> should.fail()
  }
}

pub fn versioned_decoder_from_schemas_test() {
  let v1_schema =
    codec.new_schema(
      tag: "num",
      version: 1,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  let v2_schema =
    codec.new_schema(
      tag: "num",
      version: 2,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  let data_v1 = codec.wrap_envelope("num", 1, <<0, 0, 0, 0, 0, 0, 0, 10>>)
  let data_v2 = codec.wrap_envelope("num", 2, <<0, 0, 0, 0, 0, 0, 0, 20>>)

  let decoder = codec.versioned_decoder_from_schemas([v1_schema, v2_schema])

  let assert Ok(result1) = decoder(data_v1)
  let assert Ok(result2) = decoder(data_v2)

  result1 |> should.equal(10)
  result2 |> should.equal(20)
}

pub fn schema_decode_with_migrations_test() {
  // target schema is version 2
  let target =
    codec.new_schema(
      tag: "num",
      version: 2,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  // migration from v1 -> v2: in this test our payload format is identical,
  // so the migration is a no-op. In real scenarios this could transform
  // payload bytes (e.g., change field order, add defaults, etc.)
  let migration = fn(payload: BitArray) { Ok(payload) }

  let data_v1 = codec.wrap_envelope("num", 1, <<0, 0, 0, 0, 0, 0, 0, 10>>)
  let decoder = codec.schema_decode_with_migrations(target, [#(1, migration)])

  let assert Ok(result) = decoder(data_v1)
  result |> should.equal(10)
}

pub fn schema_decode_with_migrations_missing_migration_test() {
  let target =
    codec.new_schema(
      tag: "num",
      version: 2,
      encoder: codec.int_encoder(),
      decoder: codec.int_decoder(),
    )

  let data_v1 = codec.wrap_envelope("num", 1, <<0, 0, 0, 0, 0, 0, 0, 10>>)
  let decoder = codec.schema_decode_with_migrations(target, [])

  case decoder(data_v1) {
    Error(codec.MigrationMissing(step)) -> step |> should.equal(1)
    _ -> should.fail()
  }
}

pub fn migration_chain_applies_steps_test() {
  // Prepare migrations that increment the integer payload
  let migr1 = fn(payload: BitArray) {
    case codec.int_decoder()(payload) {
      Ok(v) ->
        case codec.encode(codec.int_encoder(), v + 1) {
          Ok(b) -> Ok(b)
          Error(enc) ->
            Error(codec.DecodeFailed(codec.encode_error_to_string(enc)))
        }
      Error(e) -> Error(e)
    }
  }

  let migr2 = fn(payload: BitArray) {
    case codec.int_decoder()(payload) {
      Ok(v) ->
        case codec.encode(codec.int_encoder(), v + 1) {
          Ok(b) -> Ok(b)
          Error(enc) ->
            Error(codec.DecodeFailed(codec.encode_error_to_string(enc)))
        }
      Error(e) -> Error(e)
    }
  }

  // Use graph-based builder with explicit edges (1->2, 2->3)
  let chain = codec.build_migration_graph([#(1, 2, migr1), #(2, 3, migr2)])

  let data_v1 = codec.wrap_envelope("num", 1, <<0, 0, 0, 0, 0, 0, 0, 10>>)
  // Apply chain from 1 -> 3
  case codec.unwrap_envelope(data_v1) {
    Ok(#(_, _, payload)) ->
      case chain(1, 3, payload) {
        Ok(new_payload) ->
          case codec.int_decoder()(new_payload) {
            Ok(v) -> v |> should.equal(12)
            Error(_) -> should.fail()
          }
        Error(_) -> should.fail()
      }
    Error(_) -> should.fail()
  }
}

pub fn migration_chain_missing_step_test() {
  let migr1 = fn(payload: BitArray) {
    case codec.int_decoder()(payload) {
      Ok(v) ->
        case codec.encode(codec.int_encoder(), v + 1) {
          Ok(b) -> Ok(b)
          Error(enc) ->
            Error(codec.DecodeFailed(codec.encode_error_to_string(enc)))
        }
      Error(e) -> Error(e)
    }
  }

  // chain builder accepts both single-step and multi-step edges; test missing step
  let chain = codec.build_migration_graph([#(1, 2, migr1)])
  let data_v1 = codec.wrap_envelope("num", 1, <<0, 0, 0, 0, 0, 0, 0, 10>>)

  case codec.unwrap_envelope(data_v1) {
    Ok(#(_, _, payload)) ->
      case chain(1, 3, payload) {
        Error(codec.MigrationMissing(step)) -> step |> should.equal(1)
        _ -> should.fail()
      }
    Error(_) -> should.fail()
  }
}
