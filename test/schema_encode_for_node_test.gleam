import distribute/capability.{Capability}
import distribute/codec
import distribute/handshake/negotiation
import distribute/registry/actor as registry
import distribute/registry/behaviour
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// schema_encode_for_node / schema_decode_from_node integration tests
// ============================================================================

// Test message type
pub type TestMessage {
  TestMessage(content: String, counter: Int)
}

// Encoder/decoder for TestMessage v1 (uses built-in codecs)
fn test_encoder_v1() -> codec.Encoder(TestMessage) {
  fn(msg) {
    case msg {
      TestMessage(content, counter) -> {
        // Encode as tuple of (String, Int)
        let tuple_encoder =
          codec.tuple2_encoder(codec.string_encoder(), codec.int_encoder())
        codec.encode(tuple_encoder, #(content, counter))
      }
    }
  }
}

fn test_decoder_v1() -> codec.Decoder(TestMessage) {
  fn(data) {
    let tuple_decoder =
      codec.tuple2_decoder(
        codec.string_sized_decoder(),
        codec.int_sized_decoder(),
      )
    case codec.decode(tuple_decoder, data) {
      Ok(#(content, counter)) -> Ok(TestMessage(content, counter))
      Error(e) -> Error(e)
    }
  }
}

// Encoder/decoder for TestMessage v2 (adds prefix to content)
fn test_encoder_v2() -> codec.Encoder(TestMessage) {
  fn(msg) {
    case msg {
      TestMessage(content, counter) -> {
        let prefixed = "v2:" <> content
        let tuple_encoder =
          codec.tuple2_encoder(codec.string_encoder(), codec.int_encoder())
        codec.encode(tuple_encoder, #(prefixed, counter))
      }
    }
  }
}

fn test_decoder_v2() -> codec.Decoder(TestMessage) {
  fn(data) {
    let tuple_decoder =
      codec.tuple2_decoder(
        codec.string_sized_decoder(),
        codec.int_sized_decoder(),
      )
    case codec.decode(tuple_decoder, data) {
      Ok(#(content, counter)) -> {
        // Remove v2: prefix if present
        let clean_content = case content {
          "v2:" <> rest -> rest
          _ -> content
        }
        Ok(TestMessage(clean_content, counter))
      }
      Error(e) -> Error(e)
    }
  }
}

pub fn encode_decode_for_node_test() {
  // Start registry
  let assert Ok(reg) = registry.start()

  // Create schemas for different versions
  let schema_v1 =
    codec.new_schema(
      tag: "test_msg",
      version: 1,
      encoder: test_encoder_v1(),
      decoder: test_decoder_v1(),
    )
  let schema_v2 =
    codec.new_schema(
      tag: "test_msg",
      version: 2,
      encoder: test_encoder_v2(),
      decoder: test_decoder_v2(),
    )
  let schemas = [schema_v1, schema_v2]

  // Register node with negotiated version 1
  let node_id = "test_node@host"
  let metadata =
    behaviour.Metadata(
      node_id: node_id,
      capabilities: [Capability("test_protocol", 1, 2)],
      extra: "test_protocol:1",
    )

  let assert Ok(_) = registry.register_sync(reg, 5000, node_id, metadata)

  // Encode message for node (should use v1)
  let msg = TestMessage("hello", 42)
  let assert Ok(encoded) =
    negotiation.schema_encode_for_node(
      reg,
      5000,
      node_id,
      "test_protocol",
      schemas,
      msg,
    )

  // Decode message from node (should use v1)
  let assert Ok(decoded) =
    negotiation.schema_decode_from_node(
      reg,
      5000,
      node_id,
      "test_protocol",
      schemas,
      encoded,
    )

  decoded.content
  |> should.equal("hello")
  decoded.counter
  |> should.equal(42)
}

pub fn encode_for_node_version_2_test() {
  let assert Ok(reg) = registry.start()

  let schema_v1 =
    codec.new_schema(
      tag: "test_msg",
      version: 1,
      encoder: test_encoder_v1(),
      decoder: test_decoder_v1(),
    )
  let schema_v2 =
    codec.new_schema(
      tag: "test_msg",
      version: 2,
      encoder: test_encoder_v2(),
      decoder: test_decoder_v2(),
    )
  let schemas = [schema_v1, schema_v2]

  // Register node with negotiated version 2
  let node_id = "test_node_v2@host"
  let metadata =
    behaviour.Metadata(
      node_id: node_id,
      capabilities: [Capability("test_protocol", 2, 2)],
      extra: "test_protocol:2",
    )

  let assert Ok(_) = registry.register_sync(reg, 5000, node_id, metadata)

  // Encode message for node (should use v2)
  let msg = TestMessage("world", 99)
  let assert Ok(encoded) =
    negotiation.schema_encode_for_node(
      reg,
      5000,
      node_id,
      "test_protocol",
      schemas,
      msg,
    )

  // Decode should use v2 decoder
  let assert Ok(decoded) =
    negotiation.schema_decode_from_node(
      reg,
      5000,
      node_id,
      "test_protocol",
      schemas,
      encoded,
    )

  decoded.content
  |> should.equal("world")
  decoded.counter
  |> should.equal(99)
}

pub fn encode_for_node_not_found_test() {
  let assert Ok(reg) = registry.start()

  let schema_v1 =
    codec.new_schema(
      tag: "test_msg",
      version: 1,
      encoder: test_encoder_v1(),
      decoder: test_decoder_v1(),
    )
  let schemas = [schema_v1]

  // Try to encode for unknown node
  let msg = TestMessage("test", 1)
  let result =
    negotiation.schema_encode_for_node(
      reg,
      5000,
      "unknown_node@host",
      "test_protocol",
      schemas,
      msg,
    )

  result
  |> should.be_error()
}

pub fn encode_for_node_no_negotiated_version_test() {
  let assert Ok(reg) = registry.start()

  let schema_v1 =
    codec.new_schema(
      tag: "test_msg",
      version: 1,
      encoder: test_encoder_v1(),
      decoder: test_decoder_v1(),
    )
  let schemas = [schema_v1]

  // Register node WITHOUT negotiated version for our protocol
  let node_id = "test_node@host"
  let metadata =
    behaviour.Metadata(
      node_id: node_id,
      capabilities: [Capability("other_protocol", 1, 1)],
      extra: "other_protocol:1",
    )

  let assert Ok(_) = registry.register_sync(reg, 5000, node_id, metadata)

  // Try to encode for protocol that wasn't negotiated
  let msg = TestMessage("test", 1)
  let result =
    negotiation.schema_encode_for_node(
      reg,
      5000,
      node_id,
      "test_protocol",
      schemas,
      msg,
    )

  result
  |> should.be_error()
}

pub fn encode_for_node_multiple_protocols_test() {
  let assert Ok(reg) = registry.start()

  let schema_v1 =
    codec.new_schema(
      tag: "test_msg",
      version: 1,
      encoder: test_encoder_v1(),
      decoder: test_decoder_v1(),
    )
  let schema_v2 =
    codec.new_schema(
      tag: "test_msg",
      version: 2,
      encoder: test_encoder_v2(),
      decoder: test_decoder_v2(),
    )
  let schemas = [schema_v1, schema_v2]

  // Register node with multiple negotiated protocols
  let node_id = "multi_proto_node@host"
  let metadata =
    behaviour.Metadata(
      node_id: node_id,
      capabilities: [
        Capability("proto_a", 1, 2),
        Capability("proto_b", 3, 5),
      ],
      extra: "proto_a:1,proto_b:2",
    )

  let assert Ok(_) = registry.register_sync(reg, 5000, node_id, metadata)

  // Encode for proto_a (version 1)
  let msg = TestMessage("proto_a_msg", 10)
  let assert Ok(encoded_a) =
    negotiation.schema_encode_for_node(
      reg,
      5000,
      node_id,
      "proto_a",
      schemas,
      msg,
    )

  // Verify we can decode it
  let assert Ok(decoded_a) =
    negotiation.schema_decode_from_node(
      reg,
      5000,
      node_id,
      "proto_a",
      schemas,
      encoded_a,
    )

  decoded_a.content
  |> should.equal("proto_a_msg")
  decoded_a.counter
  |> should.equal(10)
}
