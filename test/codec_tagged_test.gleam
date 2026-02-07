import distribute/codec
import distribute/codec/tagged
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Tagged message construction and accessors
// ============================================================================

pub fn tagged_new_test() {
  let msg = tagged.new("hello", 1, "payload_data")
  should.equal(tagged.tag(msg), "hello")
  should.equal(tagged.version(msg), 1)
  should.equal(tagged.payload(msg), "payload_data")
}

pub fn tagged_new_empty_tag_test() {
  let msg = tagged.new("", 0, 42)
  should.equal(tagged.tag(msg), "")
  should.equal(tagged.version(msg), 0)
  should.equal(tagged.payload(msg), 42)
}

// ============================================================================
// Tagged encoder/decoder round-trip
// ============================================================================

pub fn tagged_roundtrip_string_test() {
  let payload_encoder = codec.string_encoder()
  let payload_decoder = codec.string_decoder()

  let msg = tagged.new("test_tag", 1, "hello world")
  let enc = tagged.encoder(payload_encoder)
  let dec = tagged.decoder("test_tag", 1, payload_decoder)

  let assert Ok(encoded) = codec.encode(enc, msg)
  let assert Ok(decoded) = codec.decode(dec, encoded)

  should.equal(tagged.tag(decoded), "test_tag")
  should.equal(tagged.version(decoded), 1)
  should.equal(tagged.payload(decoded), "hello world")
}

pub fn tagged_roundtrip_int_test() {
  let payload_encoder = codec.int_encoder()
  let payload_decoder = codec.int_decoder()

  let msg = tagged.new("counter", 2, 42)
  let enc = tagged.encoder(payload_encoder)
  let dec = tagged.decoder("counter", 2, payload_decoder)

  let assert Ok(encoded) = codec.encode(enc, msg)
  let assert Ok(decoded) = codec.decode(dec, encoded)

  should.equal(tagged.payload(decoded), 42)
}

// ============================================================================
// Tag mismatch detection
// ============================================================================

pub fn tagged_tag_mismatch_test() {
  let payload_encoder = codec.string_encoder()
  let payload_decoder = codec.string_decoder()

  let msg = tagged.new("actual_tag", 1, "data")
  let enc = tagged.encoder(payload_encoder)
  let dec = tagged.decoder("expected_tag", 1, payload_decoder)

  let assert Ok(encoded) = codec.encode(enc, msg)
  let result = codec.decode(dec, encoded)

  should.be_error(result)
}

// ============================================================================
// Version mismatch detection
// ============================================================================

pub fn tagged_version_mismatch_test() {
  let payload_encoder = codec.string_encoder()
  let payload_decoder = codec.string_decoder()

  let msg = tagged.new("tag", 2, "data")
  let enc = tagged.encoder(payload_encoder)
  let dec = tagged.decoder("tag", 1, payload_decoder)

  let assert Ok(encoded) = codec.encode(enc, msg)
  let result = codec.decode(dec, encoded)

  should.be_error(result)
}

// ============================================================================
// Malformed input handling
// ============================================================================

pub fn tagged_decode_empty_input_test() {
  let payload_decoder = codec.string_decoder()
  let dec = tagged.decoder("tag", 1, payload_decoder)

  let result = codec.decode(dec, <<>>)
  should.be_error(result)
}

pub fn tagged_decode_truncated_input_test() {
  let payload_decoder = codec.string_decoder()
  let dec = tagged.decoder("tag", 1, payload_decoder)

  // Only tag_len, no actual tag
  let result = codec.decode(dec, <<100:32>>)
  should.be_error(result)
}
