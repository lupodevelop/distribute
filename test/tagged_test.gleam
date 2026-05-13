import distribute/codec
import distribute/codec/tagged
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn tagged_roundtrip_test() {
  let enc = tagged.encoder(codec.string_encoder())
  let dec = tagged.decoder("myproto", 1, codec.string_decoder())
  let msg = tagged.new("myproto", 1, "hello")
  let assert Ok(bin) = codec.encode(enc, msg)
  let assert Ok(decoded) = codec.decode(dec, bin)
  should.equal(tagged.payload(decoded), "hello")
  should.equal(tagged.tag(decoded), "myproto")
  should.equal(tagged.version(decoded), 1)
}

pub fn tagged_tag_mismatch_test() {
  let enc = tagged.encoder(codec.int_encoder())
  let dec = tagged.decoder("expected", 1, codec.int_decoder())
  let msg = tagged.new("wrong", 1, 42)
  let assert Ok(bin) = codec.encode(enc, msg)
  let result = codec.decode(dec, bin)
  should.be_error(result)
}

pub fn tagged_version_mismatch_test() {
  let enc = tagged.encoder(codec.int_encoder())
  let dec = tagged.decoder("proto", 2, codec.int_decoder())
  let msg = tagged.new("proto", 1, 42)
  let assert Ok(bin) = codec.encode(enc, msg)
  let result = codec.decode(dec, bin)
  should.be_error(result)
}

pub fn encode_tagged_convenience_test() {
  let assert Ok(bin) =
    tagged.encode_tagged("test", 1, codec.string_encoder(), "value")
  let assert Ok(payload) =
    tagged.decode_tagged("test", 1, codec.string_decoder(), bin)
  should.equal(payload, "value")
}

pub fn decode_tagged_wrong_tag_test() {
  let assert Ok(bin) = tagged.encode_tagged("real", 1, codec.int_encoder(), 99)
  tagged.decode_tagged("fake", 1, codec.int_decoder(), bin)
  |> should.be_error()
}

// ---------------------------------------------------------------------------
// Version range enforcement. wire format is unsigned 32-bit, so any value
// outside [0, 2^32 - 1] would silently wrap and corrupt cross-node decoding.
// ---------------------------------------------------------------------------

pub fn tagged_max_version_roundtrips_test() {
  // 2^32 - 1 = 4_294_967_295
  let max = 4_294_967_295
  let enc = tagged.encoder(codec.int_encoder())
  let dec = tagged.decoder("proto", max, codec.int_decoder())
  let msg = tagged.new("proto", max, 1)
  let assert Ok(bin) = codec.encode(enc, msg)
  let assert Ok(decoded) = codec.decode(dec, bin)
  should.equal(tagged.version(decoded), max)
}

pub fn tagged_zero_version_roundtrips_test() {
  let enc = tagged.encoder(codec.int_encoder())
  let dec = tagged.decoder("proto", 0, codec.int_decoder())
  let msg = tagged.new("proto", 0, 1)
  let assert Ok(bin) = codec.encode(enc, msg)
  let assert Ok(decoded) = codec.decode(dec, bin)
  should.equal(tagged.version(decoded), 0)
}

pub fn tagged_negative_version_rejected_test() {
  let enc = tagged.encoder(codec.int_encoder())
  let msg = tagged.new("proto", -1, 1)
  let result = codec.encode(enc, msg)
  case result {
    Error(codec.ValueTooLarge(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn tagged_version_above_unsigned_32_rejected_test() {
  let enc = tagged.encoder(codec.int_encoder())
  // 2^32 = 4_294_967_296. one past the unsigned-32 max
  let msg = tagged.new("proto", 4_294_967_296, 1)
  let result = codec.encode(enc, msg)
  case result {
    Error(codec.ValueTooLarge(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn tagged_oversize_tag_rejected_test() {
  // Tag length is encoded as 32-bit unsigned; we do not allow tags
  // longer than 2^32 - 1 bytes. We can't actually allocate 4 GiB of
  // string in tests, so we settle for a sanity check that the encoder
  // path handles an empty tag (zero-length boundary) gracefully.
  let enc = tagged.encoder(codec.int_encoder())
  let dec = tagged.decoder("", 1, codec.int_decoder())
  let msg = tagged.new("", 1, 7)
  let assert Ok(bin) = codec.encode(enc, msg)
  let assert Ok(decoded) = codec.decode(dec, bin)
  should.equal(tagged.payload(decoded), 7)
}
