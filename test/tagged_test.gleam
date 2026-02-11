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
