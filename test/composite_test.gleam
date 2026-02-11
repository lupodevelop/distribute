import distribute/codec
import distribute/codec/composite
import gleam/option
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ---------------------------------------------------------------------------
// Option codecs
// ---------------------------------------------------------------------------

pub fn option_some_test() {
  let enc = composite.option_encoder(codec.int_encoder())
  let dec = composite.option_decoder(codec.int_decoder())
  let assert Ok(bin) = codec.encode(enc, option.Some(42))
  codec.decode(dec, bin)
  |> should.equal(Ok(option.Some(42)))
}

pub fn option_none_test() {
  let enc = composite.option_encoder(codec.int_encoder())
  let dec = composite.option_decoder(codec.int_decoder())
  let assert Ok(bin) = codec.encode(enc, option.None)
  codec.decode(dec, bin)
  |> should.equal(Ok(option.None))
}

pub fn option_sized_roundtrip_test() {
  let enc = composite.option_encoder(codec.string_encoder())
  let sdec = composite.option_sized_decoder(codec.string_sized_decoder())
  let assert Ok(bin) = codec.encode(enc, option.Some("hello"))
  let assert Ok(#(val, rest)) = codec.decode_sized(sdec, bin)
  should.equal(val, option.Some("hello"))
  should.equal(rest, <<>>)
}

// ---------------------------------------------------------------------------
// Result codecs
// ---------------------------------------------------------------------------

pub fn result_ok_test() {
  let enc =
    composite.result_encoder(codec.int_encoder(), codec.string_encoder())
  let dec =
    composite.result_decoder(codec.int_decoder(), codec.string_decoder())
  let assert Ok(bin) = codec.encode(enc, Ok(100))
  codec.decode(dec, bin)
  |> should.equal(Ok(Ok(100)))
}

pub fn result_error_test() {
  let enc =
    composite.result_encoder(codec.int_encoder(), codec.string_encoder())
  let dec =
    composite.result_decoder(codec.int_decoder(), codec.string_decoder())
  let assert Ok(bin) = codec.encode(enc, Error("oops"))
  codec.decode(dec, bin)
  |> should.equal(Ok(Error("oops")))
}

pub fn result_sized_test() {
  let enc = composite.result_encoder(codec.int_encoder(), codec.int_encoder())
  let sdec =
    composite.result_sized_decoder(
      codec.int_sized_decoder(),
      codec.int_sized_decoder(),
    )
  let assert Ok(bin) = codec.encode(enc, Ok(7))
  let assert Ok(#(val, rest)) = codec.decode_sized(sdec, bin)
  should.equal(val, Ok(7))
  should.equal(rest, <<>>)
}

// ---------------------------------------------------------------------------
// Tuple2 codecs
// ---------------------------------------------------------------------------

pub fn tuple2_roundtrip_test() {
  let enc =
    composite.tuple2_encoder(codec.int_encoder(), codec.string_encoder())
  let dec =
    composite.tuple2_decoder(
      codec.int_sized_decoder(),
      codec.string_sized_decoder(),
    )
  let data = #(42, "hello")
  let assert Ok(bin) = codec.encode(enc, data)
  codec.decode(dec, bin)
  |> should.equal(Ok(data))
}

pub fn tuple2_sized_test() {
  let enc =
    composite.tuple2_encoder(codec.string_encoder(), codec.bool_encoder())
  let sdec =
    composite.tuple2_sized_decoder(
      codec.string_sized_decoder(),
      codec.bool_sized_decoder(),
    )
  let assert Ok(bin) = codec.encode(enc, #("yes", True))
  let assert Ok(#(val, rest)) = codec.decode_sized(sdec, bin)
  should.equal(val, #("yes", True))
  should.equal(rest, <<>>)
}

// ---------------------------------------------------------------------------
// Tuple3 codecs
// ---------------------------------------------------------------------------

pub fn tuple3_roundtrip_test() {
  let enc =
    composite.tuple3_encoder(
      codec.int_encoder(),
      codec.string_encoder(),
      codec.bool_encoder(),
    )
  let dec =
    composite.tuple3_decoder(
      codec.int_sized_decoder(),
      codec.string_sized_decoder(),
      codec.bool_sized_decoder(),
    )
  let data = #(1, "two", True)
  let assert Ok(bin) = codec.encode(enc, data)
  codec.decode(dec, bin)
  |> should.equal(Ok(data))
}
