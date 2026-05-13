import distribute/codec
import distribute/codec/composite
import gleam/bit_array
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

pub fn result_bundled_codec_test() {
  let c = composite.result(codec.int(), codec.string())
  let assert Ok(bin) = codec.encode(c.encoder, Ok(99))
  codec.decode(c.decoder, bin)
  |> should.equal(Ok(Ok(99)))
  let assert Ok(bin2) = codec.encode(c.encoder, Error("oops"))
  codec.decode(c.decoder, bin2)
  |> should.equal(Ok(Error("oops")))
}

pub fn result_bundled_sized_test() {
  let c = composite.result(codec.int(), codec.int())
  let assert Ok(bin) = codec.encode(c.encoder, Error(7))
  let assert Ok(#(val, rest)) = codec.decode_sized(c.sized_decoder, bin)
  should.equal(val, Error(7))
  should.equal(rest, <<>>)
}

// ---------------------------------------------------------------------------
// Strict top-level Option: None must be exactly one byte. Earlier drafts
// matched `<<0, _:bytes>>` and accepted arbitrary smuggled trailing
// bytes after the None tag.
// ---------------------------------------------------------------------------

pub fn option_decoder_rejects_trailing_after_none_test() {
  let dec = composite.option_decoder(codec.int_decoder())
  case codec.decode(dec, <<0, 0xDE, 0xAD>>) {
    Error(codec.InvalidBinary(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn option_decoder_accepts_clean_none_test() {
  let dec = composite.option_decoder(codec.int_decoder())
  codec.decode(dec, <<0>>) |> should.equal(Ok(option.None))
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

// ---------------------------------------------------------------------------
// Strict inner parsing: a hostile encoder can declare `len_a = 100_000`
// and pack 8 valid Int bytes followed by 99 992 bytes of smuggled
// payload. The decoder must reject the leftover, not pass it through.
// ---------------------------------------------------------------------------

pub fn tuple2_rejects_smuggled_inner_bytes_test() {
  // Forge a tuple2 frame: `len_a:32 || valid_int(8) || smuggled(8) || tail`
  // The `string` decoder for the second slot would normally consume the
  // tail; the attack is in the *first* slot, where 16 bytes were declared
  // for an 8-byte int.
  let assert Ok(int_bytes) = codec.encode(codec.int_encoder(), 99)
  let smuggled = <<0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE>>
  let first_slot = bit_array.append(int_bytes, smuggled)
  let len_a = bit_array.byte_size(first_slot)
  let assert Ok(string_bytes) = codec.encode(codec.string_encoder(), "x")
  let frame = bit_array.concat([<<len_a:32>>, first_slot, string_bytes])
  let dec =
    composite.tuple2_decoder(
      codec.int_sized_decoder(),
      codec.string_sized_decoder(),
    )
  case codec.decode(dec, frame) {
    Error(codec.InvalidBinary(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn tuple3_rejects_smuggled_inner_bytes_test() {
  // Same shape, applied to the second element of tuple3.
  let assert Ok(int_a) = codec.encode(codec.int_encoder(), 1)
  let len_a = bit_array.byte_size(int_a)

  let assert Ok(int_b) = codec.encode(codec.int_encoder(), 2)
  let smuggled = <<1, 2, 3, 4>>
  let second_slot = bit_array.append(int_b, smuggled)
  let len_b = bit_array.byte_size(second_slot)

  let assert Ok(int_c) = codec.encode(codec.int_encoder(), 3)
  let frame =
    bit_array.concat([<<len_a:32>>, int_a, <<len_b:32>>, second_slot, int_c])
  let dec =
    composite.tuple3_decoder(
      codec.int_sized_decoder(),
      codec.int_sized_decoder(),
      codec.int_sized_decoder(),
    )
  case codec.decode(dec, frame) {
    Error(codec.InvalidBinary(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

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
