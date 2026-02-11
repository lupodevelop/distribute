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
// String codec
// ---------------------------------------------------------------------------

pub fn string_roundtrip_test() {
  let enc = codec.string_encoder()
  let dec = codec.string_decoder()
  let assert Ok(bin) = codec.encode(enc, "hello")
  codec.decode(dec, bin)
  |> should.equal(Ok("hello"))
}

pub fn string_empty_test() {
  let enc = codec.string_encoder()
  let dec = codec.string_decoder()
  let assert Ok(bin) = codec.encode(enc, "")
  codec.decode(dec, bin)
  |> should.equal(Ok(""))
}

pub fn string_unicode_test() {
  let enc = codec.string_encoder()
  let dec = codec.string_decoder()
  let assert Ok(bin) = codec.encode(enc, "日本語🎉")
  codec.decode(dec, bin)
  |> should.equal(Ok("日本語🎉"))
}

pub fn string_sized_decoder_test() {
  let enc = codec.string_encoder()
  let sdec = codec.string_sized_decoder()
  let assert Ok(bin) = codec.encode(enc, "abc")
  // Append extra bytes
  let with_extra = bit_array.append(bin, <<99, 100>>)
  let assert Ok(#(val, rest)) = codec.decode_sized(sdec, with_extra)
  should.equal(val, "abc")
  should.equal(rest, <<99, 100>>)
}

pub fn string_insufficient_data_test() {
  let dec = codec.string_decoder()
  // Length prefix says 10 but only 3 bytes follow
  codec.decode(dec, <<0, 10, 65, 66, 67>>)
  |> should.be_error()
}

// ---------------------------------------------------------------------------
// Int codec
// ---------------------------------------------------------------------------

pub fn int_roundtrip_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  let assert Ok(bin) = codec.encode(enc, 42)
  codec.decode(dec, bin)
  |> should.equal(Ok(42))
}

pub fn int_negative_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  let assert Ok(bin) = codec.encode(enc, -999)
  codec.decode(dec, bin)
  |> should.equal(Ok(-999))
}

pub fn int_zero_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  let assert Ok(bin) = codec.encode(enc, 0)
  codec.decode(dec, bin)
  |> should.equal(Ok(0))
}

pub fn int_sized_decoder_test() {
  let enc = codec.int_encoder()
  let sdec = codec.int_sized_decoder()
  let assert Ok(bin) = codec.encode(enc, 7)
  let with_extra = bit_array.append(bin, <<255>>)
  let assert Ok(#(val, rest)) = codec.decode_sized(sdec, with_extra)
  should.equal(val, 7)
  should.equal(rest, <<255>>)
}

// ---------------------------------------------------------------------------
// Float codec
// ---------------------------------------------------------------------------

pub fn float_roundtrip_test() {
  let enc = codec.float_encoder()
  let dec = codec.float_decoder()
  let assert Ok(bin) = codec.encode(enc, 3.14)
  let assert Ok(decoded) = codec.decode(dec, bin)
  // Float comparison with tolerance
  let diff = decoded -. 3.14
  let abs_diff = case diff <. 0.0 {
    True -> 0.0 -. diff
    False -> diff
  }
  should.be_true(abs_diff <. 0.0001)
}

pub fn float_zero_test() {
  let enc = codec.float_encoder()
  let dec = codec.float_decoder()
  let assert Ok(bin) = codec.encode(enc, 0.0)
  codec.decode(dec, bin)
  |> should.equal(Ok(0.0))
}

// ---------------------------------------------------------------------------
// Bool codec
// ---------------------------------------------------------------------------

pub fn bool_true_test() {
  let enc = codec.bool_encoder()
  let dec = codec.bool_decoder()
  let assert Ok(bin) = codec.encode(enc, True)
  codec.decode(dec, bin)
  |> should.equal(Ok(True))
}

pub fn bool_false_test() {
  let enc = codec.bool_encoder()
  let dec = codec.bool_decoder()
  let assert Ok(bin) = codec.encode(enc, False)
  codec.decode(dec, bin)
  |> should.equal(Ok(False))
}

// ---------------------------------------------------------------------------
// BitArray codec
// ---------------------------------------------------------------------------

pub fn bitarray_roundtrip_test() {
  let enc = codec.bitarray_encoder()
  let dec = codec.bitarray_decoder()
  let data = <<1, 2, 3, 4, 5>>
  let assert Ok(bin) = codec.encode(enc, data)
  codec.decode(dec, bin)
  |> should.equal(Ok(data))
}

pub fn bitarray_empty_test() {
  let enc = codec.bitarray_encoder()
  let dec = codec.bitarray_decoder()
  let assert Ok(bin) = codec.encode(enc, <<>>)
  codec.decode(dec, bin)
  |> should.equal(Ok(<<>>))
}

// ---------------------------------------------------------------------------
// List codec
// ---------------------------------------------------------------------------

pub fn list_int_roundtrip_test() {
  let enc = codec.list_encoder(codec.int_encoder())
  let dec = codec.list_decoder(codec.int_sized_decoder())
  let data = [1, 2, 3, 4, 5]
  let assert Ok(bin) = codec.encode(enc, data)
  codec.decode(dec, bin)
  |> should.equal(Ok(data))
}

pub fn list_string_roundtrip_test() {
  let enc = codec.list_encoder(codec.string_encoder())
  let dec = codec.list_decoder(codec.string_sized_decoder())
  let data = ["hello", "world", "gleam"]
  let assert Ok(bin) = codec.encode(enc, data)
  codec.decode(dec, bin)
  |> should.equal(Ok(data))
}

pub fn list_empty_test() {
  let enc = codec.list_encoder(codec.int_encoder())
  let dec = codec.list_decoder(codec.int_sized_decoder())
  let assert Ok(bin) = codec.encode(enc, [])
  codec.decode(dec, bin)
  |> should.equal(Ok([]))
}

pub fn list_nested_test() {
  let inner_enc = codec.list_encoder(codec.int_encoder())
  let outer_enc = codec.list_encoder(inner_enc)
  let inner_dec = codec.list_sized_decoder(codec.int_sized_decoder())
  let outer_dec = codec.list_decoder(inner_dec)
  let data = [[1, 2], [3, 4, 5], []]
  let assert Ok(bin) = codec.encode(outer_enc, data)
  codec.decode(outer_dec, bin)
  |> should.equal(Ok(data))
}

// ---------------------------------------------------------------------------
// to_decoder
// ---------------------------------------------------------------------------

pub fn to_decoder_discards_rest_test() {
  let sdec = codec.int_sized_decoder()
  let dec = codec.to_decoder(sdec)
  let assert Ok(bin) = codec.encode(codec.int_encoder(), 99)
  let with_extra = bit_array.append(bin, <<1, 2, 3>>)
  codec.decode(dec, with_extra)
  |> should.equal(Ok(99))
}

// ---------------------------------------------------------------------------
// Error formatting
// ---------------------------------------------------------------------------

pub fn decode_error_to_string_test() {
  codec.decode_error_to_string(codec.DecodeTimeout)
  |> should.equal("Decode timeout")

  codec.decode_error_to_string(codec.InvalidBinary("bad"))
  |> should.equal("Invalid binary: bad")

  codec.decode_error_to_string(codec.TagMismatch("a", "b"))
  |> should.equal("Tag mismatch: expected 'a', got 'b'")
}

pub fn encode_error_to_string_test() {
  codec.encode_error_to_string(codec.ValueTooLarge("x"))
  |> should.equal("Value too large: x")
}

// ---------------------------------------------------------------------------
// Nil codec
// ---------------------------------------------------------------------------

pub fn nil_roundtrip_test() {
  let assert Ok(bin) = codec.encode(codec.nil_encoder(), Nil)
  should.equal(bin, <<>>)
  codec.decode(codec.nil_decoder(), bin)
  |> should.equal(Ok(Nil))
}

pub fn nil_sized_passes_bytes_through_test() {
  let assert Ok(#(val, rest)) =
    codec.decode_sized(codec.nil_sized_decoder(), <<1, 2, 3>>)
  should.equal(val, Nil)
  should.equal(rest, <<1, 2, 3>>)
}

pub fn nil_sized_empty_input_test() {
  let assert Ok(#(val, rest)) =
    codec.decode_sized(codec.nil_sized_decoder(), <<>>)
  should.equal(val, Nil)
  should.equal(rest, <<>>)
}

// ---------------------------------------------------------------------------
// Codec(a) bundled constructors
// ---------------------------------------------------------------------------

pub fn codec_int_roundtrip_test() {
  let c = codec.int()
  let assert Ok(bin) = c.encoder(42)
  c.decoder(bin)
  |> should.equal(Ok(42))
}

pub fn codec_string_roundtrip_test() {
  let c = codec.string()
  let assert Ok(bin) = c.encoder("gleam")
  c.decoder(bin)
  |> should.equal(Ok("gleam"))
}

pub fn codec_bool_roundtrip_test() {
  let c = codec.bool()
  let assert Ok(bin) = c.encoder(True)
  c.decoder(bin)
  |> should.equal(Ok(True))
}

pub fn codec_nil_roundtrip_test() {
  let c = codec.nil()
  let assert Ok(bin) = c.encoder(Nil)
  should.equal(bin, <<>>)
  c.decoder(bin)
  |> should.equal(Ok(Nil))
}

pub fn codec_list_roundtrip_test() {
  let c = codec.list(codec.int())
  let assert Ok(bin) = c.encoder([10, 20, 30])
  c.decoder(bin)
  |> should.equal(Ok([10, 20, 30]))
}

pub fn codec_list_empty_test() {
  let c = codec.list(codec.string())
  let assert Ok(bin) = c.encoder([])
  c.decoder(bin)
  |> should.equal(Ok([]))
}

pub fn codec_list_sized_test() {
  let c = codec.list(codec.int())
  let assert Ok(bin) = c.encoder([1, 2])
  let with_extra = bit_array.append(bin, <<99>>)
  let assert Ok(#(val, rest)) = c.sized_decoder(with_extra)
  should.equal(val, [1, 2])
  should.equal(rest, <<99>>)
}

// ---------------------------------------------------------------------------
// codec.map
// ---------------------------------------------------------------------------

pub type Score {
  Score(Int)
}

pub fn map_roundtrip_test() {
  let score =
    codec.map(codec.int(), Score, fn(s) {
      let Score(n) = s
      n
    })
  let assert Ok(bin) = score.encoder(Score(99))
  score.decoder(bin)
  |> should.equal(Ok(Score(99)))
}

pub fn map_sized_decoder_test() {
  let score =
    codec.map(codec.int(), Score, fn(s) {
      let Score(n) = s
      n
    })
  let assert Ok(bin) = score.encoder(Score(7))
  let with_extra = bit_array.append(bin, <<255>>)
  let assert Ok(#(val, rest)) = score.sized_decoder(with_extra)
  should.equal(val, Score(7))
  should.equal(rest, <<255>>)
}

pub fn map_in_list_test() {
  let score =
    codec.map(codec.int(), Score, fn(s) {
      let Score(n) = s
      n
    })
  let scores = codec.list(score)
  let assert Ok(bin) = scores.encoder([Score(1), Score(2), Score(3)])
  scores.decoder(bin)
  |> should.equal(Ok([Score(1), Score(2), Score(3)]))
}

pub fn map_string_wrapper_test() {
  // A simple newtype: wrap a string
  let tag =
    codec.map(codec.string(), fn(s) { #("tag", s) }, fn(pair) { pair.1 })
  let assert Ok(bin) = tag.encoder(#("tag", "hello"))
  tag.decoder(bin)
  |> should.equal(Ok(#("tag", "hello")))
}

// ---------------------------------------------------------------------------
// Composite bundled codecs
// ---------------------------------------------------------------------------

pub fn option_codec_some_test() {
  let c = composite.option(codec.int())
  let assert Ok(bin) = c.encoder(option.Some(42))
  c.decoder(bin)
  |> should.equal(Ok(option.Some(42)))
}

pub fn option_codec_none_test() {
  let c = composite.option(codec.int())
  let assert Ok(bin) = c.encoder(option.None)
  c.decoder(bin)
  |> should.equal(Ok(option.None))
}

pub fn option_codec_sized_test() {
  let c = composite.option(codec.int())
  let assert Ok(bin) = c.encoder(option.Some(5))
  let with_extra = bit_array.append(bin, <<99>>)
  let assert Ok(#(val, rest)) = c.sized_decoder(with_extra)
  should.equal(val, option.Some(5))
  should.equal(rest, <<99>>)
}

pub fn tuple2_codec_roundtrip_test() {
  let c = composite.tuple2(codec.int(), codec.string())
  let assert Ok(bin) = c.encoder(#(42, "hello"))
  c.decoder(bin)
  |> should.equal(Ok(#(42, "hello")))
}

pub fn tuple3_codec_roundtrip_test() {
  let c = composite.tuple3(codec.int(), codec.string(), codec.bool())
  let assert Ok(bin) = c.encoder(#(1, "a", True))
  c.decoder(bin)
  |> should.equal(Ok(#(1, "a", True)))
}

pub fn composite_with_map_test() {
  // Option(Score) via composed codecs
  let score =
    codec.map(codec.int(), Score, fn(s) {
      let Score(n) = s
      n
    })
  let c = composite.option(score)
  let assert Ok(bin) = c.encoder(option.Some(Score(100)))
  c.decoder(bin)
  |> should.equal(Ok(option.Some(Score(100))))
}
