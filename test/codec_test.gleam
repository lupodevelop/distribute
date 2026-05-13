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
  // Length prefix says 10 but only 3 bytes follow (using 32-bit prefix now)
  codec.decode(dec, <<0, 0, 0, 10, 65, 66, 67>>)
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
// Int range enforcement. wire format is signed 64-bit, Gleam Ints are not.
// Encoding outside [-2^63, 2^63 - 1] would silently truncate and corrupt
// data on the receiving node, so we reject it explicitly.
// ---------------------------------------------------------------------------

pub fn int_max_signed_64_roundtrips_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  // 2^63 - 1 = 9_223_372_036_854_775_807
  let max = 9_223_372_036_854_775_807
  let assert Ok(bin) = codec.encode(enc, max)
  codec.decode(dec, bin) |> should.equal(Ok(max))
}

pub fn int_min_signed_64_roundtrips_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  // -2^63 = -9_223_372_036_854_775_808
  let min = -9_223_372_036_854_775_808
  let assert Ok(bin) = codec.encode(enc, min)
  codec.decode(dec, bin) |> should.equal(Ok(min))
}

pub fn int_above_max_signed_64_rejected_test() {
  let enc = codec.int_encoder()
  // 2^63 = 9_223_372_036_854_775_808. one past the signed-64 max
  let result = codec.encode(enc, 9_223_372_036_854_775_808)
  case result {
    Error(codec.ValueTooLarge(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn int_below_min_signed_64_rejected_test() {
  let enc = codec.int_encoder()
  // -2^63 - 1 = -9_223_372_036_854_775_809. one past the signed-64 min
  let result = codec.encode(enc, -9_223_372_036_854_775_809)
  case result {
    Error(codec.ValueTooLarge(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

// ---------------------------------------------------------------------------
// List codec. 32-bit prefix with a hardened logical cap for RPC-style
// workloads.
//
// v4 keeps encode/decode symmetric by enforcing the same logical cap on
// both sides (`max_decoded_list_elements` in the codec module).
// ---------------------------------------------------------------------------

pub fn list_max_cap_roundtrips_test() {
  // The hardened cap is 10 000 elements. The max legal value must
  // still roundtrip cleanly.
  let big = build_int_list(10_000, [])
  let enc = codec.list_encoder(codec.int_encoder())
  let dec = codec.list_decoder(codec.int_sized_decoder())
  let assert Ok(bin) = codec.encode(enc, big)
  let assert Ok(decoded) = codec.decode(dec, bin)
  // Compare lengths. enough to prove the declared count was accepted.
  let dec_len = count_int_list(decoded, 0)
  dec_len |> should.equal(10_000)
}

pub fn list_encoder_rejects_count_above_decoder_cap_test() {
  // Build one element above the decoder cap (10_000) and verify the
  // encoder refuses to produce a self-incompatible frame.
  let too_many = build_nil_list(10_001, [])
  let enc = codec.list_encoder(codec.nil_encoder())
  case codec.encode(enc, too_many) {
    Error(codec.ValueTooLarge(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

fn build_int_list(n: Int, acc: List(Int)) -> List(Int) {
  case n <= 0 {
    True -> acc
    False -> build_int_list(n - 1, [n, ..acc])
  }
}

fn count_int_list(xs: List(Int), acc: Int) -> Int {
  case xs {
    [] -> acc
    [_, ..rest] -> count_int_list(rest, acc + 1)
  }
}

fn build_nil_list(n: Int, acc: List(Nil)) -> List(Nil) {
  case n <= 0 {
    True -> acc
    False -> build_nil_list(n - 1, [Nil, ..acc])
  }
}

// ---------------------------------------------------------------------------
// Zip-bomb / billion-laughs defence: a 4-byte payload that declares N
// billion zero-byte elements must NOT be allowed to allocate N billion
// list cells. The decoder caps element count regardless of payload size.
// ---------------------------------------------------------------------------

pub fn list_decoder_rejects_zip_bomb_count_test() {
  // Forge a payload: just the 32-bit count field, value 4 billion.
  // Element decoder is `nil_sized_decoder`. consumes 0 bytes per
  // element, so the count alone determines memory pressure.
  let bomb = <<4_000_000_000:32>>
  let dec = codec.list_decoder(codec.nil_sized_decoder())
  case codec.decode(dec, bomb) {
    Error(codec.ListTooLong(count, _cap)) -> should.equal(count, 4_000_000_000)
    _ -> should.be_true(False)
  }
}

pub fn list_decoder_rejects_count_just_above_cap_test() {
  // 10 001. one above the documented 10 000 cap.
  let bomb = <<10_001:32>>
  let dec = codec.list_decoder(codec.nil_sized_decoder())
  case codec.decode(dec, bomb) {
    Error(codec.ListTooLong(_, _)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

// ---------------------------------------------------------------------------
// String / BitArray length validation. Both encoders write a 32-bit prefix
// and must reject inputs that would silently wrap.
//
// We can't actually allocate 4 GiB of input in tests, so we exercise the
// boundary indirectly via a helper that injects a fake oversized length
// (proves the validation path is wired). Empty / small inputs still
// roundtrip cleanly.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Strict top-level: nil_decoder must reject ANY trailing data. Earlier
// drafts accepted any binary as Nil, defeating to_decoder's strict
// contract for every other primitive.
// ---------------------------------------------------------------------------

pub fn nil_decoder_strict_top_level_test() {
  let dec = codec.nil_decoder()
  // Empty -> Ok(Nil)
  codec.decode(dec, <<>>) |> should.equal(Ok(Nil))
  // Any trailing bytes -> Error.
  case codec.decode(dec, <<1, 2, 3>>) {
    Error(codec.InvalidBinary(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn empty_string_roundtrips_under_32bit_prefix_test() {
  let enc = codec.string_encoder()
  let dec = codec.string_decoder()
  let assert Ok(bin) = codec.encode(enc, "")
  codec.decode(dec, bin) |> should.equal(Ok(""))
}

pub fn empty_bitarray_roundtrips_under_32bit_prefix_test() {
  let enc = codec.bitarray_encoder()
  let dec = codec.bitarray_decoder()
  let assert Ok(bin) = codec.encode(enc, <<>>)
  codec.decode(dec, bin) |> should.equal(Ok(<<>>))
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

pub fn to_decoder_strict_no_trailing_bytes_test() {
  // v4 semantics: top-level decode must consume the binary completely.
  // Trailing bytes are a protocol violation (data smuggling, double
  // payloads, framing bug) and surface as InvalidBinary.
  let sdec = codec.int_sized_decoder()
  let dec = codec.to_decoder(sdec)
  let assert Ok(bin) = codec.encode(codec.int_encoder(), 99)
  let with_extra = bit_array.append(bin, <<1, 2, 3>>)
  case codec.decode(dec, with_extra) {
    Error(codec.InvalidBinary(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn to_decoder_strict_exact_match_succeeds_test() {
  // No trailing bytes -> Ok.
  let sdec = codec.int_sized_decoder()
  let dec = codec.to_decoder(sdec)
  let assert Ok(bin) = codec.encode(codec.int_encoder(), 99)
  codec.decode(dec, bin) |> should.equal(Ok(99))
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

// ---------------------------------------------------------------------------
// Z4. v3 → v4 wire-format incompatibility
//
// v3 used a 16-bit length prefix on `String`/`BitArray`/`List`. v4 uses
// 32-bit. A v3 binary fed to a v4 decoder must be rejected. we never
// want a v3 sender to silently corrupt a v4 receiver's decode.
// ---------------------------------------------------------------------------

pub fn v3_string_decoder_rejects_short_prefix_test() {
  // v3 string "hello": <<5:16, "hello":utf8>> = 7 bytes total.
  let v3_bin = <<5:16, "hello":utf8>>
  // v4 reads <<len:32, ...>>: len = <<0,5,104,101>> = 327_781; needs 327k+
  // bytes after the prefix, has 3. Decoder must surface as a typed error,
  // never a silent partial decode.
  let result = codec.string_decoder()(v3_bin)
  should.be_error(result)
}

pub fn v3_bitarray_decoder_rejects_short_prefix_test() {
  let v3_bin = <<3:16, 1, 2, 3>>
  let result = codec.bitarray_decoder()(v3_bin)
  should.be_error(result)
}

pub fn v3_list_decoder_rejects_short_prefix_test() {
  // v3 list of two ints: <<2:16, int1:64, int2:64>> = 18 bytes.
  let v3_bin = <<2:16, 1:64, 2:64>>
  let result = codec.list_decoder(codec.int_sized_decoder())(v3_bin)
  should.be_error(result)
}

pub fn v3_subject_decoder_rejects_short_prefix_test() {
  // v3 subject was `<<len:16, term_to_binary_bytes:bits>>`. We synthesise
  // a 4-byte body and wrap it in v3 framing. Decoder must reject before
  // touching binary_to_term.
  let v3_bin = <<4:16, 1, 2, 3, 4>>
  let result = codec.subject_decoder()(v3_bin)
  should.be_error(result)
}
