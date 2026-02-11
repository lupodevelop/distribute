/// Binary codecs for sending Gleam values across nodes.
import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

pub type EncodeError {
  InvalidValue(String)
  EncodeFailed(String)
  ValueTooLarge(String)
}

pub type DecodeError {
  InvalidBinary(String)
  TypeMismatch(String)
  DecodeFailed(String)
  InsufficientData(String)
  DecodeTimeout
  TagMismatch(expected: String, got: String)
  VersionMismatch(expected: Int, got: Int)
}

// ---------------------------------------------------------------------------
// Encoder / Decoder types
// ---------------------------------------------------------------------------

pub type Encoder(a) =
  fn(a) -> Result(BitArray, EncodeError)

pub type Decoder(a) =
  fn(BitArray) -> Result(a, DecodeError)

/// Like `Decoder` but returns leftover bytes for chaining.
pub type SizedDecoder(a) =
  fn(BitArray) -> Result(#(a, BitArray), DecodeError)

/// Encoder + decoder + sized decoder, bundled.
pub type Codec(a) {
  Codec(
    encoder: Encoder(a),
    decoder: Decoder(a),
    sized_decoder: SizedDecoder(a),
  )
}

// ---------------------------------------------------------------------------
// Core operations
// ---------------------------------------------------------------------------

pub fn encode(encoder: Encoder(a), value: a) -> Result(BitArray, EncodeError) {
  encoder(value)
}

pub fn decode(decoder: Decoder(a), data: BitArray) -> Result(a, DecodeError) {
  decoder(data)
}

pub fn decode_sized(
  decoder: SizedDecoder(a),
  data: BitArray,
) -> Result(#(a, BitArray), DecodeError) {
  decoder(data)
}

/// Turn a `SizedDecoder` into a `Decoder` (drops remaining bytes).
pub fn to_decoder(sized: SizedDecoder(a)) -> Decoder(a) {
  fn(data) {
    case sized(data) {
      Ok(#(value, _rest)) -> Ok(value)
      Error(e) -> Error(e)
    }
  }
}

// ---------------------------------------------------------------------------
// String codec — UTF-8, 16-bit length prefix
// ---------------------------------------------------------------------------

pub fn string_encoder() -> Encoder(String) {
  fn(s) {
    let bytes = bit_array.from_string(s)
    let len = bit_array.byte_size(bytes)
    case len > 65_535 {
      True -> Error(ValueTooLarge("string length " <> int.to_string(len)))
      False -> Ok(<<len:16, bytes:bits>>)
    }
  }
}

pub fn string_sized_decoder() -> SizedDecoder(String) {
  fn(data) {
    case data {
      <<len:16, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        case rest_size >= len {
          False -> Error(InsufficientData("string"))
          True -> {
            use str_bytes <- result.try(
              bit_array.slice(rest, 0, len)
              |> result.replace_error(InsufficientData("string slice")),
            )
            use s <- result.try(
              bit_array.to_string(str_bytes)
              |> result.replace_error(InvalidBinary("invalid UTF-8")),
            )
            let remaining = case bit_array.slice(rest, len, rest_size - len) {
              Ok(r) -> r
              Error(_) -> <<>>
            }
            Ok(#(s, remaining))
          }
        }
      }
      _ -> Error(InvalidBinary("missing string length prefix"))
    }
  }
}

pub fn string_decoder() -> Decoder(String) {
  to_decoder(string_sized_decoder())
}

// ---------------------------------------------------------------------------
// Int codec — 64-bit big-endian signed
// ---------------------------------------------------------------------------

pub fn int_encoder() -> Encoder(Int) {
  fn(i) { Ok(<<i:64>>) }
}

pub fn int_sized_decoder() -> SizedDecoder(Int) {
  fn(data) {
    case data {
      <<i:signed-size(64), rest:bytes>> -> Ok(#(i, rest))
      _ -> Error(InvalidBinary("int64"))
    }
  }
}

pub fn int_decoder() -> Decoder(Int) {
  to_decoder(int_sized_decoder())
}

// ---------------------------------------------------------------------------
// Float codec — 64-bit IEEE 754 big-endian
// ---------------------------------------------------------------------------

pub fn float_encoder() -> Encoder(Float) {
  fn(f) { Ok(<<f:float>>) }
}

pub fn float_sized_decoder() -> SizedDecoder(Float) {
  fn(data) {
    case data {
      <<f:float, rest:bytes>> -> Ok(#(f, rest))
      _ -> Error(InvalidBinary("float64"))
    }
  }
}

pub fn float_decoder() -> Decoder(Float) {
  to_decoder(float_sized_decoder())
}

// ---------------------------------------------------------------------------
// Bool codec — single byte (0 = False, 1 = True)
// ---------------------------------------------------------------------------

pub fn bool_encoder() -> Encoder(Bool) {
  fn(b) {
    case b {
      True -> Ok(<<1>>)
      False -> Ok(<<0>>)
    }
  }
}

pub fn bool_sized_decoder() -> SizedDecoder(Bool) {
  fn(data) {
    case data {
      <<1, rest:bytes>> -> Ok(#(True, rest))
      <<0, rest:bytes>> -> Ok(#(False, rest))
      _ -> Error(InvalidBinary("bool"))
    }
  }
}

pub fn bool_decoder() -> Decoder(Bool) {
  to_decoder(bool_sized_decoder())
}

// ---------------------------------------------------------------------------
// BitArray codec — 32-bit length prefix + raw bytes
// ---------------------------------------------------------------------------

pub fn bitarray_encoder() -> Encoder(BitArray) {
  fn(bytes) {
    let len = bit_array.byte_size(bytes)
    Ok(<<len:32, bytes:bits>>)
  }
}

pub fn bitarray_sized_decoder() -> SizedDecoder(BitArray) {
  fn(data) {
    case data {
      <<len:32, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        case rest_size >= len {
          False -> Error(InsufficientData("bitarray"))
          True -> {
            use bytes <- result.try(
              bit_array.slice(rest, 0, len)
              |> result.replace_error(InsufficientData("bitarray slice")),
            )
            let remaining = case bit_array.slice(rest, len, rest_size - len) {
              Ok(r) -> r
              Error(_) -> <<>>
            }
            Ok(#(bytes, remaining))
          }
        }
      }
      _ -> Error(InvalidBinary("bitarray length prefix"))
    }
  }
}

pub fn bitarray_decoder() -> Decoder(BitArray) {
  to_decoder(bitarray_sized_decoder())
}

// ---------------------------------------------------------------------------
// List codec — 16-bit element count + elements via SizedDecoder
// ---------------------------------------------------------------------------

pub fn list_encoder(element_encoder: Encoder(a)) -> Encoder(List(a)) {
  fn(items) {
    let len = list.length(items)
    case len > 65_535 {
      True -> Error(ValueTooLarge("list length " <> int.to_string(len)))
      False -> {
        use body <- result.try(
          encode_list_elements(items, element_encoder, <<>>),
        )
        Ok(<<len:16, body:bits>>)
      }
    }
  }
}

pub fn list_sized_decoder(
  element_decoder: SizedDecoder(a),
) -> SizedDecoder(List(a)) {
  fn(data) {
    case data {
      <<count:16, rest:bytes>> ->
        decode_list_elements(rest, element_decoder, count, [])
      _ -> Error(InvalidBinary("list length prefix"))
    }
  }
}

pub fn list_decoder(element_decoder: SizedDecoder(a)) -> Decoder(List(a)) {
  to_decoder(list_sized_decoder(element_decoder))
}

fn encode_list_elements(
  items: List(a),
  enc: Encoder(a),
  acc: BitArray,
) -> Result(BitArray, EncodeError) {
  case items {
    [] -> Ok(acc)
    [head, ..tail] -> {
      use bytes <- result.try(enc(head))
      encode_list_elements(tail, enc, bit_array.append(acc, bytes))
    }
  }
}

fn decode_list_elements(
  data: BitArray,
  dec: SizedDecoder(a),
  remaining: Int,
  acc: List(a),
) -> Result(#(List(a), BitArray), DecodeError) {
  case remaining {
    0 -> Ok(#(list.reverse(acc), data))
    _ -> {
      case dec(data) {
        Ok(#(element, rest)) ->
          decode_list_elements(rest, dec, remaining - 1, [element, ..acc])
        Error(e) -> Error(e)
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Nil codec — zero bytes, for actors that don't carry a payload
// ---------------------------------------------------------------------------

pub fn nil_encoder() -> Encoder(Nil) {
  fn(_) { Ok(<<>>) }
}

pub fn nil_sized_decoder() -> SizedDecoder(Nil) {
  fn(data) { Ok(#(Nil, data)) }
}

pub fn nil_decoder() -> Decoder(Nil) {
  fn(_) { Ok(Nil) }
}

// ---------------------------------------------------------------------------
// Subject codec — for request/response (call) pattern
// ---------------------------------------------------------------------------

@external(erlang, "distribute_ffi_utils", "encode_subject")
fn encode_subject_ffi(subject: process.Subject(BitArray)) -> BitArray

@external(erlang, "distribute_ffi_utils", "decode_subject_safe")
fn decode_subject_ffi(data: BitArray) -> Result(process.Subject(BitArray), Nil)

/// Encode a `Subject(BitArray)` via `term_to_binary`. The PID
/// inside carries node info, so it routes back cross-node.
pub fn subject_encoder() -> Encoder(process.Subject(BitArray)) {
  fn(sub) {
    let bytes = encode_subject_ffi(sub)
    let len = bit_array.byte_size(bytes)
    Ok(<<len:32, bytes:bits>>)
  }
}

pub fn subject_sized_decoder() -> SizedDecoder(process.Subject(BitArray)) {
  fn(data) {
    case data {
      <<len:32, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        case rest_size >= len {
          False -> Error(InsufficientData("subject"))
          True -> {
            case bit_array.slice(rest, 0, len) {
              Ok(subject_bytes) ->
                case decode_subject_ffi(subject_bytes) {
                  Ok(sub) -> {
                    let remaining = case
                      bit_array.slice(rest, len, rest_size - len)
                    {
                      Ok(r) -> r
                      Error(_) -> <<>>
                    }
                    Ok(#(sub, remaining))
                  }
                  Error(Nil) -> Error(DecodeFailed("invalid subject binary"))
                }
              Error(_) -> Error(InsufficientData("subject slice"))
            }
          }
        }
      }
      _ -> Error(InvalidBinary("subject length prefix"))
    }
  }
}

pub fn subject_decoder() -> Decoder(process.Subject(BitArray)) {
  to_decoder(subject_sized_decoder())
}

// ---------------------------------------------------------------------------
// Bundled codecs
// ---------------------------------------------------------------------------

pub fn string() -> Codec(String) {
  Codec(
    encoder: string_encoder(),
    decoder: string_decoder(),
    sized_decoder: string_sized_decoder(),
  )
}

pub fn int() -> Codec(Int) {
  Codec(
    encoder: int_encoder(),
    decoder: int_decoder(),
    sized_decoder: int_sized_decoder(),
  )
}

pub fn float() -> Codec(Float) {
  Codec(
    encoder: float_encoder(),
    decoder: float_decoder(),
    sized_decoder: float_sized_decoder(),
  )
}

pub fn bool() -> Codec(Bool) {
  Codec(
    encoder: bool_encoder(),
    decoder: bool_decoder(),
    sized_decoder: bool_sized_decoder(),
  )
}

pub fn bitarray() -> Codec(BitArray) {
  Codec(
    encoder: bitarray_encoder(),
    decoder: bitarray_decoder(),
    sized_decoder: bitarray_sized_decoder(),
  )
}

pub fn nil() -> Codec(Nil) {
  Codec(
    encoder: nil_encoder(),
    decoder: nil_decoder(),
    sized_decoder: nil_sized_decoder(),
  )
}

pub fn list(element: Codec(a)) -> Codec(List(a)) {
  Codec(
    encoder: list_encoder(element.encoder),
    decoder: list_decoder(element.sized_decoder),
    sized_decoder: list_sized_decoder(element.sized_decoder),
  )
}

/// Subject codec, for the request/response pattern.
pub fn subject() -> Codec(process.Subject(BitArray)) {
  Codec(
    encoder: subject_encoder(),
    decoder: subject_decoder(),
    sized_decoder: subject_sized_decoder(),
  )
}

/// Transform a codec. `wrap` runs after decoding, `unwrap` before encoding.
///
/// ```gleam
/// type UserId { UserId(Int) }
///
/// let user_id = codec.map(codec.int(), UserId, fn(uid) {
///   let UserId(n) = uid
///   n
/// })
/// ```
pub fn map(c: Codec(a), wrap: fn(a) -> b, unwrap: fn(b) -> a) -> Codec(b) {
  Codec(
    encoder: fn(value) { c.encoder(unwrap(value)) },
    decoder: fn(data) {
      case c.decoder(data) {
        Ok(val) -> Ok(wrap(val))
        Error(e) -> Error(e)
      }
    },
    sized_decoder: fn(data) {
      case c.sized_decoder(data) {
        Ok(#(val, rest)) -> Ok(#(wrap(val), rest))
        Error(e) -> Error(e)
      }
    },
  )
}

// ---------------------------------------------------------------------------
// Error formatting
// ---------------------------------------------------------------------------

pub fn decode_error_to_string(error: DecodeError) -> String {
  case error {
    InvalidBinary(msg) -> "Invalid binary: " <> msg
    TypeMismatch(msg) -> "Type mismatch: " <> msg
    DecodeFailed(msg) -> "Decode failed: " <> msg
    InsufficientData(msg) -> "Insufficient data: " <> msg
    DecodeTimeout -> "Decode timeout"
    TagMismatch(expected, got) ->
      "Tag mismatch: expected '" <> expected <> "', got '" <> got <> "'"
    VersionMismatch(expected, got) ->
      "Version mismatch: expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
  }
}

pub fn encode_error_to_string(error: EncodeError) -> String {
  case error {
    InvalidValue(msg) -> "Invalid value: " <> msg
    EncodeFailed(msg) -> "Encode failed: " <> msg
    ValueTooLarge(msg) -> "Value too large: " <> msg
  }
}
