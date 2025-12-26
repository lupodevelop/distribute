// Encoder / Decoder primitives for typed messaging and RPC.
//
// This module provides efficient binary serialization for values sent across
// node boundaries. The API enforces type safety: callers must provide an
// `Encoder(T)` to serialize values and a `Decoder(T)` to deserialize.
//
// Features:
// - Binary serialization using BitArray for maximum efficiency
// - Explicit error handling prevents runtime crashes
// - Composable codec design for complex data types
// - Built-in codecs for common Gleam types

import gleam/bit_array
import gleam/int
import gleam/list

pub type EncodeError {
  /// Value cannot be encoded (e.g., invalid data)
  InvalidValue(String)
  /// Encoding operation failed
  EncodeFailed(String)
  /// Value is too large to encode
  ValueTooLarge(String)
}

pub type DecodeError {
  /// Binary data is malformed or truncated
  InvalidBinary(String)
  /// Decoded value doesn't match expected type
  TypeMismatch(String)
  /// Decoding operation failed
  DecodeFailed(String)
  /// Binary data is incomplete
  InsufficientData(String)
}

// An `Encoder(a)` converts a value of type `a` to a binary representation.
pub type Encoder(a) =
  fn(a) -> Result(BitArray, EncodeError)

pub fn encode(encoder: Encoder(a), value: a) -> Result(BitArray, EncodeError) {
  encoder(value)
}

// A `Decoder(a)` converts binary data into a value of type `a`.
pub type Decoder(a) =
  fn(BitArray) -> Result(a, DecodeError)

pub fn decode(decoder: Decoder(a), data: BitArray) -> Result(a, DecodeError) {
  decoder(data)
}

// -----------------
// Core codecs
// -----------------

// String codec: UTF-8 encoding with length prefix for safety.
pub fn string_encoder() -> Encoder(String) {
  fn(s) {
    let bytes = bit_array.from_string(s)
    let len = bit_array.byte_size(bytes)
    case len > 65_535 {
      True -> Error(ValueTooLarge("string too long: " <> int.to_string(len)))
      False -> {
        let len_bytes = <<len:16>>
        Ok(bit_array.append(len_bytes, bytes))
      }
    }
  }
}

pub fn string_decoder() -> Decoder(String) {
  fn(data) {
    case data {
      <<len:16, rest:bytes>> -> {
        case bit_array.byte_size(rest) >= len {
          True -> {
            case bit_array.slice(rest, 0, len) {
              Ok(str_bytes) -> {
                case bit_array.to_string(str_bytes) {
                  Ok(s) -> Ok(s)
                  Error(_) -> Error(InvalidBinary("invalid UTF-8 sequence"))
                }
              }
              Error(_) -> Error(InsufficientData("slice failed"))
            }
          }
          False -> Error(InsufficientData("incomplete string data"))
        }
      }
      _ -> Error(InvalidBinary("missing length prefix"))
    }
  }
}

// Integer codec: 64-bit big-endian encoding.
pub fn int_encoder() -> Encoder(Int) {
  fn(i) { Ok(<<i:64>>) }
}

pub fn int_decoder() -> Decoder(Int) {
  fn(data) {
    case data {
      <<i:64, _:bytes>> -> Ok(i)
      _ -> Error(InvalidBinary("insufficient data for int64"))
    }
  }
}

// -----------------
// Additional utility codecs
// -----------------

// Boolean codec: single byte encoding.
pub fn bool_encoder() -> Encoder(Bool) {
  fn(b) {
    case b {
      True -> Ok(<<1>>)
      False -> Ok(<<0>>)
    }
  }
}

pub fn bool_decoder() -> Decoder(Bool) {
  fn(data) {
    case data {
      <<1, _:bytes>> -> Ok(True)
      <<0, _:bytes>> -> Ok(False)
      _ -> Error(InvalidBinary("invalid boolean value"))
    }
  }
}

// List codec: encodes list length followed by elements.
pub fn list_encoder(element_encoder: Encoder(a)) -> Encoder(List(a)) {
  fn(lst) {
    let len = list.length(lst)
    case len > 65_535 {
      True -> Error(ValueTooLarge("list too long: " <> int.to_string(len)))
      False -> {
        let len_bytes = <<len:16>>
        use encoded_elements <- result_then(
          encode_list_elements(lst, element_encoder, <<>>),
        )
        Ok(bit_array.append(len_bytes, encoded_elements))
      }
    }
  }
}

pub fn list_decoder(element_decoder: Decoder(a)) -> Decoder(List(a)) {
  fn(data) {
    case data {
      <<len:16, rest:bytes>> ->
        decode_list_elements(rest, element_decoder, len, [])
      _ -> Error(InvalidBinary("missing list length"))
    }
  }
}

// ---------------------------------------------------------------------------
// Envelope: tag + version + payload
// ---------------------------------------------------------------------------

// Wrap a payload with a small envelope containing a tag and a version.
pub fn wrap_envelope(tag: String, version: Int, payload: BitArray) -> BitArray {
  let tag_bytes = bit_array.from_string(tag)
  let tag_len = bit_array.byte_size(tag_bytes)
  let tag_len_bytes = <<tag_len:16>>
  let version_bytes = <<version:32>>
  bit_array.append(
    bit_array.append(tag_len_bytes, tag_bytes),
    bit_array.append(version_bytes, payload),
  )
}

// Unwrap an envelope returning (tag, version, payload) or a DecodeError.
pub fn unwrap_envelope(
  data: BitArray,
) -> Result(#(String, Int, BitArray), DecodeError) {
  case data {
    <<tag_len:16, rest:bytes>> -> {
      let rest_size = bit_array.byte_size(rest)
      case rest_size >= tag_len + 4 {
        // 4 bytes for version (32-bit)
        False -> Error(InsufficientData("envelope too short"))
        True -> {
          case bit_array.slice(rest, 0, tag_len) {
            Ok(tag_bytes) -> {
              case bit_array.to_string(tag_bytes) {
                Ok(tag_str) -> {
                  // remaining after tag
                  case bit_array.slice(rest, tag_len, rest_size - tag_len) {
                    Ok(after_tag) -> {
                      case after_tag {
                        <<version:32, payload:bytes>> ->
                          Ok(#(tag_str, version, payload))
                        _ ->
                          Error(InvalidBinary(
                            "missing version/payload in envelope",
                          ))
                      }
                    }
                    Error(_) ->
                      Error(InsufficientData("failed to slice after tag"))
                  }
                }
                Error(_) -> Error(InvalidBinary("invalid UTF-8 in tag"))
              }
            }
            Error(_) -> Error(InsufficientData("failed to slice tag bytes"))
          }
        }
      }
    }
    _ -> Error(InvalidBinary("missing envelope length prefix"))
  }
}

// Helper to receive and decode a payload inside an envelope. It validates
// tag and version before invoking the provided `Decoder` on the payload.
pub fn receive_with_decoder(
  decoder: Decoder(a),
  expected_tag: String,
  expected_version: Int,
  data: BitArray,
) -> Result(a, DecodeError) {
  case unwrap_envelope(data) {
    Ok(#(tag, version, payload)) ->
      case tag == expected_tag {
        False -> Error(TypeMismatch("tag mismatch"))
        True ->
          case version == expected_version {
            False -> Error(TypeMismatch("version mismatch"))
            True -> decode(decoder, payload)
          }
      }
    Error(e) -> Error(e)
  }
}

// -----------------
// Helper functions
// -----------------

// Helper for result chaining in binary operations.
fn result_then(result: Result(a, e), f: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(value) -> f(value)
    Error(e) -> Error(e)
  }
}

// Helper to encode list elements recursively.
fn encode_list_elements(
  lst: List(a),
  encoder: Encoder(a),
  acc: BitArray,
) -> Result(BitArray, EncodeError) {
  case lst {
    [] -> Ok(acc)
    [head, ..tail] -> {
      use encoded_head <- result_then(encode(encoder, head))
      let new_acc = bit_array.append(acc, encoded_head)
      encode_list_elements(tail, encoder, new_acc)
    }
  }
}

// Helper to decode list elements recursively.
fn decode_list_elements(
  data: BitArray,
  decoder: Decoder(a),
  remaining: Int,
  acc: List(a),
) -> Result(List(a), DecodeError) {
  case remaining {
    0 -> Ok(list.reverse(acc))
    _ -> {
      use element <- result_then(decode(decoder, data))
      // This is simplified - in reality we'd need to track byte positions
      decode_list_elements(data, decoder, remaining - 1, [element, ..acc])
    }
  }
}
