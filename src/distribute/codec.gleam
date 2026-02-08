/// Binary serialization for cross-node type-safe messaging.
///
/// This module provides efficient binary encoding and decoding for values
/// transmitted across node boundaries. All operations are explicit and
/// return Result types to prevent runtime crashes.
///
/// ## Features
///
/// - Zero-copy binary serialization using BitArray
/// - Composable encoder/decoder design for complex types
/// - Built-in codecs for primitives and common Gleam types
/// - Explicit error handling with detailed error messages
/// - **SizedDecoder** for proper list/composite decoding with byte tracking
///
/// ## Submodules
///
/// - `distribute/codec/schema` — Schema helper for tag+version management and migrations
/// - `distribute/codec/composite` — Option, Result, and Tuple codecs
/// - `distribute/codec/advanced` — Dynamic, Pid, and Subject codecs (escape hatch)
/// - `distribute/codec/tagged` — Tagged union codec builder
/// - `distribute/codec/builder` — Schema builder DSL
///
/// ## Decoder Types
///
/// This module provides two decoder types:
///
/// - `Decoder(a)` — Simple decoder that consumes the entire input. Use for
///   top-level message decoding where the payload is the complete message.
///
/// - `SizedDecoder(a)` — Returns `#(value, remaining_bytes)`. Use for composing
///   decoders where you need to track how many bytes were consumed (e.g., lists,
///   tuples, nested structures).
///
/// Most built-in decoders have both variants (e.g., `int_decoder` and `int_sized_decoder`).
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
  /// Receive timeout when waiting for message
  DecodeTimeout
  /// Tag mismatch in envelope
  TagMismatch(expected: String, got: String)
  /// Version mismatch in envelope
  VersionMismatch(expected: Int, got: Int)
  /// Migration missing for a step (from_version)
  MigrationMissing(Int)
  /// Migration failed with reason
  MigrationFailed(String)
}

// ============================================================================
// Encoder type
// ============================================================================

/// An `Encoder(a)` converts a value of type `a` to a binary representation.
pub type Encoder(a) =
  fn(a) -> Result(BitArray, EncodeError)

/// Encode a value using the given encoder.
pub fn encode(encoder: Encoder(a), value: a) -> Result(BitArray, EncodeError) {
  encoder(value)
}

// ============================================================================
// Decoder types
// ============================================================================

/// A `Decoder(a)` converts binary data into a value of type `a`.
/// This is the simple decoder that consumes the input without tracking bytes.
pub type Decoder(a) =
  fn(BitArray) -> Result(a, DecodeError)

/// Decode binary data using the given decoder.
pub fn decode(decoder: Decoder(a), data: BitArray) -> Result(a, DecodeError) {
  decoder(data)
}

/// A `SizedDecoder(a)` converts binary data into a value of type `a` AND
/// returns the remaining unconsumed bytes. This is essential for composing
/// decoders (lists, tuples, nested structures).
pub type SizedDecoder(a) =
  fn(BitArray) -> Result(#(a, BitArray), DecodeError)

/// Decode binary data using a sized decoder, returning value and remaining bytes.
pub fn decode_sized(
  decoder: SizedDecoder(a),
  data: BitArray,
) -> Result(#(a, BitArray), DecodeError) {
  decoder(data)
}

/// Convert a SizedDecoder to a simple Decoder (discards remaining bytes).
pub fn to_decoder(sized: SizedDecoder(a)) -> Decoder(a) {
  fn(data) {
    case sized(data) {
      Ok(#(value, _remaining)) -> Ok(value)
      Error(e) -> Error(e)
    }
  }
}

// ============================================================================
// Core codecs - String
// ============================================================================

/// String encoder: UTF-8 encoding with 16-bit length prefix.
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

/// String sized decoder: returns decoded string and remaining bytes.
pub fn string_sized_decoder() -> SizedDecoder(String) {
  fn(data) {
    case data {
      <<len:16, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        case rest_size >= len {
          True -> {
            case bit_array.slice(rest, 0, len) {
              Ok(str_bytes) -> {
                case bit_array.to_string(str_bytes) {
                  Ok(s) -> {
                    case bit_array.slice(rest, len, rest_size - len) {
                      Ok(remaining) -> Ok(#(s, remaining))
                      Error(_) -> Ok(#(s, <<>>))
                    }
                  }
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

/// String decoder: simple decoder that discards remaining bytes.
pub fn string_decoder() -> Decoder(String) {
  to_decoder(string_sized_decoder())
}

// ============================================================================
// Core codecs - Int
// ============================================================================

/// Integer encoder: 64-bit big-endian encoding (8 bytes).
pub fn int_encoder() -> Encoder(Int) {
  fn(i) { Ok(<<i:64>>) }
}

/// Integer sized decoder: returns decoded int and remaining bytes.
pub fn int_sized_decoder() -> SizedDecoder(Int) {
  fn(data) {
    case data {
      <<i:64, rest:bytes>> -> Ok(#(i, rest))
      _ -> Error(InvalidBinary("insufficient data for int64"))
    }
  }
}

/// Integer decoder: simple decoder.
pub fn int_decoder() -> Decoder(Int) {
  to_decoder(int_sized_decoder())
}

// ============================================================================
// Core codecs - Float
// ============================================================================

/// Float encoder: 64-bit IEEE 754 encoding (8 bytes).
pub fn float_encoder() -> Encoder(Float) {
  fn(f) { Ok(<<f:float>>) }
}

/// Float sized decoder: returns decoded float and remaining bytes.
pub fn float_sized_decoder() -> SizedDecoder(Float) {
  fn(data) {
    case data {
      <<f:float, rest:bytes>> -> Ok(#(f, rest))
      _ -> Error(InvalidBinary("insufficient data for float64"))
    }
  }
}

/// Float decoder: simple decoder.
pub fn float_decoder() -> Decoder(Float) {
  to_decoder(float_sized_decoder())
}

// ============================================================================
// Core codecs - Bool
// ============================================================================

/// Boolean encoder: single byte (0 or 1).
pub fn bool_encoder() -> Encoder(Bool) {
  fn(b) {
    case b {
      True -> Ok(<<1>>)
      False -> Ok(<<0>>)
    }
  }
}

/// Boolean sized decoder: returns decoded bool and remaining bytes.
pub fn bool_sized_decoder() -> SizedDecoder(Bool) {
  fn(data) {
    case data {
      <<1, rest:bytes>> -> Ok(#(True, rest))
      <<0, rest:bytes>> -> Ok(#(False, rest))
      _ -> Error(InvalidBinary("invalid boolean value"))
    }
  }
}

/// Boolean decoder: simple decoder.
pub fn bool_decoder() -> Decoder(Bool) {
  to_decoder(bool_sized_decoder())
}

// ============================================================================
// Core codecs - BitArray (raw bytes with length prefix)
// ============================================================================

/// BitArray encoder: 32-bit length prefix followed by raw bytes.
pub fn bitarray_encoder() -> Encoder(BitArray) {
  fn(bytes) {
    let len = bit_array.byte_size(bytes)
    Ok(bit_array.append(<<len:32>>, bytes))
  }
}

/// BitArray sized decoder: returns decoded bytes and remaining.
pub fn bitarray_sized_decoder() -> SizedDecoder(BitArray) {
  fn(data) {
    case data {
      <<len:32, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        case rest_size >= len {
          True -> {
            case bit_array.slice(rest, 0, len) {
              Ok(bytes) -> {
                case bit_array.slice(rest, len, rest_size - len) {
                  Ok(remaining) -> Ok(#(bytes, remaining))
                  Error(_) -> Ok(#(bytes, <<>>))
                }
              }
              Error(_) -> Error(InsufficientData("bitarray slice failed"))
            }
          }
          False -> Error(InsufficientData("incomplete bitarray data"))
        }
      }
      _ -> Error(InvalidBinary("missing bitarray length prefix"))
    }
  }
}

/// BitArray decoder: simple decoder.
pub fn bitarray_decoder() -> Decoder(BitArray) {
  to_decoder(bitarray_sized_decoder())
}

// ============================================================================
// List codecs (now properly implemented with SizedDecoder)
// ============================================================================

/// List encoder: 16-bit length prefix followed by encoded elements.
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

/// List sized decoder using a SizedDecoder for elements.
/// This properly tracks byte consumption for each element.
pub fn list_sized_decoder(
  element_decoder: SizedDecoder(a),
) -> SizedDecoder(List(a)) {
  fn(data) {
    case data {
      <<len:16, rest:bytes>> ->
        decode_list_elements_sized(rest, element_decoder, len, [])
      _ -> Error(InvalidBinary("missing list length"))
    }
  }
}

/// List decoder using a SizedDecoder for elements.
pub fn list_decoder(element_decoder: SizedDecoder(a)) -> Decoder(List(a)) {
  to_decoder(list_sized_decoder(element_decoder))
}

/// List decoder using a simple Decoder - ONLY for fixed-size elements.
/// 
/// ⚠️ **WARNING**: This function only works correctly with fixed-size decoders
/// (e.g., int_decoder, float_decoder, bool_decoder). For variable-size elements
/// (strings, nested lists, custom types), use `list_decoder` with a SizedDecoder.
@deprecated("Use list_decoder with a SizedDecoder for proper byte tracking.")
pub fn list_decoder_fixed(element_decoder: Decoder(a)) -> Decoder(List(a)) {
  fn(data) {
    case data {
      <<len:16, rest:bytes>> ->
        decode_list_elements_legacy(rest, element_decoder, len, [])
      _ -> Error(InvalidBinary("missing list length"))
    }
  }
}

// ---------------------------------------------------------------------------
// Envelope: tag + version + payload
// ---------------------------------------------------------------------------

/// Wrap a payload with an envelope containing a protocol tag and version.
/// 
/// The envelope format is: `[tag_len:16][tag:utf8][version:32][payload]`
/// 
/// This enables protocol mismatch detection and versioned message handling.
/// 
/// ## Example
/// 
/// ```gleam
/// let payload = codec.encode(codec.string_encoder(), "hello")
/// let envelope = codec.wrap_envelope("my_protocol", 1, payload |> result.unwrap(<<>>))
/// ```
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

/// Unwrap an envelope, returning the tag, version, and payload.
/// 
/// Returns `DecodeError` if the envelope format is invalid.
/// 
/// ## Example
/// 
/// ```gleam
/// case codec.unwrap_envelope(data) {
///   Ok(#(tag, version, payload)) -> // Process message
///   Error(e) -> // Handle error
/// }
/// ```
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

// Schema, versioned decoders and migrations have been moved to:
// distribute/codec/schema

// ============================================================================
// Helper functions
// ============================================================================

/// Helper for result chaining in binary operations.
fn result_then(result: Result(a, e), f: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(value) -> f(value)
    Error(e) -> Error(e)
  }
}

/// Helper to encode list elements recursively.
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

/// Helper to decode list elements using SizedDecoder (proper implementation).
fn decode_list_elements_sized(
  data: BitArray,
  decoder: SizedDecoder(a),
  remaining: Int,
  acc: List(a),
) -> Result(#(List(a), BitArray), DecodeError) {
  case remaining {
    0 -> Ok(#(list.reverse(acc), data))
    _ -> {
      case decoder(data) {
        Ok(#(element, rest)) ->
          decode_list_elements_sized(rest, decoder, remaining - 1, [
            element,
            ..acc
          ])
        Error(e) -> Error(e)
      }
    }
  }
}

/// Legacy helper for fixed-size decoders (deprecated).
fn decode_list_elements_legacy(
  data: BitArray,
  decoder: Decoder(a),
  remaining: Int,
  acc: List(a),
) -> Result(List(a), DecodeError) {
  case remaining {
    0 -> Ok(list.reverse(acc))
    _ -> {
      // WARNING: This assumes fixed-size elements and reuses the same data
      use element <- result_then(decode(decoder, data))
      decode_list_elements_legacy(data, decoder, remaining - 1, [element, ..acc])
    }
  }
}

// Dynamic / Erlang Term Codecs (dynamic, any, pid, subject) have been moved to:
// distribute/codec/advanced

// Composite codecs (option, result, tuple) have been moved to:
// distribute/codec/composite

// ============================================================================
// Error formatting helpers
// ============================================================================

/// Format a DecodeError as a human-readable string.
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
    MigrationMissing(step) ->
      "Migration missing for step: " <> int.to_string(step)
    MigrationFailed(msg) -> "Migration failed: " <> msg
  }
}

/// Format an EncodeError as a human-readable string.
pub fn encode_error_to_string(error: EncodeError) -> String {
  case error {
    InvalidValue(msg) -> "Invalid value: " <> msg
    EncodeFailed(msg) -> "Encode failed: " <> msg
    ValueTooLarge(msg) -> "Value too large: " <> msg
  }
}
