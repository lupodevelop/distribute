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
/// - **Schema** helper for convenient tag+version management
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
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result

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

// ============================================================================
// Schema: Convenient tag+version management
// ============================================================================

/// A Schema represents a message type with its tag, version, encoder and decoder.
/// Use this to ensure consistent encoding/decoding across your application.
pub type Schema(a) {
  Schema(tag: String, version: Int, encoder: Encoder(a), decoder: Decoder(a))
}

/// Create a new schema with the given tag, version, encoder, and decoder.
pub fn new_schema(
  tag tag: String,
  version version: Int,
  encoder encoder: Encoder(a),
  decoder decoder: Decoder(a),
) -> Schema(a) {
  Schema(tag: tag, version: version, encoder: encoder, decoder: decoder)
}

/// Encode a value using a schema. Automatically wraps in envelope with tag+version.
pub fn schema_encode(
  schema: Schema(a),
  value: a,
) -> Result(BitArray, EncodeError) {
  case schema.encoder(value) {
    Ok(payload) -> Ok(wrap_envelope(schema.tag, schema.version, payload))
    Error(e) -> Error(e)
  }
}

/// Decode a value using a schema. Validates tag and version before decoding.
pub fn schema_decode(
  schema: Schema(a),
  data: BitArray,
) -> Result(a, DecodeError) {
  case unwrap_envelope(data) {
    Ok(#(tag, version, payload)) -> {
      case tag == schema.tag {
        False -> Error(TagMismatch(expected: schema.tag, got: tag))
        True ->
          case version == schema.version {
            False ->
              Error(VersionMismatch(expected: schema.version, got: version))
            True -> schema.decoder(payload)
          }
      }
    }
    Error(e) -> Error(e)
  }
}

/// Check if binary data matches a schema's tag (without full decode).
/// Useful for routing messages to the correct handler.
pub fn schema_matches_tag(schema: Schema(a), data: BitArray) -> Bool {
  case unwrap_envelope(data) {
    Ok(#(tag, _, _)) -> tag == schema.tag
    Error(_) -> False
  }
}

/// Get the tag from a binary envelope without full decode.
pub fn peek_tag(data: BitArray) -> Result(String, DecodeError) {
  case unwrap_envelope(data) {
    Ok(#(tag, _, _)) -> Ok(tag)
    Error(e) -> Error(e)
  }
}

/// Get tag and version from a binary envelope without decoding payload.
pub fn peek_envelope(data: BitArray) -> Result(#(String, Int), DecodeError) {
  case unwrap_envelope(data) {
    Ok(#(tag, version, _)) -> Ok(#(tag, version))
    Error(e) -> Error(e)
  }
}

/// Create a versioned migration path between schema versions.
/// Returns a decoder that can handle multiple versions.
pub fn versioned_decoder(
  tag: String,
  handlers: List(#(Int, Decoder(a))),
) -> Decoder(a) {
  fn(data) {
    case unwrap_envelope(data) {
      Ok(#(actual_tag, version, payload)) -> {
        case actual_tag == tag {
          False -> Error(TagMismatch(expected: tag, got: actual_tag))
          True -> {
            case list.find(handlers, fn(h) { h.0 == version }) {
              Ok(#(_, decoder)) -> decoder(payload)
              Error(_) ->
                Error(VersionMismatch(
                  expected: -1,
                  // -1 indicates "any supported version"
                  got: version,
                ))
            }
          }
        }
      }
      Error(e) -> Error(e)
    }
  }
}

/// Build a versioned decoder from a list of `Schema(a)`.
/// All schemas must share the same tag. Returns a decoder that picks the
/// appropriate schema by version and decodes the payload.
pub fn versioned_decoder_from_schemas(schemas: List(Schema(a))) -> Decoder(a) {
  fn(data) {
    case schemas {
      [] -> Error(InvalidBinary("no schemas provided"))
      [Schema(tag: tag, version: _, encoder: _, decoder: _), ..] -> {
        let handlers = list.map(schemas, fn(s) { #(s.version, s.decoder) })
        versioned_decoder(tag, handlers)(data)
      }
    }
  }
}

/// Decode a schema applying migrations when necessary.
///
/// - `target_schema` is the desired schema to decode into (its decoder is used).
/// - `migrations` is a list of pairs `(version, migr_fn)` where `migr_fn`
///    transforms an older payload into the payload expected by the target schema.
///
/// If a payload arrives with an older version, the corresponding migration
/// is applied before decoding. If no migration is found the decoder returns
/// `VersionMismatch`.
pub fn schema_decode_with_migrations(
  target_schema: Schema(a),
  migrations: List(#(Int, fn(BitArray) -> Result(BitArray, DecodeError))),
) -> Decoder(a) {
  fn(data) {
    case unwrap_envelope(data) {
      Ok(#(tag, version, payload)) -> {
        case tag == target_schema.tag {
          False -> Error(TagMismatch(expected: target_schema.tag, got: tag))
          True -> {
            case version == target_schema.version {
              True -> target_schema.decoder(payload)
              False ->
                // If a direct migration exists for this exact version apply it
                case list.find(migrations, fn(m) { m.0 == version }) {
                  Ok(#(_, migr)) ->
                    case migr(payload) {
                      Ok(new_payload) -> target_schema.decoder(new_payload)
                      Error(e) -> Error(e)
                    }
                  Error(_) -> {
                    // Otherwise try to build a migration chain from single-step migrations
                    let chain = build_migration_chain(migrations)
                    case chain(version, target_schema.version, payload) {
                      Ok(new_payload) -> target_schema.decoder(new_payload)
                      Error(e) -> Error(e)
                    }
                  }
                }
            }
          }
        }
      }
      Error(e) -> Error(e)
    }
  }
}

// ---------------------------------------------------------------------------
// Migration chain helper
// ---------------------------------------------------------------------------

/// A single-step migration: transforms payload for version `from` into
/// payload for version `from + 1`.
pub type Migration =
  #(Int, fn(BitArray) -> Result(BitArray, DecodeError))

/// Build a migration chain from single-step migrations.
///
/// The returned function has signature: fn(from_version, to_version, payload)
/// and applies the sequence of migrations (from->from+1, ... -> to) if possible.
/// If a step is missing, returns `MigrationMissing(step)`.
pub fn build_migration_chain(
  migrations: List(Migration),
) -> fn(Int, Int, BitArray) -> Result(BitArray, DecodeError) {
  fn(from, to, payload) {
    case from == to {
      True -> Ok(payload)
      False ->
        case from > to {
          True -> Error(MigrationFailed("downgrade not supported"))
          False -> apply_steps(from, to, payload, migrations)
        }
    }
  }
}

fn apply_steps(
  current: Int,
  target: Int,
  payload: BitArray,
  migrations: List(Migration),
) -> Result(BitArray, DecodeError) {
  case current == target {
    True -> Ok(payload)
    False ->
      case list.find(migrations, fn(m) { m.0 == current }) {
        Ok(#(_, migr)) ->
          case migr(payload) {
            Ok(next_payload) ->
              apply_steps(current + 1, target, next_payload, migrations)
            Error(e) -> Error(MigrationFailed(decode_error_to_string(e)))
          }
        Error(_) -> Error(MigrationMissing(current))
      }
  }
}

// ---------------------------------------------------------------------------
// Migration graph (multi-step migration sugar)
// ---------------------------------------------------------------------------

pub type MigrationEdge =
  #(Int, Int, fn(BitArray) -> Result(BitArray, DecodeError))

/// Build a migration graph from a list of migration edges.
/// Each edge is a single migration from `from` -> `to` (not necessarily +1).
/// The returned function will find a path from `from` to `to` (if any) and
/// apply each edge's migration in sequence. If no path exists, returns
/// `MigrationMissing(step)` where `step` is the first missing intermediate version.
pub fn build_migration_graph(
  edges: List(MigrationEdge),
) -> fn(Int, Int, BitArray) -> Result(BitArray, DecodeError) {
  fn(from, to, payload) {
    case find_path(edges, [from], to) {
      Ok(path) -> apply_path_edges(edges, list.reverse(path), payload)
      Error(_) -> Error(MigrationMissing(from))
    }
  }
}

// Depth-first search for a path from the last element of `path` to `goal`.
// Returns the full version path as a list if found.
fn find_path(
  edges: List(MigrationEdge),
  path: List(Int),
  goal: Int,
) -> Result(List(Int), DecodeError) {
  case path {
    [] -> Error(MigrationMissing(goal))
    [last, ..] ->
      case last == goal {
        True -> Ok(path)
        False ->
          find_path_neighbors(
            edges,
            path,
            list.fold(edges, [], fn(acc, e) {
              case e {
                #(from, to, _migr) ->
                  case from == last {
                    True -> [to, ..acc]
                    False -> acc
                  }
              }
            }),
            goal,
          )
      }
  }
}

fn find_path_neighbors(
  edges: List(MigrationEdge),
  path: List(Int),
  neighbors: List(Int),
  goal: Int,
) -> Result(List(Int), DecodeError) {
  case neighbors {
    [] -> Error(MigrationMissing(goal))
    [n, ..rest] ->
      // avoid cycles
      case list.contains(path, n) {
        True -> find_path_neighbors(edges, path, rest, goal)
        False ->
          case find_path(edges, [n, ..path], goal) {
            Ok(found_path) -> Ok(found_path)
            Error(_) -> find_path_neighbors(edges, path, rest, goal)
          }
      }
  }
}

// Apply migrations along a found path of versions [v0, v1, v2, ..., vn].
// Returns the transformed payload or a MigrationMissing/MigrationFailed error.
fn apply_path_edges(
  edges: List(MigrationEdge),
  versions: List(Int),
  payload: BitArray,
) -> Result(BitArray, DecodeError) {
  case versions {
    [] -> Ok(payload)
    [_single] -> Ok(payload)
    [from, to, ..rest] ->
      case find_edge(edges, from, to) {
        Ok(migr) ->
          case migr(payload) {
            Ok(next) -> apply_path_edges(edges, [to, ..rest], next)
            Error(e) -> Error(MigrationFailed(decode_error_to_string(e)))
          }
        Error(_) -> Error(MigrationMissing(from))
      }
  }
}

fn find_edge(
  edges: List(MigrationEdge),
  from: Int,
  to: Int,
) -> Result(fn(BitArray) -> Result(BitArray, DecodeError), DecodeError) {
  case list.find(edges, fn(e) { e.0 == from && e.1 == to }) {
    Ok(#(_, _, migr)) -> Ok(migr)
    Error(_) -> Error(MigrationMissing(from))
  }
}

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

// ============================================================================
// Dynamic / Erlang Term Codecs
// ============================================================================
//
// ⚠️ **WARNING**: These codecs use Erlang's `term_to_binary`/`binary_to_term`
// which bypasses Gleam's type system. They are provided as an **escape hatch**
// for advanced use cases but should be avoided when possible.
//
// **Risks:**
// - No compile-time type checking
// - Binary format may change between Erlang/OTP versions
// - Type changes in your code will cause silent runtime failures
// - Not suitable for long-term storage or cross-language communication
//
// **When to use:**
// - Temporary serialization within a single running cluster
// - Pid/Subject transmission (no other option on BEAM)
// - Prototyping before implementing proper codecs
//
// **Prefer instead:**
// - Built-in typed codecs (string, int, bool, etc.)
// - Custom codecs for your domain types
// - Schema-based encoding for versioned protocols

@external(erlang, "erlang", "term_to_binary")
fn term_to_binary(x: a) -> BitArray

@external(erlang, "codec_ffi", "safe_binary_to_term")
fn safe_binary_to_term(b: BitArray) -> Result(Dynamic, Dynamic)

/// Encoder for any Gleam/Erlang term using `term_to_binary`.
///
/// ⚠️ **ESCAPE HATCH** — This encoder bypasses Gleam's type system.
/// The binary format is opaque and may not be portable across:
/// - Different Erlang/OTP versions
/// - Different Gleam versions
/// - Type definition changes in your code
///
/// Use typed codecs (string_encoder, int_encoder, etc.) whenever possible.
/// This is suitable only for temporary in-cluster communication or prototyping.
pub fn dynamic_encoder() -> Encoder(Dynamic) {
  fn(d) { Ok(term_to_binary(d)) }
}

/// Decoder for any Gleam/Erlang term using `binary_to_term`.
///
/// ⚠️ **ESCAPE HATCH** — This decoder bypasses Gleam's type system.
/// The returned `Dynamic` must be validated using `gleam/dynamic` decoders.
/// Binary data from untrusted sources should be treated with extreme caution.
///
/// Uses `binary_to_term([safe])` to prevent atom table attacks, but the
/// decoded value still requires runtime type checking.
pub fn dynamic_decoder() -> Decoder(Dynamic) {
  fn(b) {
    case safe_binary_to_term(b) {
      Ok(d) -> Ok(d)
      Error(_) -> Error(InvalidBinary("invalid erlang term"))
    }
  }
}

/// Encode any Gleam value to binary using Erlang term serialization.
///
/// ⚠️ **ESCAPE HATCH** — Bypasses type safety. See `dynamic_encoder` warnings.
/// This is a convenience function for quick prototyping.
pub fn any_encoder() -> Encoder(a) {
  fn(value) { Ok(term_to_binary(value)) }
}

@external(erlang, "codec_ffi", "unsafe_coerce")
fn unsafe_coerce(x: Dynamic) -> a

/// Encoder for Pids.
///
/// Note: Pids are inherently untyped in Erlang. This encoder uses
/// `term_to_binary` which is safe for same-cluster communication.
pub fn pid_encoder() -> Encoder(Pid) {
  fn(p) { Ok(term_to_binary(p)) }
}

/// Decoder for Pids.
///
/// Note: The decoded Pid is validated as a proper Erlang pid internally.
pub fn pid_decoder() -> Decoder(Pid) {
  fn(b) {
    case safe_binary_to_term(b) {
      Ok(d) -> Ok(unsafe_coerce(d))
      Error(_) -> Error(InvalidBinary("invalid pid binary"))
    }
  }
}

/// Encoder for Subjects.
///
/// Note: Subject serialization preserves the Pid but the type parameter
/// is erased. The receiving side must know the expected message type.
pub fn subject_encoder() -> Encoder(Subject(a)) {
  fn(s) { Ok(term_to_binary(s)) }
}

/// Decoder for Subjects.
///
/// ⚠️ **Type parameter is not validated** — The returned Subject(a) will
/// accept any type parameter at compile time. Ensure the type matches
/// what was encoded, or use Schema-based messaging for safety.
pub fn subject_decoder() -> Decoder(Subject(a)) {
  fn(b) {
    case safe_binary_to_term(b) {
      Ok(d) -> Ok(unsafe_coerce(d))
      Error(_) -> Error(InvalidBinary("invalid subject binary"))
    }
  }
}

// ============================================================================
// Composite codecs
// ============================================================================

/// Codec for Option(a).
pub fn option_encoder(inner: Encoder(a)) -> Encoder(Option(a)) {
  fn(opt) {
    case opt {
      option.None -> Ok(<<0>>)
      option.Some(value) -> {
        use encoded <- result.try(inner(value))
        Ok(bit_array.append(<<1>>, encoded))
      }
    }
  }
}

/// Option sized decoder.
pub fn option_sized_decoder(inner: SizedDecoder(a)) -> SizedDecoder(Option(a)) {
  fn(data) {
    case data {
      <<0, rest:bytes>> -> Ok(#(option.None, rest))
      <<1, rest:bytes>> -> {
        case inner(rest) {
          Ok(#(value, remaining)) -> Ok(#(option.Some(value), remaining))
          Error(e) -> Error(e)
        }
      }
      _ -> Error(InvalidBinary("invalid option tag"))
    }
  }
}

/// Option decoder (simple).
pub fn option_decoder(inner: Decoder(a)) -> Decoder(Option(a)) {
  fn(data) {
    case data {
      <<0, _:bytes>> -> Ok(option.None)
      <<1, rest:bytes>> -> {
        use value <- result.try(inner(rest))
        Ok(option.Some(value))
      }
      _ -> Error(InvalidBinary("invalid option tag"))
    }
  }
}

/// Codec for Result(a, e).
pub fn result_encoder(
  ok_encoder: Encoder(a),
  error_encoder: Encoder(e),
) -> Encoder(Result(a, e)) {
  fn(res) {
    case res {
      Ok(value) -> {
        use encoded <- result.try(ok_encoder(value))
        Ok(bit_array.append(<<0>>, encoded))
      }
      Error(err) -> {
        use encoded <- result.try(error_encoder(err))
        Ok(bit_array.append(<<1>>, encoded))
      }
    }
  }
}

/// Result sized decoder.
pub fn result_sized_decoder(
  ok_decoder: SizedDecoder(a),
  error_decoder: SizedDecoder(e),
) -> SizedDecoder(Result(a, e)) {
  fn(data) {
    case data {
      <<0, rest:bytes>> -> {
        case ok_decoder(rest) {
          Ok(#(value, remaining)) -> Ok(#(Ok(value), remaining))
          Error(e) -> Error(e)
        }
      }
      <<1, rest:bytes>> -> {
        case error_decoder(rest) {
          Ok(#(err, remaining)) -> Ok(#(Error(err), remaining))
          Error(e) -> Error(e)
        }
      }
      _ -> Error(InvalidBinary("invalid result tag"))
    }
  }
}

/// Result decoder (simple).
pub fn result_decoder(
  ok_decoder: Decoder(a),
  error_decoder: Decoder(e),
) -> Decoder(Result(a, e)) {
  fn(data) {
    case data {
      <<0, rest:bytes>> -> {
        use value <- result.try(ok_decoder(rest))
        Ok(Ok(value))
      }
      <<1, rest:bytes>> -> {
        use err <- result.try(error_decoder(rest))
        Ok(Error(err))
      }
      _ -> Error(InvalidBinary("invalid result tag"))
    }
  }
}

// ============================================================================
// Tuple codecs
// ============================================================================

/// Tuple2 encoder with length prefixes for proper boundary tracking.
pub fn tuple2_encoder(first: Encoder(a), second: Encoder(b)) -> Encoder(#(a, b)) {
  fn(tuple) {
    let #(a, b) = tuple
    use encoded_a <- result.try(first(a))
    use encoded_b <- result.try(second(b))
    // Prefix first element with length for proper parsing
    let len_a = bit_array.byte_size(encoded_a)
    Ok(bit_array.concat([<<len_a:32>>, encoded_a, encoded_b]))
  }
}

/// Tuple2 sized decoder.
pub fn tuple2_sized_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
) -> SizedDecoder(#(a, b)) {
  fn(data) {
    case data {
      <<len_a:32, rest:bytes>> -> {
        case bit_array.slice(rest, 0, len_a) {
          Ok(first_data) -> {
            case first(first_data) {
              Ok(#(a, _)) -> {
                let rest_size = bit_array.byte_size(rest)
                case bit_array.slice(rest, len_a, rest_size - len_a) {
                  Ok(second_data) -> {
                    case second(second_data) {
                      Ok(#(b, remaining)) -> Ok(#(#(a, b), remaining))
                      Error(e) -> Error(e)
                    }
                  }
                  Error(_) -> Error(InsufficientData("tuple2 second element"))
                }
              }
              Error(e) -> Error(e)
            }
          }
          Error(_) -> Error(InsufficientData("tuple2 first element"))
        }
      }
      _ -> Error(InvalidBinary("missing tuple2 length prefix"))
    }
  }
}

/// Tuple2 decoder (simple).
pub fn tuple2_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
) -> Decoder(#(a, b)) {
  to_decoder(tuple2_sized_decoder(first, second))
}

/// Tuple3 encoder.
pub fn tuple3_encoder(
  first: Encoder(a),
  second: Encoder(b),
  third: Encoder(c),
) -> Encoder(#(a, b, c)) {
  fn(tuple) {
    let #(a, b, c) = tuple
    use encoded_a <- result.try(first(a))
    use encoded_b <- result.try(second(b))
    use encoded_c <- result.try(third(c))
    let len_a = bit_array.byte_size(encoded_a)
    let len_b = bit_array.byte_size(encoded_b)
    Ok(
      bit_array.concat([
        <<len_a:32>>,
        encoded_a,
        <<len_b:32>>,
        encoded_b,
        encoded_c,
      ]),
    )
  }
}

/// Tuple3 sized decoder.
pub fn tuple3_sized_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
  third: SizedDecoder(c),
) -> SizedDecoder(#(a, b, c)) {
  fn(data) {
    case data {
      <<len_a:32, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        case bit_array.slice(rest, 0, len_a) {
          Ok(first_data) -> {
            case first(first_data) {
              Ok(#(a, _)) -> {
                case bit_array.slice(rest, len_a, rest_size - len_a) {
                  Ok(after_a) -> {
                    case after_a {
                      <<len_b:32, rest2:bytes>> -> {
                        let rest2_size = bit_array.byte_size(rest2)
                        case bit_array.slice(rest2, 0, len_b) {
                          Ok(second_data) -> {
                            case second(second_data) {
                              Ok(#(b, _)) -> {
                                case
                                  bit_array.slice(
                                    rest2,
                                    len_b,
                                    rest2_size - len_b,
                                  )
                                {
                                  Ok(third_data) -> {
                                    case third(third_data) {
                                      Ok(#(c, remaining)) ->
                                        Ok(#(#(a, b, c), remaining))
                                      Error(e) -> Error(e)
                                    }
                                  }
                                  Error(_) ->
                                    Error(InsufficientData("tuple3 third"))
                                }
                              }
                              Error(e) -> Error(e)
                            }
                          }
                          Error(_) ->
                            Error(InsufficientData("tuple3 second slice"))
                        }
                      }
                      _ -> Error(InvalidBinary("tuple3 missing second length"))
                    }
                  }
                  Error(_) -> Error(InsufficientData("tuple3 after first"))
                }
              }
              Error(e) -> Error(e)
            }
          }
          Error(_) -> Error(InsufficientData("tuple3 first"))
        }
      }
      _ -> Error(InvalidBinary("tuple3 missing length"))
    }
  }
}

/// Tuple3 decoder (simple).
pub fn tuple3_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
  third: SizedDecoder(c),
) -> Decoder(#(a, b, c)) {
  to_decoder(tuple3_sized_decoder(first, second, third))
}

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
