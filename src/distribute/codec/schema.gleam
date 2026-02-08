/// Schema-based codec management with versioning and migrations.
///
/// This module extends the core codec with:
/// - `Schema(a)` for associating tag+version with an encoder/decoder pair
/// - Envelope peek helpers for routing without full decode
/// - Versioned decoders for handling multiple protocol versions
/// - Migration chains and graphs for payload upgrades
///
/// ## Example
///
/// ```gleam
/// import distribute/codec
/// import distribute/codec/schema
///
/// let user_schema = schema.new_schema(
///   tag: "user",
///   version: 1,
///   encoder: my_user_encoder(),
///   decoder: my_user_decoder(),
/// )
///
/// let assert Ok(data) = schema.schema_encode(user_schema, my_user)
/// let assert Ok(user) = schema.schema_decode(user_schema, data)
/// ```

import distribute/codec.{
  type DecodeError, type Decoder, type EncodeError, type Encoder,
  InsufficientData, InvalidBinary, MigrationFailed, MigrationMissing,
  TagMismatch, VersionMismatch, decode_error_to_string, unwrap_envelope,
  wrap_envelope,
}
import gleam/int
import gleam/list

// ============================================================================
// Schema type
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

// ============================================================================
// Schema encode / decode
// ============================================================================

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

// ============================================================================
// Envelope peek helpers
// ============================================================================

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

// ============================================================================
// Versioned decoders
// ============================================================================

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

// ============================================================================
// Schema decode with migrations
// ============================================================================

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

// ============================================================================
// Migration chain
// ============================================================================

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

// ============================================================================
// Migration graph (multi-step, non-linear)
// ============================================================================

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
