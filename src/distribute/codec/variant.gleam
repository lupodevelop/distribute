import distribute/codec.{type Codec}
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type VariantStrategy(a) {
  VariantStrategy(
    id: Int,
    name: String,
    encoder: fn(a) -> Result(BitArray, Nil),
    decoder: codec.SizedDecoder(a),
  )
}

/// A builder for creating codecs for Gleam's Custom Types (ADTs).
pub opaque type VariantBuilder(a) {
  VariantBuilder(strategies: List(VariantStrategy(a)))
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

/// Create a new builder for an ADT codec.
pub fn new() -> VariantBuilder(a) {
  VariantBuilder(strategies: [])
}

/// Add a variant with a payload to the codec.
/// id: unique identifier (0-255). name: for error messages.
/// inner: codec for the payload. wrap: ADT constructor.
/// unwrap: extract payload or return Error(Nil) if wrong variant.
pub fn add(
  builder: VariantBuilder(a),
  id: Int,
  name: String,
  inner: Codec(b),
  wrap: fn(b) -> a,
  unwrap: fn(a) -> Result(b, Nil),
) -> VariantBuilder(a) {
  let strategy =
    VariantStrategy(
      id:,
      name:,
      encoder: fn(value) {
        case unwrap(value) {
          Ok(payload) -> {
            let assert Ok(bits) = inner.encoder(payload)
            Ok(bit_array.append(<<id:8>>, bits))
          }
          Error(_) -> Error(Nil)
        }
      },
      decoder: fn(data) {
        case data {
          <<tag:8, rest:bits>> if tag == id -> {
            case inner.sized_decoder(rest) {
              Ok(#(val, remaining)) -> Ok(#(wrap(val), remaining))
              Error(e) -> Error(e)
            }
          }
          _ -> Error(codec.TagMismatch(expected: name, got: "unknown"))
        }
      },
    )

  VariantBuilder([strategy, ..builder.strategies])
}

/// Add a variant without a payload (unit/constant).
pub fn unit(
  builder: VariantBuilder(a),
  id: Int,
  name: String,
  value: a,
  match: fn(a) -> Bool,
) -> VariantBuilder(a) {
  let strategy =
    VariantStrategy(
      id:,
      name:,
      encoder: fn(val) {
        case match(val) {
          True -> Ok(<<id:8>>)
          False -> Error(Nil)
        }
      },
      decoder: fn(data) {
        case data {
          <<tag:8, rest:bits>> if tag == id -> Ok(#(value, rest))
          _ -> Error(codec.TagMismatch(expected: name, got: "unknown"))
        }
      },
    )

  VariantBuilder([strategy, ..builder.strategies])
}

// ---------------------------------------------------------------------------
// Build
// ---------------------------------------------------------------------------

/// Finalize the builder and return a Codec(a).
/// Panics if duplicate IDs are found.
pub fn build(builder: VariantBuilder(a)) -> Codec(a) {
  let strategies = list.reverse(builder.strategies)
  validate_strategies(strategies)

  let encoder = fn(value) {
    case find_encoder(strategies, value) {
      Ok(bits) -> Ok(bits)
      Error(_) ->
        Error(codec.EncodeFailed("No variant matched for encoding ADT value"))
    }
  }

  let decoder_dict =
    list.fold(strategies, dict.new(), fn(acc, s) { dict.insert(acc, s.id, s) })

  let sized_decoder = fn(data) {
    case data {
      <<tag:8, _:bits>> -> {
        case dict.get(decoder_dict, tag) {
          Ok(strategy) -> strategy.decoder(data)
          Error(_) ->
            Error(codec.TagMismatch(
              expected: "one of "
                <> list.map(strategies, fn(s) { s.name })
              |> list.unique
              |> list.fold("", fn(acc, n) {
                case acc {
                  "" -> n
                  _ -> acc <> ", " <> n
                }
              }),
              got: int.to_string(tag),
            ))
        }
      }
      _ -> Error(codec.InsufficientData("missing variant tag"))
    }
  }

  codec.Codec(
    encoder:,
    decoder: codec.to_decoder(sized_decoder),
    sized_decoder:,
  )
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn find_encoder(
  strategies: List(VariantStrategy(a)),
  value: a,
) -> Result(BitArray, Nil) {
  case strategies {
    [] -> Error(Nil)
    [s, ..rest] ->
      case s.encoder(value) {
        Ok(bits) -> Ok(bits)
        Error(_) -> find_encoder(rest, value)
      }
  }
}

fn validate_strategies(strategies: List(VariantStrategy(a))) -> Nil {
  let ids = list.map(strategies, fn(s) { s.id })
  case list.unique(ids) |> list.length == list.length(ids) {
    True -> Nil
    False -> panic as "Variant codec: Duplicate IDs detected in builder"
  }
}
