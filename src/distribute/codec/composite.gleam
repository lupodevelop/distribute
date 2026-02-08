/// Composite codecs for Option, Result, and Tuple types.
///
/// These codecs handle Gleam's standard composite types and are built
/// on top of the core codec primitives.
///
/// ## Example
///
/// ```gleam
/// import distribute/codec
/// import distribute/codec/composite
///
/// let enc = composite.option_encoder(codec.int_encoder())
/// let dec = composite.option_decoder(codec.int_decoder())
/// ```

import distribute/codec.{
  type DecodeError, type Decoder, type EncodeError, type Encoder,
  type SizedDecoder, InsufficientData, InvalidBinary,
}
import gleam/bit_array
import gleam/option.{type Option}
import gleam/result

// ============================================================================
// Option codecs
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

// ============================================================================
// Result codecs
// ============================================================================

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
// Tuple2 codecs
// ============================================================================

/// Encoder for 2-tuples. Encodes both elements with length prefix for first.
///
/// ## Example
///
/// ```gleam
/// let enc = composite.tuple2_encoder(codec.int_encoder(), codec.string_encoder())
/// let encoded = codec.encode(enc, #(42, "hello"))
/// ```
pub fn tuple2_encoder(
  first: Encoder(a),
  second: Encoder(b),
) -> Encoder(#(a, b)) {
  fn(tuple) {
    let #(a, b) = tuple
    use encoded_a <- result.try(first(a))
    use encoded_b <- result.try(second(b))
    // Prefix first element with length for proper parsing
    let len_a = bit_array.byte_size(encoded_a)
    Ok(bit_array.concat([<<len_a:32>>, encoded_a, encoded_b]))
  }
}

/// SizedDecoder for 2-tuples. Returns the decoded tuple and remaining bytes.
/// Use with `to_decoder()` for simple decoding.
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

/// Simple decoder for 2-tuples.
///
/// ## Example
///
/// ```gleam
/// let dec = composite.tuple2_decoder(
///   codec.int_sized_decoder(),
///   codec.string_sized_decoder()
/// )
/// let Ok(#(num, text)) = codec.decode(dec, encoded)
/// ```
pub fn tuple2_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
) -> Decoder(#(a, b)) {
  codec.to_decoder(tuple2_sized_decoder(first, second))
}

// ============================================================================
// Tuple3 codecs
// ============================================================================

/// Encoder for 3-tuples. Encodes all three elements with length prefixes.
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
  codec.to_decoder(tuple3_sized_decoder(first, second, third))
}
