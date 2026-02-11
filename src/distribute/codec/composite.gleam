/// Codecs for Option, Result, and Tuple types.
import distribute/codec.{
  type Decoder, type Encoder, type SizedDecoder, InsufficientData, InvalidBinary,
}
import gleam/bit_array
import gleam/option.{type Option}
import gleam/result

// ============================================================================
// Option codecs
// ============================================================================

/// Encoder for `Option(a)`. None → `<<0>>`, Some(v) → `<<1, encoded_v>>`.
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

pub fn option_sized_decoder(inner: SizedDecoder(a)) -> SizedDecoder(Option(a)) {
  fn(data) {
    case data {
      <<0, rest:bytes>> -> Ok(#(option.None, rest))
      <<1, rest:bytes>> -> {
        use #(value, remaining) <- result.try(inner(rest))
        Ok(#(option.Some(value), remaining))
      }
      _ -> Error(InvalidBinary("invalid option tag"))
    }
  }
}

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

/// Encoder for `Result(a, e)`. Ok → `<<0, encoded>>`, Error → `<<1, encoded>>`.
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

pub fn result_sized_decoder(
  ok_decoder: SizedDecoder(a),
  error_decoder: SizedDecoder(e),
) -> SizedDecoder(Result(a, e)) {
  fn(data) {
    case data {
      <<0, rest:bytes>> -> {
        use #(value, remaining) <- result.try(ok_decoder(rest))
        Ok(#(Ok(value), remaining))
      }
      <<1, rest:bytes>> -> {
        use #(err, remaining) <- result.try(error_decoder(rest))
        Ok(#(Error(err), remaining))
      }
      _ -> Error(InvalidBinary("invalid result tag"))
    }
  }
}

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

/// Encoder for `#(a, b)`. First element is length-prefixed (32-bit).
pub fn tuple2_encoder(first: Encoder(a), second: Encoder(b)) -> Encoder(#(a, b)) {
  fn(tuple) {
    let #(a, b) = tuple
    use encoded_a <- result.try(first(a))
    use encoded_b <- result.try(second(b))
    let len_a = bit_array.byte_size(encoded_a)
    Ok(bit_array.concat([<<len_a:32>>, encoded_a, encoded_b]))
  }
}

pub fn tuple2_sized_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
) -> SizedDecoder(#(a, b)) {
  fn(data) {
    case data {
      <<len_a:32, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        use first_data <- result.try(
          bit_array.slice(rest, 0, len_a)
          |> result.replace_error(InsufficientData("tuple2 first")),
        )
        use #(a, _) <- result.try(first(first_data))
        use after_first <- result.try(
          bit_array.slice(rest, len_a, rest_size - len_a)
          |> result.replace_error(InsufficientData("tuple2 second")),
        )
        use #(b, remaining) <- result.try(second(after_first))
        Ok(#(#(a, b), remaining))
      }
      _ -> Error(InvalidBinary("missing tuple2 length prefix"))
    }
  }
}

pub fn tuple2_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
) -> Decoder(#(a, b)) {
  codec.to_decoder(tuple2_sized_decoder(first, second))
}

// ============================================================================
// Tuple3 codecs
// ============================================================================

/// Encoder for `#(a, b, c)`. First two elements are length-prefixed (32-bit).
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

pub fn tuple3_sized_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
  third: SizedDecoder(c),
) -> SizedDecoder(#(a, b, c)) {
  fn(data) {
    case data {
      <<len_a:32, rest:bytes>> -> {
        let rest_size = bit_array.byte_size(rest)
        use first_data <- result.try(
          bit_array.slice(rest, 0, len_a)
          |> result.replace_error(InsufficientData("tuple3 first")),
        )
        use #(a, _) <- result.try(first(first_data))
        use after_first <- result.try(
          bit_array.slice(rest, len_a, rest_size - len_a)
          |> result.replace_error(InsufficientData("tuple3 after first")),
        )
        case after_first {
          <<len_b:32, rest2:bytes>> -> {
            let rest2_size = bit_array.byte_size(rest2)
            use second_data <- result.try(
              bit_array.slice(rest2, 0, len_b)
              |> result.replace_error(InsufficientData("tuple3 second")),
            )
            use #(b, _) <- result.try(second(second_data))
            use third_data <- result.try(
              bit_array.slice(rest2, len_b, rest2_size - len_b)
              |> result.replace_error(InsufficientData("tuple3 third")),
            )
            use #(c, remaining) <- result.try(third(third_data))
            Ok(#(#(a, b, c), remaining))
          }
          _ -> Error(InvalidBinary("tuple3 missing second length"))
        }
      }
      _ -> Error(InvalidBinary("tuple3 missing length"))
    }
  }
}

pub fn tuple3_decoder(
  first: SizedDecoder(a),
  second: SizedDecoder(b),
  third: SizedDecoder(c),
) -> Decoder(#(a, b, c)) {
  codec.to_decoder(tuple3_sized_decoder(first, second, third))
}

// ============================================================================
// Bundled Codec versions
// ============================================================================

pub fn option(inner: codec.Codec(a)) -> codec.Codec(Option(a)) {
  codec.Codec(
    encoder: option_encoder(inner.encoder),
    decoder: option_decoder(inner.decoder),
    sized_decoder: option_sized_decoder(inner.sized_decoder),
  )
}

pub fn tuple2(
  first: codec.Codec(a),
  second: codec.Codec(b),
) -> codec.Codec(#(a, b)) {
  codec.Codec(
    encoder: tuple2_encoder(first.encoder, second.encoder),
    decoder: tuple2_decoder(first.sized_decoder, second.sized_decoder),
    sized_decoder: tuple2_sized_decoder(
      first.sized_decoder,
      second.sized_decoder,
    ),
  )
}

pub fn tuple3(
  first: codec.Codec(a),
  second: codec.Codec(b),
  third: codec.Codec(c),
) -> codec.Codec(#(a, b, c)) {
  codec.Codec(
    encoder: tuple3_encoder(first.encoder, second.encoder, third.encoder),
    decoder: tuple3_decoder(
      first.sized_decoder,
      second.sized_decoder,
      third.sized_decoder,
    ),
    sized_decoder: tuple3_sized_decoder(
      first.sized_decoder,
      second.sized_decoder,
      third.sized_decoder,
    ),
  )
}
