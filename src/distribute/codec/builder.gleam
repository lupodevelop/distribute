/// Helper builders for creating codecs for custom types.
///
/// This module provides convenience functions for building type-safe codecs
/// for your custom Gleam types without writing boilerplate.
import distribute/codec
import gleam/bit_array
import gleam/result

/// Build a codec for a custom type with 2 fields.
pub fn custom2(
  encoder1: codec.Encoder(a),
  encoder2: codec.Encoder(b),
  decoder1: codec.Decoder(a),
  decoder2: codec.Decoder(b),
  constructor: fn(a, b) -> c,
  destructor: fn(c) -> #(a, b),
) -> #(codec.Encoder(c), codec.Decoder(c)) {
  let encoder = fn(value: c) -> Result(BitArray, codec.EncodeError) {
    let #(field1, field2) = destructor(value)

    use enc1 <- result.try(codec.encode(encoder1, field1))
    use enc2 <- result.try(codec.encode(encoder2, field2))

    // Combine with length prefixes
    let len1 = bit_array.byte_size(enc1)
    let len2 = bit_array.byte_size(enc2)

    Ok(<<len1:32, enc1:bits, len2:32, enc2:bits>>)
  }

  let decoder = fn(binary: BitArray) -> Result(c, codec.DecodeError) {
    case binary {
      <<len1:32, rest:bits>> -> {
        case bit_array.slice(rest, 0, len1) {
          Ok(enc1) -> {
            case bit_array.slice(rest, len1, bit_array.byte_size(rest) - len1) {
              Ok(remaining) -> {
                case remaining {
                  <<len2:32, rest2:bits>> -> {
                    case bit_array.slice(rest2, 0, len2) {
                      Ok(enc2) -> {
                        use field1 <- result.try(codec.decode(decoder1, enc1))
                        use field2 <- result.try(codec.decode(decoder2, enc2))
                        Ok(constructor(field1, field2))
                      }
                      Error(_) ->
                        Error(codec.InsufficientData("Field 2 truncated"))
                    }
                  }
                  _ -> Error(codec.InvalidBinary("Missing field 2 length"))
                }
              }
              Error(_) -> Error(codec.InsufficientData("Field 1 truncated"))
            }
          }
          Error(_) -> Error(codec.InsufficientData("Field 1 truncated"))
        }
      }
      _ -> Error(codec.InvalidBinary("Missing field 1 length"))
    }
  }

  #(encoder, decoder)
}

/// Build a codec for a custom type with 3 fields.
pub fn custom3(
  encoder1: codec.Encoder(a),
  encoder2: codec.Encoder(b),
  encoder3: codec.Encoder(c),
  decoder1: codec.Decoder(a),
  decoder2: codec.Decoder(b),
  decoder3: codec.Decoder(c),
  constructor: fn(a, b, c) -> d,
  destructor: fn(d) -> #(a, b, c),
) -> #(codec.Encoder(d), codec.Decoder(d)) {
  let encoder = fn(value: d) -> Result(BitArray, codec.EncodeError) {
    let #(f1, f2, f3) = destructor(value)

    use enc1 <- result.try(codec.encode(encoder1, f1))
    use enc2 <- result.try(codec.encode(encoder2, f2))
    use enc3 <- result.try(codec.encode(encoder3, f3))

    let len1 = bit_array.byte_size(enc1)
    let len2 = bit_array.byte_size(enc2)
    let len3 = bit_array.byte_size(enc3)

    Ok(<<len1:32, enc1:bits, len2:32, enc2:bits, len3:32, enc3:bits>>)
  }

  let decoder = fn(binary: BitArray) -> Result(d, codec.DecodeError) {
    // Simplified decoder - in production would need proper slicing
    case binary {
      <<
        len1:32,
        f1_data:bytes-size(len1),
        len2:32,
        f2_data:bytes-size(len2),
        len3:32,
        f3_data:bytes-size(len3),
      >> -> {
        use field1 <- result.try(codec.decode(decoder1, f1_data))
        use field2 <- result.try(codec.decode(decoder2, f2_data))
        use field3 <- result.try(codec.decode(decoder3, f3_data))
        Ok(constructor(field1, field2, field3))
      }
      _ -> Error(codec.InvalidBinary("Invalid 3-field custom type format"))
    }
  }

  #(encoder, decoder)
}

/// Build a codec for an enum (variant type without payloads).
pub fn enum_codec(
  to_int: fn(a) -> Int,
  from_int: fn(Int) -> Result(a, Nil),
) -> #(codec.Encoder(a), codec.Decoder(a)) {
  let encoder = fn(value: a) -> Result(BitArray, codec.EncodeError) {
    let tag = to_int(value)
    codec.encode(codec.int_encoder(), tag)
  }

  let decoder = fn(binary: BitArray) -> Result(a, codec.DecodeError) {
    use tag <- result.try(codec.decode(codec.int_decoder(), binary))
    use variant <- result.try(
      from_int(tag)
      |> result.replace_error(codec.TypeMismatch("Unknown enum variant")),
    )
    Ok(variant)
  }

  #(encoder, decoder)
}
