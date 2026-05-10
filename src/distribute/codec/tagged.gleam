/// Tagged message codec: embeds a tag string and version number in the
/// binary format. Decoders reject messages with mismatched tags or versions.
///
/// Wire format: `[tag_len:32][tag:utf8][version:32-unsigned][payload]`
///
/// The version field is an unsigned 32-bit integer (0–4 294 967 295).
import distribute/codec
import gleam/bit_array
import gleam/int
import gleam/result

const unsigned_32_max: Int = 4_294_967_295

/// A message annotated with a protocol tag and version.
pub opaque type TaggedMessage(payload) {
  TaggedMessage(tag: String, version: Int, payload: payload)
}

/// Create a tagged message.
pub fn new(
  tag: String,
  version: Int,
  payload: payload,
) -> TaggedMessage(payload) {
  TaggedMessage(tag:, version:, payload:)
}

/// Extract the payload.
pub fn payload(msg: TaggedMessage(payload)) -> payload {
  msg.payload
}

/// Get the tag.
pub fn tag(msg: TaggedMessage(payload)) -> String {
  msg.tag
}

/// Get the version.
pub fn version(msg: TaggedMessage(payload)) -> Int {
  msg.version
}

/// Encoder for tagged messages.
///
/// Format: `[tag_len:32][tag:utf8][version:32][payload]`
///
/// Rejects values that would silently wrap when written to the fixed-width
/// 32-bit fields:
/// - `version` must be in `[0, 2^32 - 1]`
/// - `tag` byte-length must be in `[0, 2^32 - 1]` (a 4 GiB tag is absurd
///   in practice but the bound is enforced for defence in depth)
pub fn encoder(
  payload_encoder: codec.Encoder(payload),
) -> codec.Encoder(TaggedMessage(payload)) {
  fn(msg: TaggedMessage(payload)) {
    case msg.version < 0 || msg.version > unsigned_32_max {
      True ->
        Error(codec.ValueTooLarge(
          "tagged version "
          <> int.to_string(msg.version)
          <> " out of unsigned 32-bit range",
        ))
      False -> {
        let tag_bytes = bit_array.from_string(msg.tag)
        let tag_len = bit_array.byte_size(tag_bytes)
        case tag_len > unsigned_32_max {
          True ->
            Error(codec.ValueTooLarge(
              "tagged tag length "
              <> int.to_string(tag_len)
              <> " exceeds unsigned 32-bit range",
            ))
          False -> {
            use payload_bytes <- result.try(codec.encode(
              payload_encoder,
              msg.payload,
            ))
            Ok(<<
              tag_len:32,
              tag_bytes:bits,
              msg.version:32,
              payload_bytes:bits,
            >>)
          }
        }
      }
    }
  }
}

/// Sized decoder for tagged messages with tag and version validation.
///
/// Returns `TagMismatch` or `VersionMismatch` errors for protocol mismatches.
/// Unlike `decoder`, returns remaining bytes for use in composite codecs.
pub fn sized_decoder(
  expected_tag: String,
  expected_version: Int,
  payload_sized_decoder: codec.SizedDecoder(payload),
) -> codec.SizedDecoder(TaggedMessage(payload)) {
  fn(binary: BitArray) {
    case binary {
      <<tag_len:32, rest:bits>> -> {
        let rest_bytes = bit_array.byte_size(rest)
        use tag_bytes <- result.try(
          bit_array.slice(rest, 0, tag_len)
          |> result.replace_error(codec.InsufficientData("tag truncated")),
        )
        use tag_string <- result.try(
          bit_array.to_string(tag_bytes)
          |> result.replace_error(codec.InvalidBinary("tag not valid UTF-8")),
        )
        case tag_string == expected_tag {
          False ->
            Error(codec.TagMismatch(expected: expected_tag, got: tag_string))
          True -> {
            use after_tag <- result.try(
              bit_array.slice(rest, tag_len, rest_bytes - tag_len)
              |> result.replace_error(codec.InsufficientData("missing version")),
            )
            case after_tag {
              <<ver:32, payload_rest:bits>> -> {
                case ver == expected_version {
                  False ->
                    Error(codec.VersionMismatch(
                      expected: expected_version,
                      got: ver,
                    ))
                  True -> {
                    use #(payload, remaining) <- result.try(
                      payload_sized_decoder(payload_rest),
                    )
                    Ok(#(
                      TaggedMessage(tag: tag_string, version: ver, payload:),
                      remaining,
                    ))
                  }
                }
              }
              _ -> Error(codec.InsufficientData("missing version or payload"))
            }
          }
        }
      }
      _ -> Error(codec.InvalidBinary("missing tag length"))
    }
  }
}

/// Decoder for tagged messages with tag and version validation.
///
/// Returns `TagMismatch` or `VersionMismatch` errors for protocol mismatches.
pub fn decoder(
  expected_tag: String,
  expected_version: Int,
  payload_decoder: codec.Decoder(payload),
) -> codec.Decoder(TaggedMessage(payload)) {
  fn(binary: BitArray) {
    case binary {
      <<tag_len:32, rest:bits>> -> {
        use tag_bytes <- result.try(
          bit_array.slice(rest, 0, tag_len)
          |> result.replace_error(codec.InsufficientData("tag truncated")),
        )
        use tag_string <- result.try(
          bit_array.to_string(tag_bytes)
          |> result.replace_error(codec.InvalidBinary("tag not valid UTF-8")),
        )
        case tag_string == expected_tag {
          False ->
            Error(codec.TagMismatch(expected: expected_tag, got: tag_string))
          True -> {
            use remaining <- result.try(
              bit_array.slice(
                rest,
                tag_len,
                bit_array.byte_size(rest) - tag_len,
              )
              |> result.replace_error(codec.InsufficientData("missing version")),
            )
            case remaining {
              <<ver:32, payload_bytes:bits>> -> {
                case ver == expected_version {
                  False ->
                    Error(codec.VersionMismatch(
                      expected: expected_version,
                      got: ver,
                    ))
                  True -> {
                    use payload <- result.try(codec.decode(
                      payload_decoder,
                      payload_bytes,
                    ))
                    Ok(TaggedMessage(tag: tag_string, version: ver, payload:))
                  }
                }
              }
              _ -> Error(codec.InsufficientData("missing version or payload"))
            }
          }
        }
      }
      _ -> Error(codec.InvalidBinary("missing tag length"))
    }
  }
}

/// Bundled `Codec` for tagged messages.
///
/// ```gleam
/// let tagged_int = tagged.codec("counter", 1, codec.int())
/// ```
pub fn codec(
  expected_tag: String,
  expected_version: Int,
  payload_codec: codec.Codec(payload),
) -> codec.Codec(TaggedMessage(payload)) {
  codec.Codec(
    encoder: encoder(payload_codec.encoder),
    decoder: decoder(expected_tag, expected_version, payload_codec.decoder),
    sized_decoder: sized_decoder(
      expected_tag,
      expected_version,
      payload_codec.sized_decoder,
    ),
  )
}

/// Convenience: encode a value as a tagged message in one step.
pub fn encode_tagged(
  tag: String,
  version: Int,
  payload_encoder: codec.Encoder(payload),
  value: payload,
) -> Result(BitArray, codec.EncodeError) {
  let msg = new(tag, version, value)
  codec.encode(encoder(payload_encoder), msg)
}

/// Convenience: decode binary as a tagged message and extract the payload.
pub fn decode_tagged(
  expected_tag: String,
  expected_version: Int,
  payload_decoder: codec.Decoder(payload),
  data: BitArray,
) -> Result(payload, codec.DecodeError) {
  use msg <- result.try(codec.decode(
    decoder(expected_tag, expected_version, payload_decoder),
    data,
  ))
  Ok(payload(msg))
}
