/// Tagged message codec: embeds a tag string and version number in the
/// binary format. Decoders reject messages with mismatched tags or versions.
import distribute/codec
import gleam/bit_array
import gleam/result

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
pub fn encoder(
  payload_encoder: codec.Encoder(payload),
) -> codec.Encoder(TaggedMessage(payload)) {
  fn(msg: TaggedMessage(payload)) {
    let tag_bytes = bit_array.from_string(msg.tag)
    let tag_len = bit_array.byte_size(tag_bytes)
    use payload_bytes <- result.try(codec.encode(payload_encoder, msg.payload))
    Ok(<<tag_len:32, tag_bytes:bits, msg.version:32, payload_bytes:bits>>)
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
