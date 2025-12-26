/// Type-level validation for messages with tags and versions.
///
/// This module provides compile-time tag and version validation
/// to prevent protocol mismatches between nodes.
import distribute/codec
import gleam/bit_array
import gleam/int
import gleam/result

/// A message with tag and version information for protocol safety.
pub opaque type TaggedMessage(payload) {
  TaggedMessage(tag: String, version: Int, payload: payload)
}

/// Create a tagged message.
pub fn new(
  tag: String,
  version: Int,
  payload: payload,
) -> TaggedMessage(payload) {
  TaggedMessage(tag: tag, version: version, payload: payload)
}

/// Extract the payload from a tagged message.
pub fn payload(msg: TaggedMessage(payload)) -> payload {
  msg.payload
}

/// Get the tag from a tagged message.
pub fn tag(msg: TaggedMessage(payload)) -> String {
  msg.tag
}

/// Get the version from a tagged message.
pub fn version(msg: TaggedMessage(payload)) -> Int {
  msg.version
}

/// Create an encoder for tagged messages.
/// This embeds the tag and version in the binary format.
pub fn encoder(
  payload_encoder: codec.Encoder(payload),
) -> codec.Encoder(TaggedMessage(payload)) {
  fn(msg: TaggedMessage(payload)) -> Result(BitArray, codec.EncodeError) {
    // Encode tag
    let tag_bytes = bit_array.from_string(msg.tag)
    let tag_len = bit_array.byte_size(tag_bytes)

    // Encode payload
    use payload_bytes <- result.try(codec.encode(payload_encoder, msg.payload))

    // Format: [tag_len:32][tag][version:32][payload]
    Ok(<<tag_len:32, tag_bytes:bits, msg.version:32, payload_bytes:bits>>)
  }
}

/// Create a decoder for tagged messages with validation.
/// This ensures received messages match the expected tag and version.
pub fn decoder(
  expected_tag: String,
  expected_version: Int,
  payload_decoder: codec.Decoder(payload),
) -> codec.Decoder(TaggedMessage(payload)) {
  fn(binary: BitArray) -> Result(TaggedMessage(payload), codec.DecodeError) {
    case binary {
      <<tag_len:32, rest:bits>> -> {
        case bit_array.slice(rest, 0, tag_len) {
          Ok(tag_bytes) -> {
            let tag = bit_array.to_string(tag_bytes)
            case tag {
              Ok(tag_string) -> {
                // Validate tag
                case tag_string == expected_tag {
                  False ->
                    Error(codec.TypeMismatch(
                      "Tag mismatch: expected "
                      <> expected_tag
                      <> ", got "
                      <> tag_string,
                    ))
                  True -> {
                    let remaining =
                      bit_array.slice(
                        rest,
                        tag_len,
                        bit_array.byte_size(rest) - tag_len,
                      )
                    case remaining {
                      Ok(<<version:32, payload_bytes:bits>>) -> {
                        // Validate version
                        case version == expected_version {
                          False ->
                            Error(codec.TypeMismatch(
                              "Version mismatch: expected "
                              <> int.to_string(expected_version)
                              <> ", got "
                              <> int.to_string(version),
                            ))
                          True -> {
                            // Decode payload
                            use payload <- result.try(codec.decode(
                              payload_decoder,
                              payload_bytes,
                            ))
                            Ok(TaggedMessage(
                              tag: tag_string,
                              version: version,
                              payload: payload,
                            ))
                          }
                        }
                      }
                      _ ->
                        Error(codec.InsufficientData(
                          "Missing version or payload",
                        ))
                    }
                  }
                }
              }
              Error(_) -> Error(codec.InvalidBinary("Tag is not valid UTF-8"))
            }
          }
          Error(_) -> Error(codec.InsufficientData("Tag truncated"))
        }
      }
      _ -> Error(codec.InvalidBinary("Missing tag length"))
    }
  }
}
