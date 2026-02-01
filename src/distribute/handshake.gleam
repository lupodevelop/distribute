import distribute/codec
import distribute/crypto/provider as crypto_provider
import gleam/bit_array
import gleam/option.{type Option}
import gleam/result

/// Represents a capability advertised during handshake negotiation.
///
/// Contains protocol name, supported version range (min/max), and optional metadata.
pub type Capability {
  Capability(
    protocol: String,
    min: Int,
    max: Int,
    meta: List(#(String, String)),
  )
}

/// Represents the initial Hello message sent when establishing a connection.
///
/// Contains the sending node's identifier, metadata, and advertised capabilities.
pub type Hello {
  Hello(
    node_id: String,
    node_info: List(#(String, String)),
    capabilities: List(Capability),
  )
}

/// Message containing a list of capabilities for negotiation.
pub type CapabilitiesMsg {
  CapabilitiesMsg(capabilities: List(Capability))
}

/// Represents acceptance of a negotiated protocol with specific version and parameters.
pub type Accept {
  Accept(protocol: String, version: Int, params: List(#(String, String)))
}

/// Represents rejection of a handshake with a reason message.
pub type Reject {
  Reject(reason: String)
}

/// Message containing cryptographic key exchange payload.
pub type KeyExchangeMsg {
  KeyExchangeMsg(payload: BitArray)
}

/// A tagged envelope containing a versioned payload for wire transmission.
pub type Envelope {
  Envelope(tag: String, version: Int, payload: BitArray)
}

/// Metadata about a cluster member after successful handshake.
///
/// Contains negotiated protocol versions and optional secure context for encryption.
pub type MemberMetadata {
  MemberMetadata(
    negotiated_versions: List(#(String, Int)),
    secure_context: Option(crypto_provider.SecureContext),
  )
}

/// Errors that can occur during handshake operations.
pub type HandshakeError {
  InvalidEnvelope(String)
  UnsupportedProtocol(String)
  Timeout(String)
  CryptoError(String)
}

/// Peeks at the envelope tag and version without consuming the data.
///
/// Returns the tag string and version number if the envelope is valid.
pub fn peek_envelope_tag(
  data: BitArray,
) -> Result(#(String, Int), HandshakeError) {
  case codec.peek_envelope(data) {
    Ok(t) -> Ok(t)
    Error(_) -> Error(InvalidEnvelope("invalid envelope"))
  }
}

/// Wraps a payload in an envelope with the given tag and version.
///
/// Returns the complete envelope as a BitArray ready for transmission.
pub fn wrap_envelope(tag: String, version: Int, payload: BitArray) -> BitArray {
  codec.wrap_envelope(tag, version, payload)
}

/// Unwraps an envelope, extracting the tag, version, and payload.
///
/// Returns a tuple of (tag, version, payload) or an error if invalid.
pub fn unwrap_envelope(
  data: BitArray,
) -> Result(#(String, Int, BitArray), HandshakeError) {
  case codec.unwrap_envelope(data) {
    Ok(r) -> Ok(r)
    Error(_) -> Error(InvalidEnvelope("invalid envelope"))
  }
}

// ---------------------------------------------------------------------------
// Encoders / Decoders for handshake messages
// ---------------------------------------------------------------------------

// Pair encoder/decoder for `#(String, String)` used in metadata lists.
fn pair_encoder() -> codec.Encoder(#(String, String)) {
  fn(pair: #(String, String)) {
    let #(a, b) = pair
    use a_bytes <- result.try(codec.encode(codec.string_encoder(), a))
    use b_bytes <- result.try(codec.encode(codec.string_encoder(), b))
    Ok(bit_array.append(a_bytes, b_bytes))
  }
}

fn pair_sized_decoder() -> codec.SizedDecoder(#(String, String)) {
  fn(data) {
    use #(a, rest) <- result.try(codec.string_sized_decoder()(data))
    use #(b, rest2) <- result.try(codec.string_sized_decoder()(rest))
    Ok(#(#(a, b), rest2))
  }
}

/// Creates an encoder for serializing Capability values to binary format.
pub fn capability_encoder() -> codec.Encoder(Capability) {
  fn(c: Capability) {
    let meta_enc = codec.list_encoder(pair_encoder())
    use proto <- result.try(codec.encode(codec.string_encoder(), c.protocol))
    use min_b <- result.try(codec.encode(codec.int_encoder(), c.min))
    use max_b <- result.try(codec.encode(codec.int_encoder(), c.max))
    use meta_b <- result.try(codec.encode(meta_enc, c.meta))
    Ok(bit_array.append(
      proto,
      bit_array.append(min_b, bit_array.append(max_b, meta_b)),
    ))
  }
}

/// Creates a sized decoder for deserializing Capability values from binary format.
pub fn capability_sized_decoder() -> codec.SizedDecoder(Capability) {
  fn(data: BitArray) {
    use #(protocol, rest1) <- result.try(codec.string_sized_decoder()(data))
    use #(min, rest2) <- result.try(codec.int_sized_decoder()(rest1))
    use #(max, rest3) <- result.try(codec.int_sized_decoder()(rest2))
    use #(meta, rest4) <- result.try(
      codec.list_sized_decoder(pair_sized_decoder())(rest3),
    )
    Ok(#(Capability(protocol, min, max, meta), rest4))
  }
}

/// Creates an encoder for serializing Hello messages to binary format.
pub fn hello_encoder() -> codec.Encoder(Hello) {
  fn(h: Hello) {
    let info_enc = codec.list_encoder(pair_encoder())
    let cap_enc = codec.list_encoder(capability_encoder())
    use id_b <- result.try(codec.encode(codec.string_encoder(), h.node_id))
    use info_b <- result.try(codec.encode(info_enc, h.node_info))
    use caps_b <- result.try(codec.encode(cap_enc, h.capabilities))
    Ok(bit_array.append(id_b, bit_array.append(info_b, caps_b)))
  }
}

/// Creates a sized decoder for deserializing Hello messages from binary format.
pub fn hello_sized_decoder() -> codec.SizedDecoder(Hello) {
  fn(data: BitArray) {
    use #(node_id, rest1) <- result.try(codec.string_sized_decoder()(data))
    use #(node_info, rest2) <- result.try(
      codec.list_sized_decoder(pair_sized_decoder())(rest1),
    )
    use #(capabilities, rest3) <- result.try(
      codec.list_sized_decoder(capability_sized_decoder())(rest2),
    )
    Ok(#(Hello(node_id, node_info, capabilities), rest3))
  }
}

/// Creates an encoder for serializing CapabilitiesMsg to binary format.
pub fn capabilities_encoder() -> codec.Encoder(CapabilitiesMsg) {
  fn(m: CapabilitiesMsg) {
    let enc = codec.list_encoder(capability_encoder())
    codec.encode(enc, m.capabilities)
  }
}

/// Creates a sized decoder for deserializing CapabilitiesMsg from binary format.
pub fn capabilities_sized_decoder() -> codec.SizedDecoder(CapabilitiesMsg) {
  fn(data: BitArray) {
    use #(caps, rest) <- result.try(
      codec.list_sized_decoder(capability_sized_decoder())(data),
    )
    Ok(#(CapabilitiesMsg(caps), rest))
  }
}

/// Creates an encoder for serializing Accept messages to binary format.
pub fn accept_encoder() -> codec.Encoder(Accept) {
  fn(a: Accept) {
    let params_enc = codec.list_encoder(pair_encoder())
    use p <- result.try(codec.encode(codec.string_encoder(), a.protocol))
    use vb <- result.try(codec.encode(codec.int_encoder(), a.version))
    use pb <- result.try(codec.encode(params_enc, a.params))
    Ok(bit_array.append(p, bit_array.append(vb, pb)))
  }
}

/// Creates a sized decoder for deserializing Accept messages from binary format.
pub fn accept_sized_decoder() -> codec.SizedDecoder(Accept) {
  fn(data: BitArray) {
    use #(protocol, rest1) <- result.try(codec.string_sized_decoder()(data))
    use #(version, rest2) <- result.try(codec.int_sized_decoder()(rest1))
    use #(params, rest3) <- result.try(
      codec.list_sized_decoder(pair_sized_decoder())(rest2),
    )
    Ok(#(Accept(protocol, version, params), rest3))
  }
}

/// Creates an encoder for serializing Reject messages to binary format.
pub fn reject_encoder() -> codec.Encoder(Reject) {
  fn(r: Reject) { codec.encode(codec.string_encoder(), r.reason) }
}

/// Creates a sized decoder for deserializing Reject messages from binary format.
pub fn reject_sized_decoder() -> codec.SizedDecoder(Reject) {
  fn(data: BitArray) {
    use #(reason, rest) <- result.try(codec.string_sized_decoder()(data))
    Ok(#(Reject(reason), rest))
  }
}

/// Creates an encoder for serializing KeyExchangeMsg to binary format.
pub fn keyexchange_encoder() -> codec.Encoder(KeyExchangeMsg) {
  fn(k: KeyExchangeMsg) { codec.encode(codec.bitarray_encoder(), k.payload) }
}

/// Creates a sized decoder for deserializing KeyExchangeMsg from binary format.
pub fn keyexchange_sized_decoder() -> codec.SizedDecoder(KeyExchangeMsg) {
  fn(data: BitArray) {
    use #(payload, rest) <- result.try(codec.bitarray_sized_decoder()(data))
    Ok(#(KeyExchangeMsg(payload), rest))
  }
}

/// Returns the codec schema for Hello messages with envelope support.
pub fn hello_schema() -> codec.Schema(Hello) {
  codec.new_schema(
    tag: "distribute:hello",
    version: 1,
    encoder: hello_encoder(),
    decoder: codec.to_decoder(hello_sized_decoder()),
  )
}

/// Returns the codec schema for CapabilitiesMsg with envelope support.
pub fn capabilities_schema() -> codec.Schema(CapabilitiesMsg) {
  codec.new_schema(
    tag: "distribute:capabilities",
    version: 1,
    encoder: capabilities_encoder(),
    decoder: codec.to_decoder(capabilities_sized_decoder()),
  )
}

/// Returns the codec schema for Accept messages with envelope support.
pub fn accept_schema() -> codec.Schema(Accept) {
  codec.new_schema(
    tag: "distribute:accept",
    version: 1,
    encoder: accept_encoder(),
    decoder: codec.to_decoder(accept_sized_decoder()),
  )
}

/// Returns the codec schema for Reject messages with envelope support.
pub fn reject_schema() -> codec.Schema(Reject) {
  codec.new_schema(
    tag: "distribute:reject",
    version: 1,
    encoder: reject_encoder(),
    decoder: codec.to_decoder(reject_sized_decoder()),
  )
}

/// Returns the codec schema for KeyExchangeMsg with envelope support.
pub fn keyexchange_schema() -> codec.Schema(KeyExchangeMsg) {
  codec.new_schema(
    tag: "distribute:keyexchange",
    version: 1,
    encoder: keyexchange_encoder(),
    decoder: codec.to_decoder(keyexchange_sized_decoder()),
  )
}

/// Encodes a Hello message to binary format with envelope wrapper.
pub fn encode_hello(h: Hello) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(hello_schema(), h)
}

/// Decodes a Hello message from binary format, unwrapping the envelope.
pub fn decode_hello(data: BitArray) -> Result(Hello, codec.DecodeError) {
  codec.schema_decode(hello_schema(), data)
}

/// Encodes a CapabilitiesMsg to binary format with envelope wrapper.
pub fn encode_capabilities(
  m: CapabilitiesMsg,
) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(capabilities_schema(), m)
}

/// Decodes a CapabilitiesMsg from binary format, unwrapping the envelope.
pub fn decode_capabilities(
  data: BitArray,
) -> Result(CapabilitiesMsg, codec.DecodeError) {
  codec.schema_decode(capabilities_schema(), data)
}

/// Encodes an Accept message to binary format with envelope wrapper.
pub fn encode_accept(a: Accept) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(accept_schema(), a)
}

/// Decodes an Accept message from binary format, unwrapping the envelope.
pub fn decode_accept(data: BitArray) -> Result(Accept, codec.DecodeError) {
  codec.schema_decode(accept_schema(), data)
}

/// Encodes a Reject message to binary format with envelope wrapper.
pub fn encode_reject(r: Reject) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(reject_schema(), r)
}

/// Decodes a Reject message from binary format, unwrapping the envelope.
pub fn decode_reject(data: BitArray) -> Result(Reject, codec.DecodeError) {
  codec.schema_decode(reject_schema(), data)
}

/// Encodes a KeyExchangeMsg to binary format with envelope wrapper.
pub fn encode_keyexchange(
  k: KeyExchangeMsg,
) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(keyexchange_schema(), k)
}

/// Decodes a KeyExchangeMsg from binary format, unwrapping the envelope.
pub fn decode_keyexchange(
  data: BitArray,
) -> Result(KeyExchangeMsg, codec.DecodeError) {
  codec.schema_decode(keyexchange_schema(), data)
}
