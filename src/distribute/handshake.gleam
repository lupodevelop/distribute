import distribute/codec
import distribute/crypto/provider as crypto_provider
import gleam/bit_array
import gleam/option.{type Option}
import gleam/result

pub type Capability {
  Capability(
    protocol: String,
    min: Int,
    max: Int,
    meta: List(#(String, String)),
  )
}

pub type Hello {
  Hello(
    node_id: String,
    node_info: List(#(String, String)),
    capabilities: List(Capability),
  )
}

pub type CapabilitiesMsg {
  CapabilitiesMsg(capabilities: List(Capability))
}

pub type Accept {
  Accept(protocol: String, version: Int, params: List(#(String, String)))
}

pub type Reject {
  Reject(reason: String)
}

pub type KeyExchangeMsg {
  KeyExchangeMsg(payload: BitArray)
}

pub type Envelope {
  Envelope(tag: String, version: Int, payload: BitArray)
}

pub type MemberMetadata {
  MemberMetadata(
    negotiated_versions: List(#(String, Int)),
    secure_context: Option(crypto_provider.SecureContext),
  )
}

// Errors local to handshake helpers
pub type HandshakeError {
  InvalidEnvelope(String)
  UnsupportedProtocol(String)
  Timeout(String)
  CryptoError(String)
}

// Placeholder helpers (signatures only). Implementations will be added
// in follow-up commits. Keeping them here documents the intended API.

pub fn peek_envelope_tag(
  data: BitArray,
) -> Result(#(String, Int), HandshakeError) {
  case codec.peek_envelope(data) {
    Ok(t) -> Ok(t)
    Error(_) -> Error(InvalidEnvelope("invalid envelope"))
  }
}

pub fn wrap_envelope(tag: String, version: Int, payload: BitArray) -> BitArray {
  codec.wrap_envelope(tag, version, payload)
}

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

// Capability encoder/decoder
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

// Hello encoder/decoder
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

// CapabilitiesMsg encoder/decoder (wraps a list of Capability)
pub fn capabilities_encoder() -> codec.Encoder(CapabilitiesMsg) {
  fn(m: CapabilitiesMsg) {
    let enc = codec.list_encoder(capability_encoder())
    codec.encode(enc, m.capabilities)
  }
}

pub fn capabilities_sized_decoder() -> codec.SizedDecoder(CapabilitiesMsg) {
  fn(data: BitArray) {
    use #(caps, rest) <- result.try(
      codec.list_sized_decoder(capability_sized_decoder())(data),
    )
    Ok(#(CapabilitiesMsg(caps), rest))
  }
}

// Accept / Reject / KeyExchange
pub fn accept_encoder() -> codec.Encoder(Accept) {
  fn(a: Accept) {
    let params_enc = codec.list_encoder(pair_encoder())
    use p <- result.try(codec.encode(codec.string_encoder(), a.protocol))
    use vb <- result.try(codec.encode(codec.int_encoder(), a.version))
    use pb <- result.try(codec.encode(params_enc, a.params))
    Ok(bit_array.append(p, bit_array.append(vb, pb)))
  }
}

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

pub fn reject_encoder() -> codec.Encoder(Reject) {
  fn(r: Reject) { codec.encode(codec.string_encoder(), r.reason) }
}

pub fn reject_sized_decoder() -> codec.SizedDecoder(Reject) {
  fn(data: BitArray) {
    use #(reason, rest) <- result.try(codec.string_sized_decoder()(data))
    Ok(#(Reject(reason), rest))
  }
}

pub fn keyexchange_encoder() -> codec.Encoder(KeyExchangeMsg) {
  fn(k: KeyExchangeMsg) { codec.encode(codec.bitarray_encoder(), k.payload) }
}

pub fn keyexchange_sized_decoder() -> codec.SizedDecoder(KeyExchangeMsg) {
  fn(data: BitArray) {
    use #(payload, rest) <- result.try(codec.bitarray_sized_decoder()(data))
    Ok(#(KeyExchangeMsg(payload), rest))
  }
}

// Public schemas for convenient encode/decode with envelope
pub fn hello_schema() -> codec.Schema(Hello) {
  codec.new_schema(
    tag: "distribute:hello",
    version: 1,
    encoder: hello_encoder(),
    decoder: codec.to_decoder(hello_sized_decoder()),
  )
}

pub fn capabilities_schema() -> codec.Schema(CapabilitiesMsg) {
  codec.new_schema(
    tag: "distribute:capabilities",
    version: 1,
    encoder: capabilities_encoder(),
    decoder: codec.to_decoder(capabilities_sized_decoder()),
  )
}

pub fn accept_schema() -> codec.Schema(Accept) {
  codec.new_schema(
    tag: "distribute:accept",
    version: 1,
    encoder: accept_encoder(),
    decoder: codec.to_decoder(accept_sized_decoder()),
  )
}

pub fn reject_schema() -> codec.Schema(Reject) {
  codec.new_schema(
    tag: "distribute:reject",
    version: 1,
    encoder: reject_encoder(),
    decoder: codec.to_decoder(reject_sized_decoder()),
  )
}

pub fn keyexchange_schema() -> codec.Schema(KeyExchangeMsg) {
  codec.new_schema(
    tag: "distribute:keyexchange",
    version: 1,
    encoder: keyexchange_encoder(),
    decoder: codec.to_decoder(keyexchange_sized_decoder()),
  )
}

// Convenience wrappers
pub fn encode_hello(h: Hello) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(hello_schema(), h)
}

pub fn decode_hello(data: BitArray) -> Result(Hello, codec.DecodeError) {
  codec.schema_decode(hello_schema(), data)
}

pub fn encode_capabilities(
  m: CapabilitiesMsg,
) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(capabilities_schema(), m)
}

pub fn decode_capabilities(
  data: BitArray,
) -> Result(CapabilitiesMsg, codec.DecodeError) {
  codec.schema_decode(capabilities_schema(), data)
}

pub fn encode_accept(a: Accept) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(accept_schema(), a)
}

pub fn decode_accept(data: BitArray) -> Result(Accept, codec.DecodeError) {
  codec.schema_decode(accept_schema(), data)
}

pub fn encode_reject(r: Reject) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(reject_schema(), r)
}

pub fn decode_reject(data: BitArray) -> Result(Reject, codec.DecodeError) {
  codec.schema_decode(reject_schema(), data)
}

pub fn encode_keyexchange(
  k: KeyExchangeMsg,
) -> Result(BitArray, codec.EncodeError) {
  codec.schema_encode(keyexchange_schema(), k)
}

pub fn decode_keyexchange(
  data: BitArray,
) -> Result(KeyExchangeMsg, codec.DecodeError) {
  codec.schema_decode(keyexchange_schema(), data)
}
