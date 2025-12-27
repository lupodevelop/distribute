import distribute/crypto/provider as crypto_provider
import distribute/codec
import gleam/bit_array
import gleam/option.{type Option}

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

pub fn peek_envelope_tag(data: BitArray) -> Result(#(String, Int), HandshakeError) {
  case codec.peek_envelope(data) {
    Ok(t) -> Ok(t)
    Error(_) -> Error(InvalidEnvelope("invalid envelope"))
  }
}

pub fn wrap_envelope(tag: String, version: Int, payload: BitArray) -> BitArray {
  codec.wrap_envelope(tag, version, payload)
}

pub fn unwrap_envelope(data: BitArray) -> Result(#(String, Int, BitArray), HandshakeError) {
  case codec.unwrap_envelope(data) {
    Ok(r) -> Ok(r)
    Error(_) -> Error(InvalidEnvelope("invalid envelope"))
  }
}
