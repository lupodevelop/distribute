/// Secure envelope handling for handshake messages (STUB for v2.2.0+)
///
/// This module provides encrypted/authenticated envelope wrapping for
/// handshake protocol messages. Unlike the generic `codec.wrap_envelope`
/// (which provides plain-text envelopes), this module integrates with
/// the crypto provider to encrypt/decrypt and authenticate envelopes
/// during key exchange.
///
/// ## Status: STUB - Planned for v2.2.0+
///
/// Current implementation returns placeholders. Full implementation
/// will provide:
///
/// - **Secure envelope wrapping**: Automatic encryption of payload using
///   `crypto_provider.encrypt()` when secure_context is available
/// - **Authenticated envelopes**: MAC/signature for Hello, Capabilities,
///   Accept/Reject messages to prevent tampering
/// - **Transparent decryption**: `unwrap()` automatically decrypts using
///   the node's secure_context
/// - **Fallback to plain**: Graceful degradation to plain-text envelopes
///   when crypto is not negotiated yet
///
/// ## Roadmap
///
/// - **v2.1.0**: Plain-text envelopes via `codec.wrap_envelope` (done)
/// - **v2.2.0**: This module fully implemented with crypto integration,
///   AEAD for authenticated encryption, and automatic secure_context lookup
///
pub type Envelope {
  Envelope(tag: String, version: Int, payload: BitArray)
}

pub fn peek_tag(_binary: BitArray) -> Result(#(String, Int), String) {
  // Stub: will parse envelope and return tag/version without decrypting payload
  Ok(#("unknown", 0))
}

pub fn wrap(_tag: String, _version: Int, payload: BitArray) -> BitArray {
  // Stub: will wrap with encryption/authentication when secure_context available
  payload
}

pub fn unwrap(binary: BitArray) -> Result(#(String, Int, BitArray), String) {
  // Stub: will unwrap and decrypt envelope using secure_context
  Ok(#("unknown", 0, binary))
}
