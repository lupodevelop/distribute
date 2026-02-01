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
/// Represents an encrypted/authenticated envelope for handshake messages.
///
/// Contains a tag identifying the message type, a protocol version, and
/// the encrypted or plain-text payload.
///
/// ## Fields
///
/// - `tag`: Message type identifier (e.g., "hello", "accept", "reject")
/// - `version`: Protocol version number for compatibility checking
/// - `payload`: The actual message data, encrypted when secure_context is available
pub type Envelope {
  Envelope(tag: String, version: Int, payload: BitArray)
}

/// Peek at the tag and version of an envelope without decrypting.
///
/// Useful for routing messages to the appropriate handler before
/// full decryption. Returns `Ok(#(tag, version))` on success.
///
/// ## Example
///
/// ```gleam
/// case peek_tag(envelope_binary) {
///   Ok(#("hello", 1)) -> handle_hello(envelope_binary)
///   Ok(#("accept", _)) -> handle_accept(envelope_binary)
///   Error(reason) -> log_error(reason)
/// }
/// ```
///
/// ## Status: STUB
///
/// Currently returns `Ok(#("unknown", 0))`. Will parse envelope
/// header in v2.2.0+.
pub fn peek_tag(_binary: BitArray) -> Result(#(String, Int), String) {
  // Stub: will parse envelope and return tag/version without decrypting payload
  Ok(#("unknown", 0))
}

/// Wrap a payload into an authenticated/encrypted envelope.
///
/// When a secure_context is available, the payload will be encrypted
/// using AEAD. Otherwise, falls back to plain-text wrapping.
///
/// ## Parameters
///
/// - `tag`: Message type identifier
/// - `version`: Protocol version
/// - `payload`: The message data to wrap
///
/// ## Returns
///
/// The envelope as a `BitArray` ready for transmission.
///
/// ## Status: STUB
///
/// Currently returns payload unchanged. Will implement encryption in v2.2.0+.
pub fn wrap(_tag: String, _version: Int, payload: BitArray) -> BitArray {
  // Stub: will wrap with encryption/authentication when secure_context available
  payload
}

/// Unwrap and decrypt an envelope.
///
/// Parses the envelope structure and decrypts the payload using
/// the node's secure_context if encryption was applied.
///
/// ## Returns
///
/// `Ok(#(tag, version, payload))` on success, or `Error(reason)` if
/// parsing or decryption fails.
///
/// ## Status: STUB
///
/// Currently returns `Ok(#("unknown", 0, binary))`. Will implement
/// full parsing and decryption in v2.2.0+.
pub fn unwrap(binary: BitArray) -> Result(#(String, Int, BitArray), String) {
  // Stub: will unwrap and decrypt envelope using secure_context
  Ok(#("unknown", 0, binary))
}
