/// Handshake-specific crypto extensions (STUB for v2.2.0+)
///
/// This module provides handshake-level crypto provider management and
/// secure context retrieval. It extends the core `distribute/crypto/provider`
/// behaviour with handshake-specific functionality.
///
/// ## Status: STUB - Planned for v2.2.0+
///
/// The current implementation provides placeholder types and functions.
/// Full implementation is planned for release 2.2.0 and will include:
///
/// - Global crypto provider registry for handshake sessions
/// - Per-node secure context management and caching
/// - Integration with `distribute/crypto/provider` for key exchange
/// - Automatic encryption/decryption of handshake envelopes
/// - Provider hot-swapping and configuration
///
/// ## Roadmap
///
/// - **v2.1.0**: Core `crypto/provider` behaviour defined (already implemented)
/// - **v2.2.0**: This module fully implemented with AEAD, ECDH key-exchange,
///   rekeying strategies, and conformance test-suite
///
/// See `distribute/crypto/provider` for the current provider behaviour.
///
import gleam/option.{type Option, None}

/// Represents a registered crypto provider for handshake operations.
///
/// Used to identify and configure crypto providers that can be used
/// during handshake key exchange and secure communication.
///
/// ## Fields
///
/// - `name`: Unique identifier for the provider (e.g., "otp_crypto", "libsodium")
///
/// ## Status: STUB
///
/// Will be extended in v2.2.0+ to include provider capabilities,
/// configuration, and priority settings.
pub type Provider {
  Provider(name: String)
}

/// Represents an established secure context with a remote node.
///
/// Contains the cryptographic state needed for secure communication,
/// including session keys and cipher state derived from the handshake.
///
/// ## Fields
///
/// - `info`: Debug information about the secure context
///
/// ## Status: STUB
///
/// Will be extended in v2.2.0+ to include:
/// - Symmetric keys for encryption/decryption
/// - Nonce counters for replay protection
/// - Cipher suite information
/// - Key derivation parameters
pub type SecureContext {
  SecureContext(info: String)
}

/// Register a crypto provider for handshake sessions.
///
/// Providers are used during handshake key exchange to establish
/// secure contexts with remote nodes. Multiple providers can be
/// registered; the handshake will negotiate which to use based
/// on both nodes' capabilities.
///
/// ## Parameters
///
/// - `p`: The provider to register
///
/// ## Returns
///
/// `Ok(Nil)` on success, `Error(reason)` if registration fails.
///
/// ## Status: STUB
///
/// Currently always returns `Ok(Nil)`. Will implement actual
/// registration in v2.2.0+.
pub fn register_provider(_p: Provider) -> Result(Nil, String) {
  // Stub: register a crypto provider for handshake sessions
  Ok(Nil)
}

/// Retrieve the secure context for a remote node.
///
/// Returns the cached secure context if a successful handshake
/// has been completed with the specified node, or `None` if
/// no secure context exists.
///
/// ## Parameters
///
/// - `node`: The node identifier (e.g., "node@host:9000")
///
/// ## Returns
///
/// `Some(context)` if a secure context exists, `None` otherwise.
///
/// ## Status: STUB
///
/// Currently always returns `None`. Will implement context
/// lookup and caching in v2.2.0+.
pub fn get_secure_context(_node: String) -> Option(SecureContext) {
  // Stub: retrieve cached secure context for a node
  None
}
