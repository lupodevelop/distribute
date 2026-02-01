/// Minimal, insecure crypto provider stub for development and testing.
///
/// This provider **does not** perform real cryptography. It implements the
/// same contract expected by `distribute/crypto/provider` and returns
/// identity/passthrough behaviours.
///
/// ## ⚠️ WARNING: INSECURE
///
/// This stub is **not suitable for production use**. All cryptographic
/// operations are no-ops:
///
/// - `encrypt` returns plaintext unchanged
/// - `decrypt` returns ciphertext unchanged
/// - Key exchange produces a dummy context immediately
///
/// ## Intended Use Cases
///
/// - **Unit testing**: Testing handshake state machine without crypto overhead
/// - **Local development**: Quick iteration without crypto setup
/// - **Examples**: Demonstrating API usage without crypto complexity
///
/// ## Production Alternative
///
/// For production use, use `otp_crypto_adapter` which provides:
/// - Real X25519 key exchange (ECDH)
/// - ChaCha20-Poly1305 AEAD encryption
/// - HKDF-SHA256 key derivation
///
/// See `distribute/crypto/otp_crypto_adapter` for the production implementation.
///
import distribute/crypto/provider
import gleam/option.{type Option, None, Some}

/// Initialize the stub provider state.
///
/// Returns a default `ProviderState` that can be used for testing.
/// No actual initialization is performed.
pub fn init() -> provider.ProviderState {
  provider.ProviderState
}

/// Start a key exchange (no-op stub).
///
/// In a real provider, this would generate ephemeral keys and produce
/// the first key exchange message. The stub simply echoes the local
/// parameters back as the "outgoing message".
///
/// ## Parameters
///
/// - `state`: The current provider state
/// - `local_params`: Parameters from the local node (echoed back)
///
/// ## Returns
///
/// A tuple of `#(outgoing_message, new_state)` where the outgoing
/// message is just the echoed `local_params`.
pub fn start_key_exchange(
  state: provider.ProviderState,
  local_params: BitArray,
) -> #(BitArray, provider.ProviderState) {
  // Passthrough: echo local params as outgoing message
  #(local_params, state)
}

/// Handle incoming key exchange message (no-op stub).
///
/// In a real provider, this would process the remote's key exchange
/// message and potentially produce a response and/or derive a shared
/// secret. The stub immediately produces a dummy `SecureContext`.
///
/// ## Parameters
///
/// - `state`: The current provider state
/// - `_incoming`: The incoming key exchange message (ignored)
///
/// ## Returns
///
/// A tuple of `#(optional_response, new_state, optional_context)`:
/// - Response is `None` (no further messages needed)
/// - Context is `Some(SecureContext)` (exchange "complete" immediately)
pub fn handle_key_exchange(
  state: provider.ProviderState,
  _incoming: BitArray,
) -> #(Option(BitArray), provider.ProviderState, Option(provider.SecureContext)) {
  // Immediately produce a SecureContext (opaque) and no further outgoing
  #(None, state, Some(provider.SecureContext))
}

/// Encrypt plaintext (identity stub - returns unchanged).
///
/// ## ⚠️ WARNING: This does NOT encrypt anything!
///
/// In a real provider, this would use AEAD (e.g., ChaCha20-Poly1305)
/// to encrypt and authenticate the plaintext. The stub returns the
/// plaintext unchanged.
///
/// ## Parameters
///
/// - `_context`: The secure context (ignored)
/// - `plain`: The plaintext to "encrypt"
///
/// ## Returns
///
/// `Ok(plain)` - the unchanged plaintext
pub fn encrypt(
  _context: provider.SecureContext,
  plain: BitArray,
) -> Result(BitArray, String) {
  // Identity — insecure
  Ok(plain)
}

/// Decrypt ciphertext (identity stub - returns unchanged).
///
/// ## ⚠️ WARNING: This does NOT decrypt anything!
///
/// In a real provider, this would use AEAD to decrypt and verify
/// the ciphertext. The stub returns the ciphertext unchanged.
///
/// ## Parameters
///
/// - `_context`: The secure context (ignored)
/// - `cipher`: The "ciphertext" to decrypt
///
/// ## Returns
///
/// `Ok(cipher)` - the unchanged ciphertext
pub fn decrypt(
  _context: provider.SecureContext,
  cipher: BitArray,
) -> Result(BitArray, String) {
  // Identity — insecure
  Ok(cipher)
}

/// Rekey the secure context (no-op stub).
///
/// In a real provider, this would derive new encryption keys from
/// the existing context to provide forward secrecy. The stub returns
/// the context unchanged.
///
/// ## Parameters
///
/// - `context`: The current secure context
///
/// ## Returns
///
/// `Ok(context)` - the unchanged context
pub fn rekey(
  context: provider.SecureContext,
) -> Result(provider.SecureContext, String) {
  Ok(context)
}

/// Close the provider and clean up resources (no-op stub).
///
/// In a real provider, this would securely erase keys and release
/// any resources. The stub does nothing.
pub fn close(_state: provider.ProviderState) -> Nil {
  Nil
}
