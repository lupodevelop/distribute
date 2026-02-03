/// Behaviour / contract for pluggable crypto providers used during
/// handshake key-exchange and for optional payload encryption.
import gleam/option.{type Option, None}

// Opaque types exposed by the provider contract
pub type ProviderState {
  ProviderState
}

pub type SecureContext {
  SecureContext
}

// Public API the core expects from a provider implementation.
// Implementations should respect `settings.set_allow_atom_creation` and
// must not leak key material in logs.

/// Initialize the provider state.
///
/// Called once when the provider is started. Implementations must allocate
/// any required resources and prepare for key exchange operations.
///
/// ## Returns
///
/// Initial `ProviderState` for subsequent operations.
pub fn init() -> ProviderState {
  // Implementations must provide proper init.
  ProviderState
}

/// Start a key exchange with local parameters.
///
/// Initiates the key exchange protocol by generating the first message
/// to send to the peer.
///
/// ## Arguments
///
/// - `state` - Current provider state
/// - `local_params` - Local parameters for key exchange
///
/// ## Returns
///
/// Tuple of (outgoing_message, updated_state).
pub fn start_key_exchange(
  state: ProviderState,
  local_params: BitArray,
) -> #(BitArray, ProviderState) {
  // Return outgoing message to send to peer and updated state.
  #(local_params, state)
}

/// Handle an incoming key exchange message.
///
/// Processes a message from the peer during key exchange and returns
/// the next state of the protocol.
///
/// ## Arguments
///
/// - `state` - Current provider state
/// - `incoming` - Message received from peer
///
/// ## Returns
///
/// Tuple of (optional_response, updated_state, optional_secure_context).
/// When secure_context is Some, the handshake is complete.
pub fn handle_key_exchange(
  state: ProviderState,
  _incoming: BitArray,
) -> #(Option(BitArray), ProviderState, Option(SecureContext)) {
  // Return optional outgoing message, updated state and optional secure context
  #(None, state, None)
}

/// Encrypt plaintext using a secure context.
///
/// Encrypts the given data using the established secure context.
///
/// ## Arguments
///
/// - `context` - Secure context from completed handshake
/// - `plain` - Plaintext data to encrypt
///
/// ## Returns
///
/// - `Ok(ciphertext)` - Encrypted data
/// - `Error(reason)` - Encryption failed
pub fn encrypt(
  _context: SecureContext,
  _plain: BitArray,
) -> Result(BitArray, String) {
  Error("not implemented")
}

/// Decrypt ciphertext using a secure context.
///
/// Decrypts the given data using the established secure context.
///
/// ## Arguments
///
/// - `context` - Secure context from completed handshake
/// - `cipher` - Ciphertext data to decrypt
///
/// ## Returns
///
/// - `Ok(plaintext)` - Decrypted data
/// - `Error(reason)` - Decryption failed (e.g., tampered data)
pub fn decrypt(
  _context: SecureContext,
  _cipher: BitArray,
) -> Result(BitArray, String) {
  Error("not implemented")
}

/// Rotate the encryption key for a secure context.
///
/// Derives a new encryption key while maintaining the connection.
/// The old key is discarded.
///
/// ## Arguments
///
/// - `context` - Current secure context
///
/// ## Returns
///
/// - `Ok(new_context)` - Context with new key material
/// - `Error(reason)` - Rekey operation failed
pub fn rekey(_context: SecureContext) -> Result(SecureContext, String) {
  Error("not implemented")
}

/// Close the provider and release resources.
///
/// Called when the provider is shutting down. Implementations should
/// clean up any allocated resources and zero sensitive memory.
pub fn close(_state: ProviderState) -> Nil {
  Nil
}
