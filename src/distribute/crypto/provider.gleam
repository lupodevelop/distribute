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

pub fn init() -> ProviderState {
  // Implementations must provide proper init.
  ProviderState
}

pub fn start_key_exchange(
  state: ProviderState,
  local_params: BitArray,
) -> #(BitArray, ProviderState) {
  // Return outgoing message to send to peer and updated state.
  #(local_params, state)
}

pub fn handle_key_exchange(
  state: ProviderState,
  _incoming: BitArray,
) -> #(Option(BitArray), ProviderState, Option(SecureContext)) {
  // Return optional outgoing message, updated state and optional secure context
  #(None, state, None)
}

pub fn encrypt(
  _context: SecureContext,
  _plain: BitArray,
) -> Result(BitArray, String) {
  Error("not implemented")
}

pub fn decrypt(
  _context: SecureContext,
  _cipher: BitArray,
) -> Result(BitArray, String) {
  Error("not implemented")
}

pub fn rekey(_context: SecureContext) -> Result(SecureContext, String) {
  Error("not implemented")
}

pub fn close(_state: ProviderState) -> Nil {
  Nil
}
