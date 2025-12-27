/// Minimal, insecure crypto provider stub for development and testing.
/// This provider _does not_ perform real cryptography. It implements the
/// same contract expected by `distribute/crypto/provider` and returns
/// identity/passthrough behaviours. Documented as insecure and only for
/// examples / local development.
import distribute/crypto/provider
import gleam/option.{type Option, None, Some}

pub fn init() -> provider.ProviderState {
  provider.ProviderState
}

pub fn start_key_exchange(
  state: provider.ProviderState,
  local_params: BitArray,
) -> #(BitArray, provider.ProviderState) {
  // Passthrough: echo local params as outgoing message
  #(local_params, state)
}

pub fn handle_key_exchange(
  state: provider.ProviderState,
  _incoming: BitArray,
) -> #(Option(BitArray), provider.ProviderState, Option(provider.SecureContext)) {
  // Immediately produce a SecureContext (opaque) and no further outgoing
  #(None, state, Some(provider.SecureContext))
}

pub fn encrypt(
  _context: provider.SecureContext,
  plain: BitArray,
) -> Result(BitArray, String) {
  // Identity — insecure
  Ok(plain)
}

pub fn decrypt(
  _context: provider.SecureContext,
  cipher: BitArray,
) -> Result(BitArray, String) {
  // Identity — insecure
  Ok(cipher)
}

pub fn rekey(
  context: provider.SecureContext,
) -> Result(provider.SecureContext, String) {
  Ok(context)
}

pub fn close(_state: provider.ProviderState) -> Nil {
  Nil
}
