//// Sodium adapter scaffold (not implemented).
////
//// This module provides the API structure for a future libsodium-backed adapter.
//// The native libsodium implementation is NOT provided.
////
//// **Use `otp_crypto_adapter` instead** for a working crypto adapter based on
//// Erlang's `:crypto` module.
////
//// This stub exists to reserve the module name and API for future native
//// libsodium integration (using NIF with sodium_malloc/sodium_memzero/mlock).

import distribute/crypto/adapter.{type CryptoAdapter}
import distribute/crypto/types.{
  type CryptoError, type CryptoMetrics, type HandshakeMessage,
  type HandshakeResult, type HandshakeState, type HealthStatus, type NodeId,
  type ProviderHandle, type ProviderOptions, type SecureContext,
}
import gleam/option.{type Option, None}

fn not_implemented_err() -> CryptoError {
  types.ProviderFailure(
    "Native libsodium backend not implemented — use otp_crypto_adapter instead",
  )
}

pub fn new() -> CryptoAdapter {
  adapter.CryptoAdapter(
    init: sodium_init,
    shutdown: sodium_shutdown,
    handshake_start: sodium_handshake_start,
    handshake_continue: sodium_handshake_continue,
    secure_context: sodium_secure_context,
    encrypt: sodium_encrypt,
    decrypt: sodium_decrypt,
    rekey: sodium_rekey,
    health: sodium_health,
    metrics: sodium_metrics,
  )
}

fn sodium_init(_options: ProviderOptions) -> Result(ProviderHandle, CryptoError) {
  Error(not_implemented_err())
}

fn sodium_shutdown(_handle: ProviderHandle) -> Result(Nil, CryptoError) {
  Ok(Nil)
}

fn sodium_handshake_start(
  _handle: ProviderHandle,
  _local: NodeId,
  _remote: NodeId,
  _initial: Option(HandshakeMessage),
) -> Result(HandshakeResult, CryptoError) {
  Error(not_implemented_err())
}

fn sodium_handshake_continue(
  _handle: ProviderHandle,
  _state: HandshakeState,
  _message: HandshakeMessage,
) -> Result(HandshakeResult, CryptoError) {
  Error(not_implemented_err())
}

fn sodium_secure_context(
  _handle: ProviderHandle,
  _node: NodeId,
) -> Option(SecureContext) {
  None
}

fn sodium_encrypt(
  _handle: ProviderHandle,
  _ctx: SecureContext,
  _plaintext: BitArray,
) -> Result(BitArray, CryptoError) {
  Error(not_implemented_err())
}

fn sodium_decrypt(
  _handle: ProviderHandle,
  _ctx: SecureContext,
  _ciphertext: BitArray,
) -> Result(BitArray, CryptoError) {
  Error(not_implemented_err())
}

fn sodium_rekey(
  _handle: ProviderHandle,
  _node: NodeId,
) -> Result(Nil, CryptoError) {
  Error(not_implemented_err())
}

fn sodium_health(_handle: ProviderHandle) -> HealthStatus {
  types.Down("sodium adapter not implemented")
}

fn sodium_metrics(_handle: ProviderHandle) -> CryptoMetrics {
  types.empty_metrics()
}
