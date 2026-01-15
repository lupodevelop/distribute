//// Crypto Adapter contract and utilities.
////
//// This module defines the behaviour contract that all crypto providers
//// must implement, along with helper functions for creating default options.
////
//// ## Adapter Contract
////
//// A crypto provider handles:
////
//// - Lifecycle management (init/shutdown)
//// - Key exchange handshake (start/continue)
//// - Secure context management
//// - Encryption and decryption
//// - Key rotation (rekeying)
//// - Health monitoring
////
//// ## Implementing a Provider
////
//// To implement a custom provider, create a module that provides a function
//// returning a `CryptoAdapter` record with all required functions:
////
//// ```gleam
//// pub fn new() -> CryptoAdapter {
////   CryptoAdapter(
////     init: my_init,
////     shutdown: my_shutdown,
////     handshake_start: my_handshake_start,
////     handshake_continue: my_handshake_continue,
////     secure_context: my_secure_context,
////     encrypt: my_encrypt,
////     decrypt: my_decrypt,
////     rekey: my_rekey,
////     health: my_health,
////     metrics: my_metrics,
////   )
//// }
//// ```
////
//// See `distribute/crypto/noop_adapter` for a reference implementation.

import distribute/crypto/types.{
  type CryptoError, type CryptoMetrics, type HandshakeMessage,
  type HandshakeResult, type HandshakeState, type HealthStatus, type NodeId,
  type ProviderHandle, type ProviderOptions, type SecureContext,
}
import gleam/dict
import gleam/option.{type Option}

// =============================================================================
// Behaviour Contract
// =============================================================================

/// Crypto adapter behaviour contract.
///
/// All crypto implementations must provide these 10 core functions.
pub type CryptoAdapter {
  CryptoAdapter(
    /// Initialize the provider with options.
    /// Returns an opaque handle for subsequent operations.
    init: fn(ProviderOptions) -> Result(ProviderHandle, CryptoError),
    /// Shutdown the provider gracefully.
    /// Should zero sensitive memory where possible.
    shutdown: fn(ProviderHandle) -> Result(Nil, CryptoError),
    /// Start a handshake with a remote node.
    /// Returns initial handshake state and optional first message to send.
    handshake_start: fn(
      ProviderHandle,
      NodeId,
      NodeId,
      Option(HandshakeMessage),
    ) ->
      Result(HandshakeResult, CryptoError),
    /// Continue an in-progress handshake with an incoming message.
    /// Returns updated state, established context, or error.
    handshake_continue: fn(ProviderHandle, HandshakeState, HandshakeMessage) ->
      Result(HandshakeResult, CryptoError),
    /// Get the secure context for a node if established.
    secure_context: fn(ProviderHandle, NodeId) -> Option(SecureContext),
    /// Encrypt plaintext using a secure context.
    /// Returns ciphertext or error.
    encrypt: fn(ProviderHandle, SecureContext, BitArray) ->
      Result(BitArray, CryptoError),
    /// Decrypt ciphertext using a secure context.
    /// Returns plaintext or error.
    decrypt: fn(ProviderHandle, SecureContext, BitArray) ->
      Result(BitArray, CryptoError),
    /// Trigger a rekey for a specific node.
    /// Rotates the encryption keys for the connection.
    rekey: fn(ProviderHandle, NodeId) -> Result(Nil, CryptoError),
    /// Get current health status.
    health: fn(ProviderHandle) -> HealthStatus,
    /// Get provider metrics.
    metrics: fn(ProviderHandle) -> CryptoMetrics,
  )
}

/// Create default provider options.
///
/// Provides sensible defaults:
/// - is_development: False
/// - key_rotation_interval_ms: 0 (no auto-rotation)
/// - handshake_timeout_ms: 30000 (30 seconds)
pub fn default_options(name: String) -> ProviderOptions {
  types.ProviderOptions(
    name: name,
    is_development: False,
    key_rotation_interval_ms: 0,
    handshake_timeout_ms: 30_000,
    custom: dict.new(),
  )
}

/// Create development provider options.
///
/// **Warning:** These options are for development only and should
/// never be used in production. The provider will be marked as insecure.
pub fn development_options(name: String) -> ProviderOptions {
  types.ProviderOptions(
    name: name,
    is_development: True,
    key_rotation_interval_ms: 0,
    handshake_timeout_ms: 5000,
    custom: dict.new(),
  )
}
// =============================================================================
// Implementation Notes
// =============================================================================
//
// Lifecycle Semantics
// -------------------
//
// 1. init() must allocate resources and prepare for handshakes.
//    Failure to init should be immediately reported.
//
// 2. shutdown() must clean up resources and zero sensitive memory.
//    Providers should ensure no further operations after shutdown.
//
// Handshake Flow
// --------------
//
// 1. Caller calls handshake_start() to begin.
// 2. If result is Continue, exchange messages with remote.
// 3. Call handshake_continue() with each incoming message.
// 4. When result is Established, secure context is ready.
//
// Error Classification
// --------------------
//
// - Transient (retryable with backoff):
//   TransientNetwork, Timeout
//
// - Permanent (do not retry):
//   InvalidSignature, KeyMismatch, DecryptionFailed
//
// Security Considerations
// -----------------------
//
// - Never log key material
// - Use opaque handles for all sensitive data
// - Providers marked is_development=True must never be used in production
// - Implement proper key zeroing on shutdown
