//// Crypto provider behaviour contract for secure communications.
////
//// This module defines the contract that crypto providers must implement
//// to provide encryption, key exchange, and secure context management
//// in the distribute library.
////
//// ## Design Philosophy
////
//// This behaviour follows distribute's patterns:
//// - Pure function signatures (like `transport/adapter`)
//// - Actor-based implementations for state management
//// - Integration with `handshake` for key exchange
//// - Opaque handles for secure context (no key leakage)
////
//// ## Implementation Approaches
////
//// Providers can be implemented as:
//// 1. **No-op provider**: Identity transformation for development (insecure!)
//// 2. **AEAD provider**: AES-GCM or ChaCha20-Poly1305
//// 3. **ECDH provider**: Elliptic curve key exchange + symmetric encryption
////
//// ## State Machine
////
//// Key exchange follows a state machine:
//// ```
//// Plain -> KeyExchangeInProgress -> SecureEstablished
////                                         |
////                                         v
////                                      Rekeying
////                                         |
////                                         v
////                                  SecureEstablished
//// ```
////
//// Failed states can occur at any transition.
////
//// ## Security Considerations
////
//// - Never log key material
//// - Use opaque handles for keys and contexts
//// - Zero sensitive memory on shutdown when possible
//// - Mark development providers as insecure
////
//// ## Example Implementation
////
//// See `distribute/crypto/noop_adapter` for a reference implementation.

import gleam/dict.{type Dict}
import gleam/option.{type Option}

// =============================================================================
// Core Types
// =============================================================================

/// Node identifier for crypto operations.
pub type NodeId =
  String

/// Handshake state machine stages.
///
/// Represents the current state of key exchange with a remote node.
pub type HandshakeStage {
  /// No secure context established yet
  Plain
  /// Key exchange is in progress
  KeyExchangeInProgress
  /// Secure context successfully established
  SecureEstablished
  /// Rekeying in progress (rotating keys)
  Rekeying
  /// Handshake failed
  Failed(reason: String)
}

/// Opaque secure context for a node connection.
///
/// Contains the cryptographic material needed for encrypt/decrypt
/// operations. The internal structure is provider-specific and
/// should never be logged or serialized.
pub opaque type SecureContext {
  SecureContext(
    node_id: NodeId,
    stage: HandshakeStage,
    created_at_ms: Int,
    key_id: String,
  )
}

/// Create a new secure context (for provider implementations).
pub fn new_secure_context(
  node_id: NodeId,
  stage: HandshakeStage,
  created_at_ms: Int,
  key_id: String,
) -> SecureContext {
  SecureContext(
    node_id: node_id,
    stage: stage,
    created_at_ms: created_at_ms,
    key_id: key_id,
  )
}

/// Get the node ID from a secure context.
pub fn context_node_id(ctx: SecureContext) -> NodeId {
  ctx.node_id
}

/// Get the handshake stage from a secure context.
pub fn context_stage(ctx: SecureContext) -> HandshakeStage {
  ctx.stage
}

/// Get the key ID from a secure context.
pub fn context_key_id(ctx: SecureContext) -> String {
  ctx.key_id
}

/// Handshake state during key exchange.
///
/// This opaque type holds the intermediate state during handshake
/// and is passed between handshake_start and handshake_continue calls.
pub opaque type HandshakeState {
  HandshakeState(
    local_node: NodeId,
    remote_node: NodeId,
    stage: HandshakeStage,
    data: Dict(String, String),
  )
}

/// Create a new handshake state (for provider implementations).
pub fn new_handshake_state(
  local_node: NodeId,
  remote_node: NodeId,
  stage: HandshakeStage,
) -> HandshakeState {
  HandshakeState(
    local_node: local_node,
    remote_node: remote_node,
    stage: stage,
    data: dict.new(),
  )
}

/// Get the stage from handshake state.
pub fn handshake_stage(state: HandshakeState) -> HandshakeStage {
  state.stage
}

/// Get the remote node from handshake state.
pub fn handshake_remote_node(state: HandshakeState) -> NodeId {
  state.remote_node
}

// =============================================================================
// Health & Status
// =============================================================================

/// Health status of a crypto provider.
pub type HealthStatus {
  /// Provider is fully operational
  Up
  /// Provider is operational but degraded
  Degraded(reason: String)
  /// Provider is down
  Down(reason: String)
}

/// Crypto provider metrics.
pub type CryptoMetrics {
  CryptoMetrics(
    /// Total handshakes initiated
    handshakes_initiated: Int,
    /// Total handshakes completed successfully
    handshakes_completed: Int,
    /// Total handshakes failed
    handshakes_failed: Int,
    /// Total encrypt operations
    encrypt_count: Int,
    /// Total decrypt operations
    decrypt_count: Int,
    /// Total rekey operations
    rekey_count: Int,
    /// Active secure contexts
    active_contexts: Int,
  )
}

// =============================================================================
// Errors
// =============================================================================

/// Errors from crypto operations.
///
/// ## Classification
///
/// - **Transient** (retryable): `TransientNetwork`, `Timeout`
/// - **Permanent** (do not retry): `InvalidSignature`, `KeyMismatch`, `DecryptionFailed`
pub type CryptoError {
  /// Provider initialization failed
  InitFailed(reason: String)
  /// Provider shutdown failed
  ShutdownFailed(reason: String)
  /// Transient network error during key exchange
  TransientNetwork(reason: String)
  /// Invalid cryptographic signature
  InvalidSignature
  /// Key mismatch during verification
  KeyMismatch
  /// Decryption failed (invalid ciphertext or wrong key)
  DecryptionFailed(reason: String)
  /// Encryption failed
  EncryptionFailed(reason: String)
  /// No secure context for node
  NoSecureContext(node: NodeId)
  /// Handshake failed
  HandshakeFailed(reason: String)
  /// Rekey operation failed
  RekeyFailed(reason: String)
  /// Internal provider error
  ProviderFailure(reason: String)
  /// Operation timed out
  Timeout(elapsed_ms: Int)
}

/// Check if an error is transient and should be retried.
pub fn is_transient_error(error: CryptoError) -> Bool {
  case error {
    TransientNetwork(_) -> True
    Timeout(_) -> True
    _ -> False
  }
}

/// Check if an error is permanent and should not be retried.
pub fn is_permanent_error(error: CryptoError) -> Bool {
  case error {
    InvalidSignature -> True
    KeyMismatch -> True
    DecryptionFailed(_) -> True
    InitFailed(_) -> True
    _ -> False
  }
}

// =============================================================================
// Configuration
// =============================================================================

/// Options for initializing a crypto provider.
pub type ProviderOptions {
  ProviderOptions(
    /// Registered name for the provider process
    name: String,
    /// Whether this is a development/insecure provider
    is_development: Bool,
    /// Key rotation interval in milliseconds (0 = no auto-rotation)
    key_rotation_interval_ms: Int,
    /// Handshake timeout in milliseconds
    handshake_timeout_ms: Int,
    /// Provider-specific custom options
    custom: Dict(String, String),
  )
}

/// Default provider options.
pub fn default_options(name: String) -> ProviderOptions {
  ProviderOptions(
    name: name,
    is_development: False,
    key_rotation_interval_ms: 0,
    handshake_timeout_ms: 30_000,
    custom: dict.new(),
  )
}

// =============================================================================
// Handshake Messages
// =============================================================================

/// Messages exchanged during handshake.
///
/// The format is provider-specific but typically includes:
/// - Public keys or key shares
/// - Nonces and session identifiers
/// - Signatures for authentication
pub type HandshakeMessage {
  HandshakeMessage(
    /// Message type identifier
    message_type: String,
    /// Binary payload
    payload: BitArray,
    /// Optional metadata
    metadata: Option(Dict(String, String)),
  )
}

/// Result of a handshake step.
pub type HandshakeResult {
  /// Handshake still in progress, continue with next message
  Continue(state: HandshakeState, response: Option(HandshakeMessage))
  /// Handshake completed, secure context established
  Established(context: SecureContext)
  /// Handshake failed
  HandshakeError(error: CryptoError)
}
