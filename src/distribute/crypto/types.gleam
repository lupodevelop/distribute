//// Crypto layer type definitions.
////
//// This module contains all shared types used by crypto providers. These
//// types form the contract between the provider implementations and the
//// higher-level crypto facade.
////
//// ## Type Categories
////
//// - **Handles** - Opaque references to running providers
//// - **Contexts** - Secure contexts for encrypt/decrypt
//// - **Status** - Health and metrics
//// - **Errors** - Structured error types

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

// =============================================================================
// Handles
// =============================================================================

/// Opaque handle representing an active crypto provider instance.
///
/// Created by `adapter.init()` and required for all subsequent operations.
pub opaque type ProviderHandle {
  ProviderHandle(id: String, state: Dynamic)
}

/// Create a new provider handle.
pub fn new_handle(id: String, state: Dynamic) -> ProviderHandle {
  ProviderHandle(id: id, state: state)
}

/// Get the handle's identifier.
pub fn handle_id(handle: ProviderHandle) -> String {
  handle.id
}

/// Get the handle's internal state.
pub fn handle_state(handle: ProviderHandle) -> Dynamic {
  handle.state
}

// =============================================================================
// Node ID
// =============================================================================

/// Node identifier.
pub type NodeId =
  String

// =============================================================================
// Secure Context
// =============================================================================

/// Handshake state machine stages.
pub type HandshakeStage {
  /// No secure context established
  Plain
  /// Key exchange in progress
  KeyExchangeInProgress
  /// Secure context established
  SecureEstablished
  /// Rekeying in progress
  Rekeying
  /// Handshake failed
  Failed(reason: String)
}

/// Opaque secure context for encrypt/decrypt operations.
///
/// **Security Note:** Never log or serialize the internal contents.
pub opaque type SecureContext {
  SecureContext(
    node_id: NodeId,
    stage: HandshakeStage,
    created_at_ms: Int,
    key_id: String,
    /// Internal key material (opaque to prevent leakage)
    key_material: Dynamic,
  )
}

/// Create a new secure context (provider internal use only).
pub fn new_secure_context(
  node_id: NodeId,
  stage: HandshakeStage,
  created_at_ms: Int,
  key_id: String,
  key_material: Dynamic,
) -> SecureContext {
  SecureContext(
    node_id: node_id,
    stage: stage,
    created_at_ms: created_at_ms,
    key_id: key_id,
    key_material: key_material,
  )
}

/// Get the node ID from a secure context.
pub fn context_node_id(ctx: SecureContext) -> NodeId {
  ctx.node_id
}

/// Get the handshake stage.
pub fn context_stage(ctx: SecureContext) -> HandshakeStage {
  ctx.stage
}

/// Get the key ID (for logging/debugging without exposing key material).
pub fn context_key_id(ctx: SecureContext) -> String {
  ctx.key_id
}

/// Get the key material (provider internal use only).
pub fn context_key_material(ctx: SecureContext) -> Dynamic {
  ctx.key_material
}

/// Get the creation timestamp.
pub fn context_created_at(ctx: SecureContext) -> Int {
  ctx.created_at_ms
}

// =============================================================================
// Handshake State
// =============================================================================

/// Intermediate state during handshake.
pub opaque type HandshakeState {
  HandshakeState(
    local_node: NodeId,
    remote_node: NodeId,
    stage: HandshakeStage,
    data: Dynamic,
  )
}

/// Create a new handshake state.
pub fn new_handshake_state(
  local_node: NodeId,
  remote_node: NodeId,
  stage: HandshakeStage,
  data: Dynamic,
) -> HandshakeState {
  HandshakeState(
    local_node: local_node,
    remote_node: remote_node,
    stage: stage,
    data: data,
  )
}

/// Get handshake stage.
pub fn handshake_stage(state: HandshakeState) -> HandshakeStage {
  state.stage
}

/// Get remote node.
pub fn handshake_remote_node(state: HandshakeState) -> NodeId {
  state.remote_node
}

/// Get local node.
pub fn handshake_local_node(state: HandshakeState) -> NodeId {
  state.local_node
}

/// Get handshake data (provider internal).
pub fn handshake_data(state: HandshakeState) -> Dynamic {
  state.data
}

// =============================================================================
// Handshake Messages
// =============================================================================

/// Message exchanged during handshake.
pub type HandshakeMessage {
  HandshakeMessage(
    message_type: String,
    payload: BitArray,
    metadata: Option(Dict(String, String)),
  )
}

/// Result of a handshake step.
pub type HandshakeResult {
  /// Continue handshake with optional response
  Continue(state: HandshakeState, response: Option(HandshakeMessage))
  /// Handshake completed
  Established(context: SecureContext)
  /// Handshake failed
  HandshakeError(error: CryptoError)
}

// =============================================================================
// Health Status
// =============================================================================

/// Health status of a crypto provider.
pub type HealthStatus {
  /// Provider is fully operational
  Up
  /// Provider is degraded
  Degraded(reason: String)
  /// Provider is down
  Down(reason: String)
}

/// Crypto provider metrics.
pub type CryptoMetrics {
  CryptoMetrics(
    handshakes_initiated: Int,
    handshakes_completed: Int,
    handshakes_failed: Int,
    encrypt_count: Int,
    decrypt_count: Int,
    rekey_count: Int,
    active_contexts: Int,
  )
}

/// Default empty metrics.
pub fn empty_metrics() -> CryptoMetrics {
  CryptoMetrics(
    handshakes_initiated: 0,
    handshakes_completed: 0,
    handshakes_failed: 0,
    encrypt_count: 0,
    decrypt_count: 0,
    rekey_count: 0,
    active_contexts: 0,
  )
}

// =============================================================================
// Configuration
// =============================================================================

/// Options for initializing a crypto provider.
pub type ProviderOptions {
  ProviderOptions(
    name: String,
    is_development: Bool,
    key_rotation_interval_ms: Int,
    handshake_timeout_ms: Int,
    custom: Dict(String, String),
  )
}

// =============================================================================
// Errors
// =============================================================================

/// Errors from crypto operations.
pub type CryptoError {
  /// Provider not initialized
  NotInitialized
  /// Initialization failed
  InitFailed(reason: String)
  /// Shutdown failed
  ShutdownFailed(reason: String)
  /// Transient network error
  TransientNetwork(reason: String)
  /// Invalid signature
  InvalidSignature
  /// Key mismatch
  KeyMismatch
  /// Decryption failed
  DecryptionFailed(reason: String)
  /// Encryption failed
  EncryptionFailed(reason: String)
  /// No secure context for node
  NoSecureContext(node: NodeId)
  /// Handshake failed
  HandshakeFailed(reason: String)
  /// Rekey failed
  RekeyFailed(reason: String)
  /// Internal provider error
  ProviderFailure(reason: String)
  /// Operation timed out
  Timeout(elapsed_ms: Int)
}

/// Check if an error is transient.
pub fn is_transient_error(error: CryptoError) -> Bool {
  case error {
    TransientNetwork(_) -> True
    Timeout(_) -> True
    _ -> False
  }
}

/// Check if an error is permanent.
pub fn is_permanent_error(error: CryptoError) -> Bool {
  case error {
    InvalidSignature -> True
    KeyMismatch -> True
    DecryptionFailed(_) -> True
    InitFailed(_) -> True
    _ -> False
  }
}
