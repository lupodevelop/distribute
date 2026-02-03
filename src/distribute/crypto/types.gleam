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
///
/// Creates an opaque handle that wraps provider state for safe passing
/// between crypto operations. The state is stored as Dynamic to allow
/// provider-specific implementations.
///
/// ## Arguments
///
/// - `id` - Unique identifier for this provider instance
/// - `state` - Provider-specific internal state (typically actor subject)
///
/// ## Returns
///
/// An opaque `ProviderHandle` for use with crypto operations.
pub fn new_handle(id: String, state: Dynamic) -> ProviderHandle {
  ProviderHandle(id: id, state: state)
}

/// Get the handle's identifier.
///
/// Returns the unique identifier assigned to this provider handle during
/// creation. Useful for logging, debugging, and registry lookups.
///
/// ## Example
///
/// ```gleam
/// let id = types.handle_id(handle)
/// io.println("Provider: " <> id)
/// ```
pub fn handle_id(handle: ProviderHandle) -> String {
  handle.id
}

/// Get the handle's internal state.
///
/// Returns the opaque internal state of the provider. This is typically
/// the actor subject used for message passing.
///
/// **Internal use only** - Provider implementations use this to extract
/// the actor subject for sending commands.
///
/// ## Security Note
///
/// Never log or serialize the returned Dynamic as it may contain
/// references to sensitive cryptographic state.
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
///
/// Constructs a `SecureContext` containing all cryptographic material
/// needed for encrypt/decrypt operations with a specific remote node.
///
/// ## Arguments
///
/// - `node_id` - Identifier of the remote node this context is for
/// - `stage` - Current handshake stage (should be `SecureEstablished`)
/// - `created_at_ms` - Creation timestamp in milliseconds
/// - `key_id` - Unique identifier for the current key (for logging)
/// - `key_material` - Opaque key material (provider-specific)
///
/// ## Security Note
///
/// This function is for provider implementations only. The `key_material`
/// parameter contains sensitive cryptographic keys and must never be
/// logged or serialized.
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
///
/// Returns the identifier of the remote node this context was established
/// with. Use this to verify you're using the correct context for a message.
///
/// ## Example
///
/// ```gleam
/// let node = types.context_node_id(ctx)
/// case node == expected_sender {
///   True -> decrypt(ctx, message)
///   False -> Error(WrongContext)
/// }
/// ```
pub fn context_node_id(ctx: SecureContext) -> NodeId {
  ctx.node_id
}

/// Get the handshake stage.
///
/// Returns the current stage of the handshake state machine. A context
/// should only be used for encryption when the stage is `SecureEstablished`.
///
/// ## Stages
///
/// - `Plain` - No security established
/// - `KeyExchangeInProgress` - Handshake ongoing
/// - `SecureEstablished` - Ready for encryption
/// - `Rekeying` - Key rotation in progress
/// - `Failed(reason)` - Handshake failed
pub fn context_stage(ctx: SecureContext) -> HandshakeStage {
  ctx.stage
}

/// Get the key ID (for logging/debugging without exposing key material).
///
/// Returns a unique identifier for the current encryption key. This ID
/// changes after each rekey operation and can be safely logged for
/// debugging purposes without exposing actual key material.
///
/// ## Example
///
/// ```gleam
/// log.debug("Using key: " <> types.context_key_id(ctx))
/// ```
pub fn context_key_id(ctx: SecureContext) -> String {
  ctx.key_id
}

/// Get the key material (provider internal use only).
///
/// Returns the opaque key material for encryption/decryption. This is
/// provider-specific and typically contains AEAD keys and master secrets.
///
/// **WARNING: Security-sensitive function**
///
/// - Never log the returned value
/// - Never serialize or transmit it
/// - Only use within provider implementations
/// - Provider should zero memory when context is destroyed
pub fn context_key_material(ctx: SecureContext) -> Dynamic {
  ctx.key_material
}

/// Get the creation timestamp.
///
/// Returns the timestamp (in milliseconds since epoch) when this secure
/// context was created. Useful for implementing key rotation policies
/// based on context age.
///
/// ## Example
///
/// ```gleam
/// let age_ms = now_ms() - types.context_created_at(ctx)
/// case age_ms > max_key_age_ms {
///   True -> crypto.rekey(node_id)
///   False -> Ok(Nil)
/// }
/// ```
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
///
/// Constructs intermediate state for tracking an in-progress handshake.
/// This state is passed between `handshake_start` and `handshake_continue`
/// calls during key exchange.
///
/// ## Arguments
///
/// - `local_node` - Identifier of the local node
/// - `remote_node` - Identifier of the remote node being connected to
/// - `stage` - Current stage in the handshake state machine
/// - `data` - Provider-specific handshake data (e.g., ephemeral keys)
///
/// ## Example
///
/// ```gleam
/// let state = types.new_handshake_state(
///   "node_a@localhost",
///   "node_b@localhost",
///   KeyExchangeInProgress,
///   wrap_pending_data(pending),
/// )
/// ```
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
///
/// Returns the current stage of the handshake state machine for this
/// handshake operation. Used to determine if more message exchanges
/// are needed or if the handshake has completed/failed.
///
/// ## Returns
///
/// - `KeyExchangeInProgress` - More exchanges needed
/// - `SecureEstablished` - Handshake complete
/// - `Failed(reason)` - Handshake failed
pub fn handshake_stage(state: HandshakeState) -> HandshakeStage {
  state.stage
}

/// Get remote node.
///
/// Returns the identifier of the remote node this handshake is being
/// conducted with. Used for context lookup and message routing.
pub fn handshake_remote_node(state: HandshakeState) -> NodeId {
  state.remote_node
}

/// Get local node.
///
/// Returns the identifier of the local node in this handshake.
pub fn handshake_local_node(state: HandshakeState) -> NodeId {
  state.local_node
}

/// Get handshake data (provider internal).
///
/// Returns the provider-specific handshake data. This typically contains
/// ephemeral keypairs and peer public keys needed to complete the
/// key exchange.
///
/// **Internal use only** - For provider implementations.
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
///
/// Returns a `CryptoMetrics` struct with all counters initialized to zero.
/// Useful as a fallback when metrics cannot be retrieved from a provider.
///
/// ## Example
///
/// ```gleam
/// let metrics = case adapter.metrics(handle) {
///   Ok(m) -> m
///   Error(_) -> types.empty_metrics()
/// }
/// ```
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
///
/// Transient errors are temporary and the operation may succeed if retried
/// with appropriate backoff. Use this to implement retry logic.
///
/// ## Transient Errors
///
/// - `TransientNetwork` - Temporary network issues
/// - `Timeout` - Operation timed out (may succeed later)
///
/// ## Example
///
/// ```gleam
/// case crypto.encrypt(ctx, data) {
///   Error(err) if types.is_transient_error(err) ->
///     retry_with_backoff(fn() { crypto.encrypt(ctx, data) })
///   Error(err) -> Error(err)
///   Ok(ciphertext) -> Ok(ciphertext)
/// }
/// ```
pub fn is_transient_error(error: CryptoError) -> Bool {
  case error {
    TransientNetwork(_) -> True
    Timeout(_) -> True
    _ -> False
  }
}

/// Check if an error is permanent.
///
/// Permanent errors indicate a fundamental problem that will not be
/// resolved by retrying. These require intervention (e.g., re-handshake,
/// configuration fix, or aborting the operation).
///
/// ## Permanent Errors
///
/// - `InvalidSignature` - Cryptographic verification failed
/// - `KeyMismatch` - Keys don't match expected values
/// - `DecryptionFailed` - Ciphertext corrupted or wrong key
/// - `InitFailed` - Provider couldn't initialize
///
/// ## Example
///
/// ```gleam
/// case crypto.decrypt(ctx, data) {
///   Error(err) if types.is_permanent_error(err) ->
///     // Don't retry, re-establish connection
///     reconnect_and_rehandshake(node)
///   Error(err) -> retry(fn() { crypto.decrypt(ctx, data) })
///   Ok(plaintext) -> Ok(plaintext)
/// }
/// ```
pub fn is_permanent_error(error: CryptoError) -> Bool {
  case error {
    InvalidSignature -> True
    KeyMismatch -> True
    DecryptionFailed(_) -> True
    InitFailed(_) -> True
    _ -> False
  }
}
