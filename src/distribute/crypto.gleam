//// Crypto facade - singleton per node.
////
//// This module provides the public API for cryptographic operations
//// in the distribute cluster. It follows the same facade pattern as
//// transport and discovery modules.
////
//// ## Architecture
////
//// ```
//// ┌─────────────────────────────────────────────────────────┐
//// │                    crypto.gleam                         │
//// │                   (Facade/API)                          │
//// └─────────────────────────────────────────────────────────┘
////                            │
////                            ▼
//// ┌─────────────────────────────────────────────────────────┐
//// │                  CryptoAdapter                          │
//// │               (Behaviour contract)                      │
//// └─────────────────────────────────────────────────────────┘
////                   ▲                    ▲
////                   │                    │
////     ┌─────────────┴────────┐   ┌──────┴───────────┐
////     │    noop_adapter      │   │  (future: TLS,   │
////     │    (development)     │   │   NaCl, etc.)    │
////     └──────────────────────┘   └──────────────────┘
//// ```
////
//// ## Usage
////
//// ```gleam
//// import distribute/crypto
//// import gleam/option.{Some}
////
//// // Start crypto layer (typically done in node_builder)
//// let assert Ok(handle) = crypto.start_link(crypto.default_options("my_crypto"))
////
//// // Initiate handshake with remote node
//// let local = "node_a@host"
//// let remote = "node_b@host"
//// let assert Ok(result) = crypto.handshake_start(local, remote, option.None)
////
//// // Encrypt message
//// let assert Some(ctx) = crypto.secure_context(remote)
//// let assert Ok(ciphertext) = crypto.encrypt(ctx, <<"hello">>)
//// ```
////
//// ## Lifecycle
////
//// 1. Call `start_link()` or use `child_spec()` with a supervisor
//// 2. Provider initializes and registers with registry
//// 3. Use `handshake_start/3` when connecting to remote nodes
//// 4. Use `encrypt/2` and `decrypt/2` for secure messaging
//// 5. Call `shutdown()` for graceful cleanup

import distribute/crypto/adapter.{type CryptoAdapter}
import distribute/crypto/noop_adapter
import distribute/crypto/types.{
  type CryptoError, type CryptoMetrics, type HandshakeMessage,
  type HandshakeResult, type HandshakeState, type HealthStatus, type NodeId,
  type ProviderHandle, type ProviderOptions, type SecureContext,
}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None}
import gleam/otp/supervision.{type ChildSpecification}

// =============================================================================
// Constants
// =============================================================================

/// Default name for the crypto provider.
pub const default_name = "distribute_crypto"

// =============================================================================
// Public API - Configuration
// =============================================================================

/// Create default provider options.
pub fn default_options(name: String) -> ProviderOptions {
  adapter.default_options(name)
}

/// Create development options with debug logging.
pub fn development_options(name: String) -> ProviderOptions {
  adapter.development_options(name)
}

// =============================================================================
// Public API - Lifecycle
// =============================================================================

/// Create a child specification for OTP supervision.
///
/// Uses the noop adapter by default. For production, you should
/// use a proper crypto adapter.
pub fn child_spec() -> ChildSpecification(ProviderHandle) {
  let options = default_options(default_name)
  noop_adapter.child_spec(options)
}

/// Create a child specification with custom options.
pub fn child_spec_with_options(
  options: ProviderOptions,
) -> ChildSpecification(ProviderHandle) {
  noop_adapter.child_spec(options)
}

/// Create a child specification with a custom adapter.
/// Note: For custom adapters, use the adapter's own child_spec function.
pub fn child_spec_with_adapter(
  _provider: CryptoAdapter,
  options: ProviderOptions,
) -> ChildSpecification(ProviderHandle) {
  // For now, only noop_adapter is supported as supervised worker
  noop_adapter.child_spec(options)
}

/// Start the crypto provider with default settings.
pub fn start_link(
  options: ProviderOptions,
) -> Result(ProviderHandle, CryptoError) {
  do_start_link(noop_adapter.new(), options)
}

/// Start the crypto provider with a custom adapter.
pub fn start_link_with_adapter(
  provider: CryptoAdapter,
  options: ProviderOptions,
) -> Result(ProviderHandle, CryptoError) {
  do_start_link(provider, options)
}

fn do_start_link(
  provider: CryptoAdapter,
  options: ProviderOptions,
) -> Result(ProviderHandle, CryptoError) {
  case { provider.init }(options) {
    Ok(handle) -> {
      // Store handle in persistent_term for later lookup
      let _ = store_handle(options.name, handle)
      Ok(handle)
    }
    Error(err) -> Error(err)
  }
}

/// Shutdown the crypto provider.
pub fn shutdown() -> Result(Nil, CryptoError) {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.shutdown }(handle)
    }
    Error(_) -> Error(types.NotInitialized)
  }
}

// =============================================================================
// Public API - Handshake
// =============================================================================

/// Start a cryptographic handshake with a remote node.
///
/// Returns either:
/// - `InProgress(state, message)` - Send message to remote, await response
/// - `Established(context)` - Handshake complete (e.g., noop adapter)
pub fn handshake_start(
  local: NodeId,
  remote: NodeId,
  initial: Option(HandshakeMessage),
) -> Result(HandshakeResult, CryptoError) {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.handshake_start }(handle, local, remote, initial)
    }
    Error(_) -> Error(types.NotInitialized)
  }
}

/// Continue a handshake with a message from the remote node.
pub fn handshake_continue(
  state: HandshakeState,
  message: HandshakeMessage,
) -> Result(HandshakeResult, CryptoError) {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.handshake_continue }(handle, state, message)
    }
    Error(_) -> Error(types.NotInitialized)
  }
}

// =============================================================================
// Public API - Secure Context
// =============================================================================

/// Get the secure context for a node (if handshake completed).
pub fn secure_context(node: NodeId) -> Option(SecureContext) {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.secure_context }(handle, node)
    }
    Error(_) -> None
  }
}

// =============================================================================
// Public API - Encryption/Decryption
// =============================================================================

/// Encrypt a message using the secure context.
pub fn encrypt(
  ctx: SecureContext,
  plaintext: BitArray,
) -> Result(BitArray, CryptoError) {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.encrypt }(handle, ctx, plaintext)
    }
    Error(_) -> Error(types.NotInitialized)
  }
}

/// Decrypt a message using the secure context.
pub fn decrypt(
  ctx: SecureContext,
  ciphertext: BitArray,
) -> Result(BitArray, CryptoError) {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.decrypt }(handle, ctx, ciphertext)
    }
    Error(_) -> Error(types.NotInitialized)
  }
}

// =============================================================================
// Public API - Key Management
// =============================================================================

/// Trigger a rekey operation for a node.
///
/// This rotates the session key while maintaining the connection.
pub fn rekey(node: NodeId) -> Result(Nil, CryptoError) {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.rekey }(handle, node)
    }
    Error(_) -> Error(types.NotInitialized)
  }
}

// =============================================================================
// Public API - Health and Metrics
// =============================================================================

/// Get the health status of the crypto provider.
pub fn health() -> HealthStatus {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.health }(handle)
    }
    Error(_) -> types.Down("Not initialized")
  }
}

/// Get metrics from the crypto provider.
pub fn metrics() -> CryptoMetrics {
  case get_handle() {
    Ok(handle) -> {
      let adapter = get_current_adapter()
      { adapter.metrics }(handle)
    }
    Error(_) -> types.empty_metrics()
  }
}

// =============================================================================
// Handle Management
// =============================================================================

/// Get the current provider handle.
pub fn get_handle() -> Result(ProviderHandle, Nil) {
  get_stored_handle(default_name)
}

/// Get a handle by name.
pub fn get_handle_by_name(name: String) -> Result(ProviderHandle, Nil) {
  get_stored_handle(name)
}

// =============================================================================
// Internal - Handle Storage (using persistent_term)
// =============================================================================

fn store_handle(name: String, handle: ProviderHandle) -> Result(Nil, Nil) {
  // Store handle in persistent_term for fast global lookup
  let key = "crypto_handle_" <> name
  let state = types.handle_state(handle)
  put_persistent_term(key, state)
  Ok(Nil)
}

fn get_stored_handle(name: String) -> Result(ProviderHandle, Nil) {
  let key = "crypto_handle_" <> name
  case get_persistent_term(key) {
    Ok(state) -> Ok(types.new_handle(name, state))
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// Internal - Current Adapter
// =============================================================================

/// Get the current adapter.
/// For now, always returns noop_adapter.
/// In future, this will be configurable.
fn get_current_adapter() -> CryptoAdapter {
  noop_adapter.new()
}

// =============================================================================
// FFI
// =============================================================================

@external(erlang, "crypto_provider_ffi", "put_persistent_term")
fn put_persistent_term(key: String, value: Dynamic) -> Nil

@external(erlang, "crypto_provider_ffi", "get_persistent_term")
fn get_persistent_term(key: String) -> Result(Dynamic, Nil)
