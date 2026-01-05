//// Discovery layer type definitions.
////
//// This module contains all shared types used by discovery adapters. These
//// types form the contract between the adapter implementations and the
//// higher-level discovery facade.
////
//// ## Type Categories
////
//// - **Handles** - Opaque references to running adapters
//// - **Status** - Health and subscription state
//// - **Errors** - Structured error types for operations
//// - **Peers** - Peer information and metadata

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

// =============================================================================
// Handles
// =============================================================================

/// Opaque handle representing an active discovery adapter instance.
///
/// Created by `adapter.start()` and required for all subsequent operations.
/// The handle contains internal state that should not be accessed directly.
pub opaque type AdapterHandle {
  AdapterHandle(id: String, state: Dynamic)
}

/// Create a new adapter handle with an identifier and internal state.
///
/// This is typically called by adapter implementations, not user code.
pub fn new_handle(id: String, state: Dynamic) -> AdapterHandle {
  AdapterHandle(id: id, state: state)
}

/// Get the handle's identifier (the name it was registered with).
pub fn handle_id(handle: AdapterHandle) -> String {
  handle.id
}

/// Get the handle's internal state.
///
/// Used by adapter implementations to retrieve their actor Subject.
pub fn handle_state(handle: AdapterHandle) -> Dynamic {
  handle.state
}

// =============================================================================
// Peer Types (re-exported from behaviour for convenience)
// =============================================================================

/// Peer identifier.
pub type PeerId =
  String

/// Peer metadata as key-value pairs.
pub type PeerMetadata =
  Dict(String, String)

/// Complete peer information.
pub type PeerInfo {
  PeerInfo(id: PeerId, metadata: PeerMetadata)
}

// =============================================================================
// Health Status
// =============================================================================

/// Health status of a discovery adapter.
pub type HealthStatus {
  /// Adapter is fully operational
  Up
  /// Adapter is operational but degraded
  Degraded(reason: String)
  /// Adapter is down
  Down(reason: String)
}

/// Extended health info with sync details.
pub type HealthInfo {
  HealthInfo(
    status: HealthStatus,
    last_sync_time_ms: Option(Int),
    last_error: Option(String),
    known_peers_count: Int,
  )
}

// =============================================================================
// Subscriptions
// =============================================================================

/// Subscription identifier returned by subscribe().
pub opaque type SubscriptionId {
  SubscriptionId(String)
}

/// Create a new subscription ID.
pub fn new_subscription_id(id: String) -> SubscriptionId {
  SubscriptionId(id)
}

/// Extract the string value from a subscription ID.
pub fn subscription_id_value(id: SubscriptionId) -> String {
  let SubscriptionId(value) = id
  value
}

// =============================================================================
// Events
// =============================================================================

/// Events emitted by discovery adapters.
pub type DiscoveryEvent {
  /// A new peer has been discovered
  PeerUp(peer: PeerId, metadata: PeerMetadata)
  /// A known peer's metadata has changed
  PeerUpdate(peer: PeerId, metadata: PeerMetadata)
  /// A peer has left or become unavailable
  PeerDown(peer: PeerId, reason: String)
}

/// Callback invoked when a discovery event occurs.
pub type EventCallback =
  fn(DiscoveryEvent) -> Nil

// =============================================================================
// Configuration
// =============================================================================

/// Options for starting a discovery adapter.
pub type AdapterOptions {
  AdapterOptions(
    /// Registered name for the adapter process
    name: String,
    /// How often to sync with discovery backend (ms)
    sync_interval_ms: Int,
    /// Timeout for sync operations (ms)
    sync_timeout_ms: Int,
    /// Prefix for telemetry events
    telemetry_prefix: String,
    /// Initial seed peers for bootstrap
    seed_peers: List(PeerId),
    /// Retry/backoff configuration for sync failures
    retry_config: RetryConfig,
    /// Optional hook to call on PeerUp (e.g., trigger handshake)
    on_peer_up: Option(fn(PeerId, PeerMetadata) -> Nil),
    /// Adapter-specific custom options
    custom: Dict(String, Dynamic),
  )
}

/// Retry/backoff configuration for transient failures.
pub type RetryConfig {
  RetryConfig(
    /// Maximum number of retry attempts
    max_attempts: Int,
    /// Initial backoff delay in milliseconds
    initial_backoff_ms: Int,
    /// Maximum backoff delay in milliseconds
    max_backoff_ms: Int,
    /// Multiplier for exponential backoff (e.g., 2.0 doubles each time)
    backoff_multiplier: Float,
  )
}

/// Default retry configuration.
pub fn default_retry_config() -> RetryConfig {
  RetryConfig(
    max_attempts: 3,
    initial_backoff_ms: 100,
    max_backoff_ms: 5000,
    backoff_multiplier: 2.0,
  )
}

/// Discovery metrics for observability.
pub type DiscoveryMetrics {
  DiscoveryMetrics(
    /// Total sync operations performed
    sync_count: Int,
    /// Total sync failures
    sync_failures: Int,
    /// Last sync duration in milliseconds
    last_sync_duration_ms: Int,
    /// Total events emitted
    events_emitted: Int,
    /// Current known peers count
    known_peers_count: Int,
    /// Error rate (failures / total syncs)
    error_rate: Float,
  )
}

// =============================================================================
// Errors
// =============================================================================

/// Errors from discovery operations.
pub type DiscoveryError {
  /// Failed to start the adapter
  StartFailed(reason: String)
  /// Failed to stop the adapter
  StopFailed(reason: String)
  /// Shutdown timed out
  ShutdownTimeout(elapsed_ms: Int)
  /// Failed to sync with backend
  SyncFailed(reason: String)
  /// Peer not found
  PeerNotFound(peer: PeerId)
  /// Invalid configuration
  InvalidConfiguration(reason: String)
  /// Operation timed out
  Timeout(elapsed_ms: Int)
  /// Subscription error
  SubscriptionFailed(reason: String)
}

/// Check if an error is transient and should be retried.
pub fn is_transient_error(error: DiscoveryError) -> Bool {
  case error {
    SyncFailed(_) -> True
    Timeout(_) -> True
    _ -> False
  }
}

/// Check if an error is permanent and should not be retried.
pub fn is_permanent_error(error: DiscoveryError) -> Bool {
  case error {
    InvalidConfiguration(_) -> True
    PeerNotFound(_) -> True
    StartFailed(_) -> True
    _ -> False
  }
}
