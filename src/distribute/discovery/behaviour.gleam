//// Discovery behaviour contract for peer discovery.
////
//// This module defines the contract that discovery adapters must implement
//// to provide peer discovery and membership tracking in the distribute library.
//// Discovery adapters are responsible for finding peers, tracking membership
//// state, and emitting membership events.
////
//// ## Design Philosophy
////
//// This behaviour follows distribute's patterns:
//// - Pure function signatures (like `crypto/provider`)
//// - Actor-based implementations (like `registry/actor`)  
//// - Event-driven with `process.Subject` (like `global`, `messaging`)
//// - Capability negotiation support (like `handshake`)
////
//// ## Implementation Approaches
////
//// Adapters can be implemented as:
//// 1. **Actor-based**: Use `gleam/otp/actor` (recommended, see `registry/actor`)
//// 2. **DNS-SRV**: Query DNS for service records
//// 3. **Kubernetes**: Use K8s API for pod discovery
//// 4. **Static**: Fixed list of peers for simple deployments
////
//// ## Integration with distribute
////
//// Discovery adapters integrate with:
//// - `handshake` module for triggering negotiation when peers join
//// - `registry` for tracking peer metadata
//// - `transport` for establishing connections to discovered peers
////
//// ## Example Implementation
////
//// See `distribute/discovery/beam_adapter` for a reference implementation.

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

// =============================================================================
// Core Types
// =============================================================================

/// Peer identifier used across the distribute library.
///
/// Matches the `NodeId` from `registry/behaviour` and `handshake` modules.
/// Typically formatted as "node@host" (Erlang node naming convention).
pub type PeerId =
  String

/// Metadata associated with a discovered peer.
///
/// Contains information about the peer such as:
/// - Service version
/// - Capabilities
/// - Health status
/// - Custom attributes
///
/// All keys and values are strings for portability.
pub type PeerMetadata =
  Dict(String, String)

/// Peer information including identity and metadata.
pub type PeerInfo {
  PeerInfo(id: PeerId, metadata: PeerMetadata)
}

// =============================================================================
// Events
// =============================================================================

/// Events emitted by discovery adapters.
///
/// Discovery adapters push events to subscribers when membership changes.
/// Subscribers should process events quickly; heavy processing should be
/// done asynchronously.
///
/// ## Variants
///
/// - `PeerUp`: A new peer has been discovered and is available
/// - `PeerUpdate`: A known peer's metadata has changed
/// - `PeerDown`: A peer has left or become unavailable
pub type DiscoveryEvent {
  /// A new peer has been discovered.
  /// Contains the peer ID and initial metadata.
  PeerUp(peer: PeerId, metadata: PeerMetadata)
  /// A known peer's metadata has been updated.
  /// Contains the peer ID and updated metadata.
  PeerUpdate(peer: PeerId, metadata: PeerMetadata)
  /// A peer has left or become unavailable.
  /// Contains the peer ID and reason for departure.
  PeerDown(peer: PeerId, reason: String)
}

// =============================================================================
// Health & Status
// =============================================================================

/// Health status of a discovery adapter.
///
/// ## Variants
///
/// - `Up`: Adapter is fully operational
/// - `Degraded`: Adapter is working but with issues (e.g., slow responses)
/// - `Down`: Adapter is not operational
pub type HealthStatus {
  /// Adapter is fully operational
  Up
  /// Adapter is operational but degraded
  Degraded(reason: String)
  /// Adapter is down and cannot discover peers
  Down(reason: String)
}

/// Extended health information with sync status.
pub type HealthInfo {
  HealthInfo(
    status: HealthStatus,
    last_sync_time_ms: Option(Int),
    last_error: Option(String),
    known_peers_count: Int,
  )
}

// =============================================================================
// Errors
// =============================================================================

/// Errors that can occur during discovery operations.
///
/// ## Classification
///
/// - **Transient** (retryable): `SyncFailed`, `Timeout`
/// - **Permanent** (do not retry): `InvalidConfiguration`, `PeerNotFound`
pub type DiscoveryError {
  /// Failed to start the discovery adapter
  StartFailed(reason: String)
  /// Failed to stop the discovery adapter
  StopFailed(reason: String)
  /// Shutdown timed out
  ShutdownTimeout(elapsed_ms: Int)
  /// Failed to sync with discovery backend
  SyncFailed(reason: String)
  /// Peer not found in current membership
  PeerNotFound(peer: PeerId)
  /// Invalid adapter configuration
  InvalidConfiguration(reason: String)
  /// Operation timed out
  Timeout(elapsed_ms: Int)
  /// Failed to create/manage subscription
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

// =============================================================================
// Configuration
// =============================================================================

/// Options for starting a discovery adapter.
pub type DiscoveryOptions {
  DiscoveryOptions(
    /// Registered name for the adapter process
    name: String,
    /// How often to poll/sync with discovery backend (milliseconds)
    sync_interval_ms: Int,
    /// Timeout for sync operations (milliseconds)
    sync_timeout_ms: Int,
    /// Prefix for telemetry events
    telemetry_prefix: String,
    /// Initial list of seed peers (for bootstrap)
    seed_peers: List(PeerId),
    /// Adapter-specific custom options
    custom: Dict(String, Dynamic),
  )
}

/// Create default discovery options.
pub fn default_options(name: String) -> DiscoveryOptions {
  DiscoveryOptions(
    name: name,
    sync_interval_ms: 5000,
    sync_timeout_ms: 10_000,
    telemetry_prefix: "discovery",
    seed_peers: [],
    custom: dict.new(),
  )
}

// =============================================================================
// Subscriptions
// =============================================================================

/// Subscription identifier returned by subscribe().
///
/// Use this with `unsubscribe()` to stop receiving events.
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

/// Callback type for discovery events.
pub type EventCallback =
  fn(DiscoveryEvent) -> Nil
