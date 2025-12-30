//// Discovery Adapter contract and utilities.
////
//// This module defines the behaviour contract that all discovery adapters
//// must implement, along with helper functions for creating default options.
////
//// ## Adapter Contract
////
//// A discovery adapter handles peer discovery and membership tracking.
//// It provides:
////
//// - Lifecycle management (start/stop)
//// - Event-driven membership updates (subscribe/unsubscribe)
//// - Membership queries (snapshot/lookup)
//// - Health monitoring
////
//// ## Implementing an Adapter
////
//// To implement a custom adapter, create a module that provides a function
//// returning a `DiscoveryAdapter` record with all required functions:
////
//// ```gleam
//// pub fn new() -> DiscoveryAdapter {
////   DiscoveryAdapter(
////     start: my_start,
////     stop: my_stop,
////     subscribe: my_subscribe,
////     unsubscribe: my_unsubscribe,
////     snapshot: my_snapshot,
////     lookup: my_lookup,
////     health: my_health,
////   )
//// }
//// ```
////
//// See `distribute/discovery/beam_adapter` for a reference implementation.

import distribute/discovery/types.{
  type AdapterHandle, type AdapterOptions, type DiscoveryError,
  type EventCallback, type HealthStatus, type PeerId, type PeerInfo,
  type SubscriptionId,
}
import gleam/dict
import gleam/option

// =============================================================================
// Behaviour Contract
// =============================================================================

/// Discovery adapter behaviour contract.
///
/// All discovery implementations must provide these 7 core functions.
/// 1. start - Initialize and start the adapter
/// 2. stop - Graceful shutdown with timeout
/// 3. subscribe - Register callback for membership events
/// 4. unsubscribe - Remove a subscription
/// 5. snapshot - Get current membership state
/// 6. lookup - Get metadata for a specific peer
/// 7. health - Get current adapter health status
pub type DiscoveryAdapter {
  DiscoveryAdapter(
    /// Start the adapter with provided options.
    /// Returns an opaque handle for subsequent operations.
    start: fn(AdapterOptions) -> Result(AdapterHandle, DiscoveryError),
    /// Stop the adapter gracefully within the timeout period (milliseconds).
    /// 
    /// The adapter should cancel background tasks and ensure no further
    /// events are delivered after completion.
    stop: fn(AdapterHandle, Int) -> Result(Nil, DiscoveryError),
    /// Subscribe to membership events.
    /// 
    /// Register a callback to receive PeerUp, PeerUpdate, and PeerDown events.
    /// Callbacks must execute quickly; heavy processing should be async.
    subscribe: fn(AdapterHandle, EventCallback) ->
      Result(SubscriptionId, DiscoveryError),
    /// Unsubscribe from membership events.
    unsubscribe: fn(AdapterHandle, SubscriptionId) ->
      Result(Nil, DiscoveryError),
    /// Get a consistent snapshot of currently known peers.
    /// 
    /// Returns a list of all peers with their current metadata.
    snapshot: fn(AdapterHandle) -> Result(List(PeerInfo), DiscoveryError),
    /// Lookup metadata for a specific peer.
    /// 
    /// Returns the peer's metadata if known, PeerNotFound otherwise.
    lookup: fn(AdapterHandle, PeerId) -> Result(PeerInfo, DiscoveryError),
    /// Get current health status.
    /// 
    /// Returns lightweight snapshot: Up, Degraded(reason), or Down(reason).
    health: fn(AdapterHandle) -> HealthStatus,
    /// Get adapter metrics for observability.
    ///
    /// Returns current metrics snapshot including sync stats and error rates.
    metrics: fn(AdapterHandle) -> types.DiscoveryMetrics,
  )
}

/// Create default adapter options.
/// 
/// Provides sensible defaults:
/// - sync_interval_ms: 5000 (5 seconds)
/// - sync_timeout_ms: 10000 (10 seconds)
/// - telemetry_prefix: "discovery"
/// - retry_config: 3 attempts with exponential backoff
pub fn default_options(name: String) -> AdapterOptions {
  types.AdapterOptions(
    name: name,
    sync_interval_ms: 5000,
    sync_timeout_ms: 10_000,
    telemetry_prefix: "discovery",
    seed_peers: [],
    retry_config: types.default_retry_config(),
    on_peer_up: option.None,
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
// 1. start() begins background reconciliation and event streaming.
//    Failure to start should be immediately reported.
//
// 2. stop() must cancel background tasks and ensure no events
//    are delivered after completion.
//
// Event Delivery
// --------------
//
// - Events are delivered asynchronously to subscribers.
// - Callbacks must be non-blocking.
// - Adapters should buffer events during heavy load.
//
// Error Classification
// --------------------
//
// - Transient (retryable with backoff):
//   SyncFailed, Timeout
//
// - Permanent (do not retry):
//   InvalidConfiguration, PeerNotFound
//
// Observability
// -------------
//
// Adapters should emit structured events for:
// - discovery_sync_success, discovery_sync_failure
// - peer_event_processed
// - adapter_error
//
// Security Considerations
// -----------------------
//
// - Validate and sanitize metadata from discovery backends
// - Do not create atoms from untrusted input
// - Respect RBAC and credential rotation
