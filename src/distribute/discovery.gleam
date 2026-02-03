//// High-level discovery API for the distribute library.
////
//// This module provides a unified facade for the discovery layer, managing
//// the default BEAM discovery adapter as a singleton. Other modules
//// can use these functions without explicitly passing adapter handles.
////
//// ## Architecture
////
//// The discovery layer is designed as a pluggable system:
////
//// - `discovery/types` - Shared type definitions
//// - `discovery/behaviour` - Adapter contract (behaviour)
//// - `discovery/adapter` - Adapter utilities and defaults
//// - `discovery/beam_adapter` - BEAM node monitoring implementation
//// - `discovery` (this module) - High-level singleton facade
////
//// ## Usage
////
//// Start the discovery as part of your supervision tree:
////
//// ```gleam
//// import distribute/discovery
//// import gleam/otp/static_supervisor as supervisor
////
//// pub fn start_app() {
////   supervisor.new(supervisor.OneForOne)
////   |> supervisor.add(discovery.child_spec())
////   |> supervisor.start()
//// }
//// ```
////
//// Then use the discovery functions:
////
//// ```gleam
//// // Subscribe to peer events
//// let callback = fn(event) {
////   case event {
////     discovery.PeerUp(peer, _meta) -> io.println("Peer joined: " <> peer)
////     discovery.PeerDown(peer, _reason) -> io.println("Peer left: " <> peer)
////     _ -> Nil
////   }
//// }
//// let assert Ok(sub_id) = discovery.subscribe(callback)
////
//// // Get current peers
//// let assert Ok(peers) = discovery.snapshot()
////
//// // Check health
//// case discovery.health() {
////   discovery.Up -> io.println("Discovery is healthy")
////   discovery.Degraded(reason) -> io.println("Degraded: " <> reason)
////   discovery.Down(reason) -> io.println("Down: " <> reason)
//// }
//// ```

import distribute/discovery/adapter
import distribute/discovery/beam_adapter
import distribute/discovery/types.{
  type AdapterHandle, type DiscoveryError, type EventCallback, type HealthStatus,
  type PeerInfo, type SubscriptionId,
}
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

/// The registered name for the default discovery adapter process.
///
/// This name is used to look up the singleton adapter instance. You typically
/// don't need to use this directly - the functions in this module handle
/// the lookup automatically.
pub const default_adapter_name = "distribute_discovery_adapter"

// =============================================================================
// Lifecycle
// =============================================================================

/// Create a child specification for starting the discovery adapter under
/// a supervisor.
///
/// This is the recommended way to start the discovery layer. The adapter
/// will be automatically restarted if it crashes.
///
/// ## Example
///
/// ```gleam
/// import distribute/discovery
/// import gleam/otp/static_supervisor as supervisor
///
/// supervisor.new(supervisor.OneForOne)
/// |> supervisor.add(discovery.child_spec())
/// |> supervisor.start()
/// ```
pub fn child_spec() -> ChildSpecification(AdapterHandle) {
  let opts =
    adapter.default_options(default_adapter_name)
    |> fn(o) { types.AdapterOptions(..o, name: default_adapter_name) }

  beam_adapter.child_spec(opts)
}

/// Start the discovery adapter directly without supervision.
///
/// **Note:** For production use, prefer `child_spec()` with a supervisor.
/// This function is primarily useful for testing or simple scripts.
///
/// Returns the adapter handle on success, or an error if startup fails.
pub fn start_link() -> Result(AdapterHandle, DiscoveryError) {
  let opts =
    adapter.default_options(default_adapter_name)
    |> fn(o) { types.AdapterOptions(..o, name: default_adapter_name) }

  beam_adapter.new().start(opts)
}

// =============================================================================
// Subscriptions
// =============================================================================

/// Subscribe to peer membership events.
///
/// The callback will be invoked for each peer event (up, down, update).
/// Returns a subscription ID that can be used to unsubscribe later.
///
/// ## Example
///
/// ```gleam
/// let callback = fn(event) {
///   case event {
///     types.PeerUp(peer, meta) -> 
///       io.println("New peer: " <> peer)
///     types.PeerDown(peer, reason) -> 
///       io.println("Peer left: " <> peer)
///     types.PeerUpdate(peer, meta) -> 
///       io.println("Peer updated: " <> peer)
///   }
/// }
/// let assert Ok(sub_id) = discovery.subscribe(callback)
/// ```
pub fn subscribe(
  callback: EventCallback,
) -> Result(SubscriptionId, DiscoveryError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) {
      types.SubscriptionFailed("Discovery not running")
    }),
  )
  beam_adapter.new().subscribe(handle, callback)
}

/// Unsubscribe from peer events.
///
/// After calling this function, the callback associated with the given
/// subscription ID will no longer receive events.
pub fn unsubscribe(id: SubscriptionId) -> Result(Nil, DiscoveryError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) {
      types.SubscriptionFailed("Discovery not running")
    }),
  )
  beam_adapter.new().unsubscribe(handle, id)
}

// =============================================================================
// Queries
// =============================================================================

/// Get a snapshot of all currently known peers.
///
/// Returns a list of peers with their metadata at the current point in time.
/// This is a consistent snapshot - peers won't change during iteration.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(peers) = discovery.snapshot()
/// list.each(peers, fn(peer) {
///   io.println("Peer: " <> peer.id)
/// })
/// ```
pub fn snapshot() -> Result(List(PeerInfo), DiscoveryError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) { types.SyncFailed("Discovery not running") }),
  )
  beam_adapter.new().snapshot(handle)
}

/// Lookup a specific peer by ID.
///
/// Returns the peer's info if known, or `PeerNotFound` error otherwise.
///
/// ## Example
///
/// ```gleam
/// case discovery.lookup("node2@host") {
///   Ok(peer) -> io.println("Found peer with metadata")
///   Error(types.PeerNotFound(_)) -> io.println("Peer not found")
///   Error(_) -> io.println("Discovery error")
/// }
/// ```
pub fn lookup(peer_id: String) -> Result(PeerInfo, DiscoveryError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) { types.SyncFailed("Discovery not running") }),
  )
  beam_adapter.new().lookup(handle, peer_id)
}

// =============================================================================
// Health & Diagnostics
// =============================================================================

/// Get the health status of the discovery adapter.
///
/// Returns:
/// - `Up` - Discovery is fully operational
/// - `Degraded(reason)` - Discovery is working but with issues
/// - `Down(reason)` - Discovery is not operational
///
/// ## Example
///
/// ```gleam
/// case discovery.health() {
///   types.Up -> io.println("All good!")
///   types.Degraded(r) -> io.println("Warning: " <> r)
///   types.Down(r) -> io.println("Error: " <> r)
/// }
/// ```
pub fn health() -> HealthStatus {
  case get_handle() {
    Ok(handle) -> beam_adapter.new().health(handle)
    Error(_) -> types.Down("Discovery process not found")
  }
}

// =============================================================================
// Internal
// =============================================================================

/// Get the handle for the default discovery adapter.
fn get_handle() -> Result(AdapterHandle, Nil) {
  beam_adapter.get_handle(default_adapter_name)
}
