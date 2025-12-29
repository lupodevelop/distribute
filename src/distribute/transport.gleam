//// High-level transport API for the distribute library.
////
//// This module provides a unified facade for the transport layer, managing
//// the default BEAM distribution adapter as a singleton. Other modules
//// (messaging, handshake) can use these functions without explicitly
//// passing adapter handles.
////
//// ## Architecture
////
//// The transport layer is designed as a pluggable system:
////
//// - `transport/types` - Shared type definitions
//// - `transport/behaviour` - Adapter contract (behaviour)
//// - `transport/adapter` - Adapter utilities and defaults
//// - `transport/beam_adapter` - BEAM distribution implementation
//// - `transport` (this module) - High-level singleton facade
////
//// ## Usage
////
//// Start the transport as part of your supervision tree:
////
//// ```gleam
//// import distribute/transport
//// import gleam/otp/static_supervisor as supervisor
////
//// pub fn start_app() {
////   supervisor.new(supervisor.OneForOne)
////   |> supervisor.add(transport.child_spec())
////   |> supervisor.start()
//// }
//// ```
////
//// Then use the transport functions:
////
//// ```gleam
//// // Send a message
//// transport.send("my_process", <<1, 2, 3>>, adapter.default_send_options())
////
//// // Check health
//// case transport.health() {
////   types.Up -> io.println("Transport is healthy")
////   types.Degraded(reason) -> io.println("Degraded: " <> reason)
////   types.Down(reason) -> io.println("Down: " <> reason)
//// }
//// ```

import distribute/transport/adapter
import distribute/transport/beam_adapter
import distribute/transport/types.{
  type AdapterError, type AdapterHandle, type DeliveryCallback,
  type HealthStatus, type SendError, type SendOptions, type SubscriptionId,
}
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

/// The registered name for the default transport adapter process.
///
/// This name is used to look up the singleton adapter instance. You typically
/// don't need to use this directly - the functions in this module handle
/// the lookup automatically.
pub const default_adapter_name = "distribute_transport_adapter"

// =============================================================================
// Lifecycle
// =============================================================================

/// Create a child specification for starting the transport adapter under
/// a supervisor.
///
/// This is the recommended way to start the transport layer. The adapter
/// will be automatically restarted if it crashes.
///
/// ## Example
///
/// ```gleam
/// import distribute/transport
/// import gleam/otp/static_supervisor as supervisor
///
/// supervisor.new(supervisor.OneForOne)
/// |> supervisor.add(transport.child_spec())
/// |> supervisor.start()
/// ```
pub fn child_spec() -> ChildSpecification(AdapterHandle) {
  let opts =
    adapter.default_options(default_adapter_name)
    |> fn(o) { types.AdapterOptions(..o, name: default_adapter_name) }

  beam_adapter.child_spec(opts)
}

/// Start the transport adapter directly without supervision.
///
/// **Note:** For production use, prefer `child_spec()` with a supervisor.
/// This function is primarily useful for testing or simple scripts.
///
/// Returns the adapter handle on success, or an error if startup fails.
pub fn start_link() -> Result(AdapterHandle, AdapterError) {
  let opts =
    adapter.default_options(default_adapter_name)
    |> fn(o) { types.AdapterOptions(..o, name: default_adapter_name) }

  beam_adapter.new().start(opts)
}

// =============================================================================
// Messaging
// =============================================================================

/// Send a message to a peer using the default transport.
///
/// The peer can be a local registered process name (e.g., `"my_process"`)
/// or a remote process (`"my_process@node"`).
///
/// ## Example
///
/// ```gleam
/// let payload = <<"hello">>
/// let opts = adapter.default_send_options()
/// transport.send("calculator", payload, opts)
/// ```
pub fn send(
  peer: String,
  payload: BitArray,
  opts: SendOptions,
) -> Result(Nil, SendError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) { types.AdapterFailure("Transport not running") }),
  )
  beam_adapter.new().send(handle, peer, payload, opts)
}

/// Broadcast a message to all members of a group.
///
/// Group membership is managed by the adapter. The special group `"all"`
/// broadcasts to all known peers.
///
/// ## Example
///
/// ```gleam
/// transport.broadcast("workers", payload, opts)
/// ```
pub fn broadcast(
  group: String,
  payload: BitArray,
  opts: SendOptions,
) -> Result(Nil, SendError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) { types.AdapterFailure("Transport not running") }),
  )
  beam_adapter.new().broadcast(handle, group, payload, opts)
}

// =============================================================================
// Subscriptions
// =============================================================================

/// Subscribe to incoming messages on the default transport.
///
/// The callback will be invoked for each message received by the adapter.
/// Returns a subscription ID that can be used to unsubscribe later.
///
/// ## Example
///
/// ```gleam
/// let callback = fn(from, payload) {
///   io.println("Received from: " <> from)
/// }
/// let assert Ok(sub_id) = transport.subscribe(callback)
/// ```
pub fn subscribe(
  callback: DeliveryCallback,
) -> Result(SubscriptionId, AdapterError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) {
      types.SubscriptionFailed("Transport not running")
    }),
  )
  beam_adapter.new().subscribe(handle, callback)
}

/// Unsubscribe from the default transport.
///
/// After calling this function, the callback associated with the given
/// subscription ID will no longer receive messages.
pub fn unsubscribe(id: SubscriptionId) -> Result(Nil, AdapterError) {
  use handle <- result.try(
    get_handle()
    |> result.map_error(fn(_) {
      types.SubscriptionFailed("Transport not running")
    }),
  )
  beam_adapter.new().unsubscribe(handle, id)
}

// =============================================================================
// Health & Diagnostics
// =============================================================================

/// Get the health status of the default transport.
///
/// Returns:
/// - `Up` - Transport is fully operational
/// - `Degraded(reason)` - Transport is working but with issues
/// - `Down(reason)` - Transport is not operational
///
/// ## Example
///
/// ```gleam
/// case transport.health() {
///   types.Up -> io.println("All good!")
///   types.Degraded(r) -> io.println("Warning: " <> r)
///   types.Down(r) -> io.println("Error: " <> r)
/// }
/// ```
pub fn health() -> HealthStatus {
  case get_handle() {
    Ok(handle) -> beam_adapter.new().health(handle)
    Error(_) -> types.Down("Transport process not found")
  }
}

// =============================================================================
// Internal
// =============================================================================

/// Get the handle for the default transport adapter.
fn get_handle() -> Result(AdapterHandle, Nil) {
  beam_adapter.get_handle(default_adapter_name)
}
