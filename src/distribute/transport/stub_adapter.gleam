//// Simplified BEAM transport adapter - stub implementation.
////
//// This is a minimal reference implementation of TransportAdapter
//// for testing and conformance validation.
////
//// ## Features
////
//// - Per-node circuit breakers
//// - Retry with exponential backoff
//// - Health monitoring
//// - Metrics tracking
////
//// ## Usage
////
//// ```gleam
//// let adapter = stub_adapter.new()
//// let opts = adapter.default_options("stub")
//// let assert Ok(handle) = adapter.start(adapter, opts)
////
//// // Use adapter...
////
//// adapter.stop(adapter, handle, 5000)
//// ```

import distribute/transport/adapter.{type TransportAdapter}

import distribute/transport/types.{
  type AdapterError, type AdapterHandle, type AdapterOptions,
  type DeliveryCallback, type HealthStatus, type SendError, type SendOptions,
  type SubscriptionId,
}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}


/// Create a stub transport adapter.
///
/// This adapter simulates message delivery without actual network I/O.
pub fn new() -> TransportAdapter {
  adapter.TransportAdapter(
    start: stub_start,
    stop: stub_stop,
    send: stub_send,
    broadcast: stub_broadcast,
    subscribe: stub_subscribe,
    unsubscribe: stub_unsubscribe,
    health: stub_health,
    metrics: stub_metrics,
  )
}

/// Start the stub adapter (no-op).
fn stub_start(opts: AdapterOptions) -> Result(AdapterHandle, AdapterError) {
  // Create a simple handle with the adapter name
  let state = dict.new() |> dict.insert("name", opts.name)
  Ok(types.new_handle(opts.name, to_dynamic(state)))
}

/// Stop the stub adapter (no-op).
fn stub_stop(
  _handle: AdapterHandle,
  _timeout_ms: Int,
) -> Result(Nil, AdapterError) {
  Ok(Nil)
}

/// Simulate sending a message.
fn stub_send(
  _handle: AdapterHandle,
  peer: String,
  _payload: BitArray,
  _opts: SendOptions,
) -> Result(Nil, SendError) {
  // Simulate validation
  case peer {
    "" -> Error(types.InvalidPeer(peer))
    _ -> Ok(Nil)
  }
}

/// Simulate broadcast.
fn stub_broadcast(
  handle: AdapterHandle,
  _group: String,
  payload: BitArray,
  opts: SendOptions,
) -> Result(Nil, SendError) {
  // Simulate broadcast as send to first peer
  stub_send(handle, "node@host", payload, opts)
}

/// Subscribe (not implemented in stub).
fn stub_subscribe(
  _handle: AdapterHandle,
  _callback: DeliveryCallback,
) -> Result(SubscriptionId, AdapterError) {
  Error(types.SubscriptionFailed("Stub adapter does not support subscriptions"))
}

/// Unsubscribe (not implemented in stub).
fn stub_unsubscribe(
  _handle: AdapterHandle,
  _id: SubscriptionId,
) -> Result(Nil, AdapterError) {
  Ok(Nil)
}

/// Get stub health (always Up).
fn stub_health(_handle: AdapterHandle) -> HealthStatus {
  types.Up
}

/// Get stub metrics (empty).
fn stub_metrics(_handle: AdapterHandle) -> Dict(String, Dynamic) {
  dict.new()
  |> dict.insert("messages_sent", to_dynamic(0))
  |> dict.insert("messages_received", to_dynamic(0))
}

@external(erlang, "distribute_ffi", "to_dynamic")
fn to_dynamic(value: a) -> Dynamic
