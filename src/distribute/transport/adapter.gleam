//// Transport Adapter contract and utilities.
////
//// This module defines the behaviour contract that all transport adapters
//// must implement, along with helper functions for creating default options.
////
//// ## Adapter Contract
////
//// A transport adapter handles low-level sending and receiving of binary
//// payloads between peers or groups. It provides:
////
//// - Lifecycle management (start/stop)
//// - Unicast and broadcast messaging
//// - Subscription-based message reception
//// - Health monitoring and metrics
////
//// ## Implementing an Adapter
////
//// To implement a custom adapter, create a module that provides a function
//// returning a `TransportAdapter` record with all required functions:
////
//// ```gleam
//// pub fn new() -> TransportAdapter {
////   TransportAdapter(
////     start: my_start,
////     stop: my_stop,
////     send: my_send,
////     broadcast: my_broadcast,
////     subscribe: my_subscribe,
////     unsubscribe: my_unsubscribe,
////     health: my_health,
////     metrics: my_metrics,
////   )
//// }
//// ```
////
//// See `distribute/transport/beam_adapter` for a reference implementation.

import distribute/transport/types.{
  type AdapterError, type AdapterHandle, type AdapterOptions,
  type DeliveryCallback, type HealthStatus, type SendError, type SendOptions,
  type SubscriptionId,
}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option

// =============================================================================
// Behaviour Contract
// =============================================================================

/// Transport adapter behaviour contract.
///
/// All transport implementations must provide these 8 core functions.
/// 1. start - Initialize and start the adapter
/// 2. stop - Graceful shutdown with timeout
/// 3. send - Unicast to a single peer
/// 4. broadcast - Multicast to a group
/// 5. subscribe - Register callback for incoming messages
/// 6. unsubscribe - Remove a subscription
/// 7. health - Get current adapter health status
/// 8. metrics - Get adapter-specific metrics
pub type TransportAdapter {
  TransportAdapter(
    /// Start the adapter with provided options.
    /// Returns an opaque handle for subsequent operations.
    /// 
    /// Errors are fatal start errors that prevent adapter initialization.
    start: fn(AdapterOptions) -> Result(AdapterHandle, AdapterError),
    /// Stop the adapter gracefully within the timeout period (milliseconds).
    /// 
    /// The adapter should attempt to flush in-flight messages and close
    /// connections cleanly. If graceful shutdown cannot complete within
    /// the timeout, the adapter should abort and return ShutdownTimeout error.
    stop: fn(AdapterHandle, Int) -> Result(Nil, AdapterError),
    /// Send a binary payload to a single peer (unicast).
    /// 
    /// Peer is an opaque identifier from the registry/discovery subsystem.
    /// This function should return quickly; implementations may queue sends
    /// asynchronously. Ok(Nil) indicates queuing success (not delivery).
    /// 
    /// SendError classifications:
    /// - Transient (retryable): ConnectionClosed, Timeout, Backpressure
    /// - Permanent (do not retry): InvalidPeer, PayloadTooLarge, SerializationError
    /// - AdapterFailure: Internal error, escalate to registry
    send: fn(AdapterHandle, String, BitArray, SendOptions) ->
      Result(Nil, SendError),
    /// Broadcast a binary payload to a logical group (multicast).
    /// 
    /// Group is defined by the groups subsystem. Semantics are best-effort
    /// unless the adapter explicitly supports acknowledged broadcasts.
    /// 
    /// Implementations should document delivery guarantees.
    broadcast: fn(AdapterHandle, String, BitArray, SendOptions) ->
      Result(Nil, SendError),
    /// Subscribe to incoming messages.
    /// 
    /// Register a callback to receive (peer_id, payload, metadata) tuples.
    /// Returns a subscription ID for later unsubscription.
    /// Adapters can support multiple subscribers.
    subscribe: fn(AdapterHandle, DeliveryCallback) ->
      Result(SubscriptionId, AdapterError),
    /// Unsubscribe from incoming messages.
    /// 
    /// Remove a previously registered subscription by ID.
    unsubscribe: fn(AdapterHandle, SubscriptionId) -> Result(Nil, AdapterError),
    /// Get current health status.
    /// 
    /// Returns lightweight snapshot: Up, Degraded(reason), or Down(reason).
    /// Should include basic diagnostics without heavy computation.
    health: fn(AdapterHandle) -> HealthStatus,
    /// Get adapter-specific metrics.
    /// 
    /// Returns a map of metrics for observability integration.
    /// Common metrics: message_sent_count, message_received_count,
    /// error_count, last_error_timestamp_ms, active_connections.
    metrics: fn(AdapterHandle) -> Dict(String, Dynamic),
  )
}

/// Create default adapter options.
/// 
/// Provides sensible defaults:
/// - max_payload_bytes: 1MB
/// - connect_timeout_ms: 5000
/// - telemetry_prefix: "transport"
pub fn default_options(name: String) -> AdapterOptions {
  types.AdapterOptions(
    name: name,
    bind_address: option.None,
    port: option.None,
    max_payload_bytes: 1_048_576,
    connect_timeout_ms: 5000,
    telemetry_prefix: "transport",
    custom: dict.new(),
  )
}

/// Create default send options.
///
/// Returns options suitable for most use cases:
/// - No explicit timeout (adapter default applies)
/// - Normal priority
/// - Best-effort delivery
/// - No correlation ID
pub fn default_send_options() -> SendOptions {
  types.SendOptions(
    timeout_ms: option.None,
    priority: option.None,
    reliable: False,
    correlation_id: option.None,
  )
}

// =============================================================================
// Implementation Notes
// =============================================================================
//
// Lifecycle Semantics
// -------------------
//
// 1. start() must allocate resources and begin listening/connecting.
//    Failure to start should be immediately reported.
//
// 2. stop() must attempt graceful shutdown. If timeout elapses,
//    abort and free resources but report timeout error.
//
// 3. Adapters must avoid blocking the runtime thread; use background
//    workers for I/O operations.
//
// Error Classification
// --------------------
//
// Transport errors are classified for retry policy decisions:
//
// - Transient (retryable with backoff):
//   ConnectionClosed, Timeout, Backpressure
//
// - Permanent (do not retry):
//   InvalidPeer, PayloadTooLarge, SerializationError
//
// - AdapterFailure:
//   Internal adapter error; escalate to registry
//
// Observability
// -------------
//
// Adapters should emit structured events for:
// - adapter_started, adapter_stopped
// - adapter_error
// - peer_connected, peer_disconnected
// - message_sent, message_received
//
// Security Considerations
// -----------------------
//
// - Do not create atoms from untrusted input
// - Enforce max_payload_bytes limits
// - Document encryption/authentication capabilities
//
// All adapters should pass the standard conformance test suite:
// - start/stop lifecycle
// - single send/receive between two adapters
// - broadcast semantics validation
// - invalid peer handling
// - simulated backpressure and timeout
// - graceful shutdown under load
//
// See test/transport_conformance_test.gleam for reference.
