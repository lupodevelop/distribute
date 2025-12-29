// Transport Adapter Behaviour
//
// Defines the contract for transport adapters in the distribute library.
// Based on dev/behaviours/transport.txt specification.
//
// A transport adapter handles low-level sending and receiving of binary 
// payloads between peers or groups, providing lifecycle management,
// health monitoring, and extensibility for different transport mechanisms.

import distribute/transport/types.{
  type AdapterError, type AdapterHandle, type AdapterOptions, type DeliveryCallback,
  type HealthStatus, type SendError, type SendOptions, type SubscriptionId,
}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option

/// Transport adapter behaviour contract.
/// 
/// All transport implementations must provide these 8 core functions:
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
    unsubscribe: fn(AdapterHandle, SubscriptionId) ->
      Result(Nil, AdapterError),
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
/// Provides sensible defaults:
/// - timeout_ms: None (use adapter default)
/// - priority: None (normal priority)
/// - reliable: False (best-effort delivery)
/// - correlation_id: None
pub fn default_send_options() -> SendOptions {
  types.SendOptions(
    timeout_ms: option.None,
    priority: option.None,
    reliable: False,
    correlation_id: option.None,
  )
}

// Lifecycle Semantics
// ===================
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
// Error Classification and Retry Policy
// ======================================
//
// Transport errors must be classified for upper layers to choose strategy:
//
// - Transient (retryable with backoff):
//   ConnectionClosed, Timeout, Backpressure
//
// - Permanent (do not retry):
//   InvalidPeer, PayloadTooLarge, SerializationError
//
// - AdapterFailure:
//   Internal adapter error; escalate to registry for marking unhealthy
//
// Hot-reload and Replacement
// ===========================
//
// The registry must be able to stop an adapter and start a replacement
// with minimal interruption:
//
// - start() must be idempotent for new handle instances
// - stop() must release network bindings and background tasks cleanly
// - health() should report readiness status after start()
//
// Observability and Logging
// ==========================
//
// Adapters should emit structured events for:
// - adapter_started, adapter_stopped
// - adapter_error
// - peer_connected, peer_disconnected
// - message_sent, message_received
//
// Events should include context: adapter_name, node_id, peer_id,
// correlation_id (when available)
//
// Security Considerations
// ========================
//
// - Do not create atoms from untrusted input
// - Enforce max_payload_bytes limits
// - Document encryption/authentication capabilities
// - Document key management if applicable
//
// Testing and Conformance
// ========================
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
