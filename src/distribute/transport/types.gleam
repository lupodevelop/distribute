// Transport Types
// 
// Shared types for transport adapters based on behaviour contract.

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

/// Opaque handle representing an active transport adapter instance.
/// Created by start() and used in all subsequent operations.
pub opaque type AdapterHandle {
  AdapterHandle(id: String, state: Dynamic)
}

/// Create a new adapter handle with an identifier and internal state
pub fn new_handle(id: String, state: Dynamic) -> AdapterHandle {
  AdapterHandle(id: id, state: state)
}

/// Get the handle's identifier
pub fn handle_id(handle: AdapterHandle) -> String {
  handle.id
}

/// Get the handle's internal state
pub fn handle_state(handle: AdapterHandle) -> Dynamic {
  handle.state
}

/// Health status of a transport adapter
pub type HealthStatus {
  /// Adapter is fully operational
  Up
  /// Adapter is operational but degraded (e.g., high latency, some peers unreachable)
  Degraded(reason: String)
  /// Adapter is down and cannot send/receive messages
  Down(reason: String)
}

/// Subscription identifier returned by subscribe()
pub opaque type SubscriptionId {
  SubscriptionId(String)
}

pub fn new_subscription_id(id: String) -> SubscriptionId {
  SubscriptionId(id)
}

pub fn subscription_id_value(id: SubscriptionId) -> String {
  let SubscriptionId(value) = id
  value
}

/// Metadata associated with an incoming message
pub type MessageMetadata {
  MessageMetadata(
    received_at_ms: Int,
    transport_latency_ms: Option(Int),
    peer_address: Option(String),
    correlation_id: Option(String),
  )
}

/// Callback invoked when a message is received
pub type DeliveryCallback =
  fn(String, BitArray, MessageMetadata) -> Nil

/// Options for starting an adapter
pub type AdapterOptions {
  AdapterOptions(
    name: String,
    bind_address: Option(String),
    port: Option(Int),
    max_payload_bytes: Int,
    connect_timeout_ms: Int,
    telemetry_prefix: String,
    custom: Dict(String, Dynamic),
  )
}

/// Options for send/broadcast operations
pub type SendOptions {
  SendOptions(
    timeout_ms: Option(Int),
    priority: Option(Int),
    reliable: Bool,
    correlation_id: Option(String),
  )
}

/// Classification of send errors for retry/backoff policies
pub type SendError {
  /// Peer identifier is invalid or unknown
  InvalidPeer(peer: String)
  /// Failed to serialize payload
  SerializationError(reason: String)
  /// Connection to peer is closed
  ConnectionClosed(peer: String)
  /// Transport experiencing backpressure
  Backpressure(queue_size: Int)
  /// Payload exceeds maximum allowed size
  PayloadTooLarge(size: Int, max: Int)
  /// Operation timed out
  Timeout(elapsed_ms: Int)
  /// Internal adapter failure
  AdapterFailure(reason: String)
}

/// Generic adapter error
pub type AdapterError {
  StartFailed(reason: String)
  StopFailed(reason: String)
  ShutdownTimeout(elapsed_ms: Int)
  SubscriptionFailed(reason: String)
  InvalidConfiguration(reason: String)
  ResourceExhausted(resource: String)
}

/// Is this a transient error that should be retried?
pub fn is_transient_error(error: SendError) -> Bool {
  case error {
    ConnectionClosed(_) -> True
    Backpressure(_) -> True
    Timeout(_) -> True
    _ -> False
  }
}

/// Is this a permanent error that should not be retried?
pub fn is_permanent_error(error: SendError) -> Bool {
  case error {
    InvalidPeer(_) -> True
    PayloadTooLarge(_, _) -> True
    SerializationError(_) -> True
    _ -> False
  }
}
