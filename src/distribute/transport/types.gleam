//// Transport layer type definitions.
////
//// This module contains all shared types used by transport adapters. These
//// types form the contract between the adapter implementations and the
//// higher-level transport facade.
////
//// ## Type Categories
////
//// - **Handles** - Opaque references to running adapters
//// - **Status** - Health and subscription state
//// - **Errors** - Structured error types for operations
//// - **Options** - Configuration for send/receive operations
//// - **Metadata** - Information about messages and delivery

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

// =============================================================================
// Handles
// =============================================================================

/// Opaque handle representing an active transport adapter instance.
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
// Health Status
// =============================================================================

/// Health status of a transport adapter.
pub type HealthStatus {
  /// Adapter is fully operational
  Up
  /// Adapter is operational but degraded (e.g., high latency, some peers unreachable)
  Degraded(reason: String)
  /// Adapter is down and cannot send/receive messages
  Down(reason: String)
}

// =============================================================================
// Subscriptions
// =============================================================================

/// Subscription identifier returned by subscribe().
///
/// Use this with `unsubscribe()` to stop receiving messages.
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
// Message Metadata
// =============================================================================

/// Metadata associated with an incoming message.
pub type MessageMetadata {
  MessageMetadata(
    received_at_ms: Int,
    transport_latency_ms: Option(Int),
    peer_address: Option(String),
    correlation_id: Option(String),
  )
}

/// Callback invoked when a message is received.
///
/// Arguments:
/// - `from` - The sender's address or identifier
/// - `payload` - The raw message bytes
/// - `metadata` - Additional delivery information
pub type DeliveryCallback =
  fn(String, BitArray, MessageMetadata) -> Nil

// =============================================================================
// Configuration
// =============================================================================

/// Options for starting an adapter.
pub type AdapterOptions {
  AdapterOptions(
    /// Registered name for the adapter process
    name: String,
    /// Optional bind address for listening
    bind_address: Option(String),
    /// Optional port for network adapters
    port: Option(Int),
    /// Maximum payload size in bytes (default: 10MB)
    max_payload_bytes: Int,
    /// Connection timeout in milliseconds
    connect_timeout_ms: Int,
    /// Prefix for telemetry events
    telemetry_prefix: String,
    /// Adapter-specific custom options
    custom: Dict(String, Dynamic),
  )
}

/// Options for send/broadcast operations.
pub type SendOptions {
  SendOptions(
    /// Operation timeout in milliseconds
    timeout_ms: Option(Int),
    /// Message priority (higher = more urgent)
    priority: Option(Int),
    /// Whether to use reliable delivery
    reliable: Bool,
    /// Correlation ID for request/response tracking
    correlation_id: Option(String),
  )
}

// =============================================================================
// Errors
// =============================================================================

/// Classification of send errors for retry/backoff policies.
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

/// Generic adapter lifecycle errors.
pub type AdapterError {
  /// Failed to start the adapter
  StartFailed(reason: String)
  /// Failed to stop the adapter
  StopFailed(reason: String)
  /// Shutdown timed out
  ShutdownTimeout(elapsed_ms: Int)
  /// Failed to create/manage subscription
  SubscriptionFailed(reason: String)
  /// Invalid configuration provided
  InvalidConfiguration(reason: String)
  /// Resource limit reached
  ResourceExhausted(resource: String)
}

// =============================================================================
// Error Classification
// =============================================================================

/// Check if an error is transient and should be retried.
///
/// Transient errors are temporary conditions that may resolve themselves,
/// such as network blips or temporary overload.
pub fn is_transient_error(error: SendError) -> Bool {
  case error {
    ConnectionClosed(_) -> True
    Backpressure(_) -> True
    Timeout(_) -> True
    _ -> False
  }
}

/// Check if an error is permanent and should not be retried.
///
/// Permanent errors indicate a fundamental problem that won't be fixed
/// by retrying, such as invalid input or configuration.
pub fn is_permanent_error(error: SendError) -> Bool {
  case error {
    InvalidPeer(_) -> True
    PayloadTooLarge(_, _) -> True
    SerializationError(_) -> True
    _ -> False
  }
}
