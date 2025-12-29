//// Transport behaviour contract for distributed messaging.
////
//// This module defines the contract that transport adapters must implement
//// to provide network communication in the distribute library. Transport adapters
//// handle sending and receiving binary payloads between nodes in the cluster.
////
//// ## Design Philosophy
////
//// This behaviour follows distribute's patterns:
//// - Pure function signatures (like `crypto/provider`)
//// - Actor-based implementations (like `registry/actor`)  
//// - Integration with `process.Subject(BitArray)` (like `global`, `messaging`)
//// - Capability negotiation support (like `handshake`)
////
//// ## Implementation Approaches
////
//// Adapters can be implemented as:
//// 1. **Actor-based**: Use `gleam/otp/actor` (recommended, see `registry/actor`)
//// 2. **Port/NIF-based**: Wrap external transports
//// 3. **Pure Erlang**: Use Erlang distribution protocol
////
//// ## Integration with distribute
////
//// Transport adapters integrate with:
//// - `handshake` module for capability negotiation
//// - `codec` module for message encoding/decoding
//// - `global` for GlobalSubject message delivery
//// - `messaging` for typed cross-node communication
////
//// ## Example Implementation
////
//// See `examples/adapters/transport_tcp` for a complete implementation.

import distribute/capability
import distribute/codec
import gleam/dict
import gleam/option.{type Option}

/// Opaque handle representing the internal state of a transport adapter.
///
/// Unlike `crypto/provider` which uses simple opaque types, transport adapters
/// typically need more complex state management. Following the `registry/actor`
/// pattern, implementations will usually wrap this in an actor with a command
/// message type.
///
/// Example (actor-based implementation):
/// ```gleam
/// pub type TransportCommand {
///   Send(NodeId, BitArray, Subject(Result(Nil, TransportError)))
///   Broadcast(List(NodeId), BitArray, Subject(Result(Nil, TransportError)))
///   GetHealth(Subject(HealthStatus))
/// }
/// ```
pub type TransportState {
  TransportState
}

/// Node identifier used across the distribute library.
///
/// Matches the `NodeId` from `registry/behaviour` and `handshake` modules.
/// Typically formatted as "node@host" (Erlang node naming convention).
///
/// This type alias ensures consistency across all distribute modules.
pub type NodeId =
  String

/// Transport capabilities for negotiation during handshake.
///
/// Capabilities declare what features the transport supports, allowing
/// nodes to negotiate compatible communication modes during connection setup.
///
/// Examples:
/// - Protocol versions ("tcp", min: 1, max: 2)
/// - Compression support ("compression", min: 1, max: 1)
/// - Encryption requirements ("encryption", min: 1, max: 1)
pub type TransportCapability =
  capability.Capability

/// Metadata associated with a delivered message.
///
/// Transport adapters may include additional information such as:
/// - Timestamps ("received_at", "sent_at")
/// - Routing information ("via_node", "hops")
/// - Transport-specific details ("protocol_version", "compression")
///
/// Keys and values are strings for simplicity and portability across adapters.
pub type DeliveryMetadata =
  dict.Dict(String, String)

/// Message delivered to the receiver subject.
///
/// Contains:
/// - Source node identifier
/// - Binary payload (to be decoded by the receiver)
/// - Delivery metadata
pub type IncomingMessage =
  #(NodeId, BitArray, DeliveryMetadata)

/// Configuration options for transport initialization.
///
/// All fields are optional to allow flexibility in adapter implementations.
///
/// ## Fields
///
/// - `name`: Human-readable identifier for this transport instance
/// - `bind_address`: Network address to bind to (e.g., "0.0.0.0", "127.0.0.1")
/// - `port`: Port number to listen on
/// - `max_payload_bytes`: Maximum message payload size in bytes
/// - `connect_timeout_ms`: Timeout for connection attempts
/// - `heartbeat_interval_ms`: Interval for heartbeat messages (keeps connections alive)
/// - `capabilities`: Transport capabilities for handshake negotiation
pub type TransportOpts {
  TransportOpts(
    name: Option(String),
    bind_address: Option(String),
    port: Option(Int),
    max_payload_bytes: Option(Int),
    connect_timeout_ms: Option(Int),
    heartbeat_interval_ms: Option(Int),
    capabilities: List(TransportCapability),
  )
}

/// Errors that can occur during transport operations.
///
/// ## Variants
///
/// - `AdapterFailure`: Generic adapter failure with error description
/// - `InvalidNode`: The specified node identifier is invalid or unknown
/// - `ConnectionClosed`: The connection to the node was closed
/// - `Backpressure`: The transport is under load and cannot accept more messages
/// - `PayloadTooLarge`: The message payload exceeds size limits
/// - `Timeout`: The operation did not complete within the allowed time
/// - `ShutdownTimeout`: Graceful shutdown did not complete within the timeout
/// - `EncodeError`: Failed to encode the message (wraps codec.EncodeError)
pub type TransportError {
  AdapterFailure(String)
  InvalidNode
  ConnectionClosed
  Backpressure
  PayloadTooLarge
  Timeout
  ShutdownTimeout
  EncodeError(codec.EncodeError)
}

/// Health status of a transport adapter.
///
/// ## Variants
///
/// - `Up`: The transport is fully operational
/// - `Degraded`: The transport is operational but experiencing issues (with description)
/// - `Down`: The transport is not operational (with reason)
///
/// Health checks should be lightweight and fast, suitable for frequent polling
/// by monitoring systems.
pub type HealthStatus {
  Up
  Degraded(String)
  Down(String)
}

/// Initialize a new transport adapter state.
///
/// This is a stub - actual implementations will have their own initialization.
/// Following the `crypto/provider` pattern, this provides a minimal contract.
///
/// Actor-based adapters (recommended) should use `start()` or `start_link()`
/// instead, returning `Result(Subject(Command), actor.StartError)`.
///
/// ## Returns
///
/// A placeholder `TransportState`. Real adapters ignore this.
///
/// ## Example (Actor Pattern - Recommended)
///
/// ```gleam
/// // In your adapter module:
/// pub fn start_link(
///   opts: behaviour.TransportOpts
/// ) -> Result(process.Subject(TransportCommand), actor.StartError) {
///   actor.new(initial_state(opts))
///   |> actor.on_message(handle_message)
///   |> actor.start()
///   |> result.map(fn(s) { s.data })
/// }
/// ```
pub fn init() -> TransportState {
  TransportState
}

/// Send a binary payload to a specific node.
///
/// This is a **pure function signature** following the `crypto/provider` pattern.
/// Actual implementations will vary:
///
/// - **Actor-based** (recommended): Send command to transport actor via Subject
/// - **Synchronous**: Call Erlang/NIF function directly
/// - **Async**: Queue message and return immediately
///
/// The delivery semantics (at-most-once, at-least-once, exactly-once) are
/// adapter-specific and should be documented by each implementation.
///
/// ## Parameters
///
/// - `node`: Identifier of the destination node (e.g., "node@host")
/// - `payload`: The message payload as a binary array
///
/// ## Returns
///
/// `Result(Nil, TransportError)` indicating success or failure
///
/// ## Errors
///
/// - `InvalidNode`: The node identifier is invalid or unknown
/// - `ConnectionClosed`: The connection to the node is closed
/// - `PayloadTooLarge`: The payload exceeds size limits
/// - `Backpressure`: The transport is under load
/// - `Timeout`: The send operation timed out
///
/// ## Example (Actor-based implementation)
///
/// ```gleam
/// pub fn send(
///   transport: Subject(TransportCommand),
///   node: NodeId,
///   payload: BitArray
/// ) -> Result(Nil, TransportError) {
///   let reply_subject = process.new_subject()
///   process.send(transport, Send(node, payload, reply_subject))
///   
///   case process.receive(reply_subject, 5000) {
///     Ok(result) -> result
///     Error(_) -> Error(Timeout)
///   }
/// }
/// ```
pub fn send(node: NodeId, payload: BitArray) -> Result(Nil, TransportError) {
  let _ = node
  let _ = payload
  Error(AdapterFailure("not implemented"))
}

/// Broadcast a binary payload to multiple nodes.
///
/// This is a **pure function signature**. Implementations will vary based on
/// the transport adapter architecture (actor-based, synchronous, async).
///
/// The adapter may optimize this (e.g., multicast) but should ensure best-effort
/// delivery to all reachable nodes.
///
/// ## Parameters
///
/// - `nodes`: List of destination node identifiers  
/// - `payload`: The message payload as a binary array
///
/// ## Returns
///
/// `Result(Nil, TransportError)` indicating success or failure
///
/// ## Errors
///
/// - `PayloadTooLarge`: The payload exceeds size limits
/// - `Backpressure`: The transport is under load
/// - `Timeout`: The broadcast operation timed out
///
/// ## Notes
///
/// Broadcast does not guarantee delivery to all nodes. Partial delivery
/// may occur if some nodes are unreachable. Check individual node connectivity
/// for reliability requirements.
pub fn broadcast(
  nodes: List(NodeId),
  payload: BitArray,
) -> Result(Nil, TransportError) {
  let _ = nodes
  let _ = payload
  Error(AdapterFailure("not implemented"))
}

/// Check the health status of the transport adapter.
///
/// This is a **pure function signature** for health checks.
/// Should be lightweight and suitable for frequent polling by monitoring systems.
///
/// ## Returns
///
/// `HealthStatus` indicating the current health
pub fn health() -> HealthStatus {
  Down("not implemented")
}

/// Retrieve adapter-specific operational metrics.
///
/// This is a **pure function signature** for metrics retrieval.
///
/// ## Returns
///
/// Dictionary of metric names to values
///
/// ## Common Metrics
///
/// Adapters typically include:
/// - `messages_sent`: Total number of messages sent
/// - `messages_received`: Total number of messages received
/// - `bytes_sent`: Total bytes transmitted
/// - `bytes_received`: Total bytes received
/// - `connections_active`: Current number of active connections
/// - `send_errors`: Number of send failures
/// - `recv_errors`: Number of receive failures
pub fn metrics() -> dict.Dict(String, Int) {
  dict.new()
}
