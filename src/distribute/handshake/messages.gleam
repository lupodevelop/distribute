//// Handshake protocol message types
////
//// This module defines the message types used during the handshake protocol
//// between nodes in a distributed system. These messages facilitate capability
//// negotiation and connection establishment.
////
//// ## Protocol Flow
////
//// 1. **Initiator** sends `Hello` with node info and capabilities
//// 2. **Responder** replies with `Capabilities` or `Reject`
//// 3. If compatible, **Responder** sends `Accept` with agreed protocol/version
//// 4. Connection is established
////
//// ## Example
////
//// ```gleam
//// let hello = Hello(
////   node_info: "node@host:9000",
////   capabilities: my_capabilities,
//// )
//// ```

import distribute/capability.{type NodeCapabilities}

/// Initial handshake message sent by the initiator.
///
/// Contains the node's identity information and its advertised capabilities.
/// The responder uses this to determine compatibility and decide whether
/// to accept or reject the connection.
pub type Hello {
  Hello(node_info: String, capabilities: NodeCapabilities)
}

/// Capability advertisement message.
///
/// Sent by the responder to share its own capabilities after receiving
/// a `Hello` message. Used during capability negotiation to find
/// a common protocol version and features.
pub type Capabilities {
  Capabilities(capabilities: NodeCapabilities)
}

/// Connection acceptance message.
///
/// Sent when the handshake is successful and both nodes agree on
/// a protocol and version to use for communication.
pub type Accept {
  Accept(protocol: String, version: Int)
}

/// Connection rejection message.
///
/// Sent when the handshake fails, typically due to incompatible
/// capabilities or other errors. Includes a human-readable reason.
pub type Reject {
  Reject(reason: String)
}
