/// Represents a protocol capability with version range.
/// Used during handshake negotiation to find compatible versions.
pub type Capability {
  Capability(protocol: String, min: Int, max: Int)
}

/// A list of capabilities supported by a node.
pub type NodeCapabilities =
  List(Capability)

/// Create a new capability with protocol name and version range.
pub fn new(protocol: String, min: Int, max: Int) -> Capability {
  Capability(protocol, min, max)
}

/// Create a new capability with protocol name and version range.
@deprecated("Use new() instead")
pub fn make(protocol: String, min: Int, max: Int) -> Capability {
  new(protocol, min, max)
}

/// Get the protocol name from a capability.
pub fn protocol(c: Capability) -> String {
  c.protocol
}

/// Get the minimum supported version from a capability.
pub fn min_version(c: Capability) -> Int {
  c.min
}

/// Get the maximum supported version from a capability.
pub fn max_version(c: Capability) -> Int {
  c.max
}
