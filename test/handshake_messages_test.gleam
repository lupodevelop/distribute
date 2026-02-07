import distribute/capability.{Capability}
import distribute/handshake/messages.{Accept, Capabilities, Hello, Reject}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Hello message
// ============================================================================

pub fn hello_construction_test() {
  let caps = [Capability("proto_a", 1, 3, meta: [])]
  let hello = Hello(node_info: "node1@host:9000", capabilities: caps)
  should.equal(hello.node_info, "node1@host:9000")
  should.equal(hello.capabilities, caps)
}

pub fn hello_empty_caps_test() {
  let hello = Hello(node_info: "node@local", capabilities: [])
  should.equal(hello.capabilities, [])
}

// ============================================================================
// Capabilities message
// ============================================================================

pub fn capabilities_construction_test() {
  let caps = [
    Capability("proto_a", 1, 5, meta: []),
    Capability("proto_b", 2, 3, meta: []),
  ]
  let msg = Capabilities(capabilities: caps)
  should.equal(msg.capabilities, caps)
}

// ============================================================================
// Accept message
// ============================================================================

pub fn accept_construction_test() {
  let msg = Accept(protocol: "proto_a", version: 2)
  should.equal(msg.protocol, "proto_a")
  should.equal(msg.version, 2)
}

// ============================================================================
// Reject message
// ============================================================================

pub fn reject_construction_test() {
  let msg = Reject(reason: "incompatible protocols")
  should.equal(msg.reason, "incompatible protocols")
}

pub fn reject_empty_reason_test() {
  let msg = Reject(reason: "")
  should.equal(msg.reason, "")
}
