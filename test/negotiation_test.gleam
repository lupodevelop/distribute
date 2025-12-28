import distribute/capability.{Capability}
import distribute/handshake/negotiation
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// protocol_negotiate tests
// ============================================================================

pub fn negotiate_compatible_ranges_test() {
  let local = [Capability("proto_a", 1, 3)]
  let remote = [Capability("proto_a", 2, 5)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(Some(3))
  // Overlap is [2, 3], max is 3
}

pub fn negotiate_exact_match_test() {
  let local = [Capability("proto_a", 2, 2)]
  let remote = [Capability("proto_a", 2, 2)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(Some(2))
}

pub fn negotiate_no_overlap_test() {
  let local = [Capability("proto_a", 1, 2)]
  let remote = [Capability("proto_a", 3, 5)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(None)
  // Ranges don't overlap: [1,2] vs [3,5]
}

pub fn negotiate_missing_protocol_local_test() {
  let local = [Capability("proto_a", 1, 3)]
  let remote = [Capability("proto_b", 1, 5)]

  negotiation.protocol_negotiate(local, remote, "proto_b")
  |> should.equal(None)
  // Local doesn't support proto_b
}

pub fn negotiate_missing_protocol_remote_test() {
  let local = [Capability("proto_a", 1, 3)]
  let remote = [Capability("proto_b", 1, 5)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(None)
  // Remote doesn't support proto_a
}

pub fn negotiate_missing_protocol_both_test() {
  let local = [Capability("proto_a", 1, 3)]
  let remote = [Capability("proto_b", 1, 5)]

  negotiation.protocol_negotiate(local, remote, "proto_c")
  |> should.equal(None)
  // Neither supports proto_c
}

pub fn negotiate_subset_range_test() {
  let local = [Capability("proto_a", 1, 10)]
  let remote = [Capability("proto_a", 3, 7)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(Some(7))
  // Overlap is [3, 7], max is 7
}

pub fn negotiate_remote_subset_test() {
  let local = [Capability("proto_a", 5, 8)]
  let remote = [Capability("proto_a", 1, 20)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(Some(8))
  // Overlap is [5, 8], max is 8
}

pub fn negotiate_adjacent_ranges_no_overlap_test() {
  let local = [Capability("proto_a", 1, 3)]
  let remote = [Capability("proto_a", 4, 6)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(None)
  // Ranges [1,3] and [4,6] don't overlap
}

pub fn negotiate_single_version_overlap_test() {
  let local = [Capability("proto_a", 1, 3)]
  let remote = [Capability("proto_a", 3, 5)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(Some(3))
  // Overlap is exactly [3, 3]
}

pub fn negotiate_multiple_protocols_independent_test() {
  let local = [Capability("proto_a", 1, 3), Capability("proto_b", 5, 10)]
  let remote = [Capability("proto_a", 2, 4), Capability("proto_b", 7, 12)]

  // Negotiate proto_a
  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(Some(3))

  // Negotiate proto_b
  negotiation.protocol_negotiate(local, remote, "proto_b")
  |> should.equal(Some(10))
}

pub fn negotiate_empty_capabilities_test() {
  let local = []
  let remote = [Capability("proto_a", 1, 3)]

  negotiation.protocol_negotiate(local, remote, "proto_a")
  |> should.equal(None)
}

// ============================================================================
// validate_capabilities tests
// ============================================================================

pub fn validate_capabilities_valid_test() {
  let caps = [Capability("proto_a", 1, 3), Capability("proto_b", 5, 10)]

  negotiation.validate_capabilities(caps)
  |> should.equal(Ok(Nil))
}

pub fn validate_capabilities_min_equals_max_test() {
  let caps = [Capability("proto_a", 2, 2)]

  negotiation.validate_capabilities(caps)
  |> should.equal(Ok(Nil))
}

pub fn validate_capabilities_invalid_range_test() {
  let caps = [Capability("proto_a", 5, 2)]

  negotiation.validate_capabilities(caps)
  |> should.be_error()
}

pub fn validate_capabilities_empty_protocol_test() {
  let caps = [Capability("", 1, 3)]

  negotiation.validate_capabilities(caps)
  |> should.be_error()
}

pub fn validate_capabilities_multiple_invalid_test() {
  let caps = [
    Capability("proto_a", 1, 3),
    Capability("proto_b", 10, 5),
    Capability("", 1, 2),
  ]

  negotiation.validate_capabilities(caps)
  |> should.be_error()
  // Should fail on first invalid (proto_b)
}

pub fn validate_capabilities_empty_list_test() {
  let caps = []

  negotiation.validate_capabilities(caps)
  |> should.equal(Ok(Nil))
  // Empty list is valid (no capabilities to validate)
}
