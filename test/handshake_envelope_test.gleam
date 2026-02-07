import distribute/handshake/envelope.{Envelope}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Envelope type construction
// ============================================================================

pub fn envelope_construction_test() {
  let env = Envelope(tag: "hello", version: 1, payload: <<1, 2, 3>>)
  should.equal(env.tag, "hello")
  should.equal(env.version, 1)
  should.equal(env.payload, <<1, 2, 3>>)
}

pub fn envelope_empty_payload_test() {
  let env = Envelope(tag: "accept", version: 2, payload: <<>>)
  should.equal(env.tag, "accept")
  should.equal(env.version, 2)
  should.equal(env.payload, <<>>)
}

// ============================================================================
// Stub functions (currently return placeholders)
// ============================================================================

pub fn peek_tag_stub_returns_ok_test() {
  // Current stub always returns Ok(#("unknown", 0))
  let result = envelope.peek_tag(<<1, 2, 3>>)
  should.be_ok(result)
  let assert Ok(#(tag, version)) = result
  should.equal(tag, "unknown")
  should.equal(version, 0)
}

pub fn peek_tag_empty_input_test() {
  let result = envelope.peek_tag(<<>>)
  should.be_ok(result)
}

pub fn wrap_stub_returns_bitarray_test() {
  let result = envelope.wrap("hello", 1, <<"payload":utf8>>)
  // Stub just returns the payload as-is
  should.equal(result, <<"payload":utf8>>)
}

pub fn unwrap_stub_returns_ok_test() {
  let result = envelope.unwrap(<<"data":utf8>>)
  should.be_ok(result)
}
