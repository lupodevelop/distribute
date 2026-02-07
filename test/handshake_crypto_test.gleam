import distribute/handshake/crypto.{Provider, SecureContext}
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Provider type construction
// ============================================================================

pub fn provider_construction_test() {
  let p = Provider(name: "otp_crypto")
  should.equal(p.name, "otp_crypto")
}

pub fn provider_different_names_test() {
  let p1 = Provider(name: "otp_crypto")
  let p2 = Provider(name: "libsodium")
  should.not_equal(p1, p2)
}

pub fn provider_same_name_equal_test() {
  let p1 = Provider(name: "x")
  let p2 = Provider(name: "x")
  should.equal(p1, p2)
}

pub fn provider_empty_name_test() {
  let p = Provider(name: "")
  should.equal(p.name, "")
}

// ============================================================================
// SecureContext type construction
// ============================================================================

pub fn secure_context_construction_test() {
  let ctx = SecureContext(info: "AES-256-GCM session with node@host")
  should.equal(ctx.info, "AES-256-GCM session with node@host")
}

pub fn secure_context_empty_info_test() {
  let ctx = SecureContext(info: "")
  should.equal(ctx.info, "")
}

pub fn secure_context_equality_test() {
  let c1 = SecureContext(info: "a")
  let c2 = SecureContext(info: "a")
  should.equal(c1, c2)
}

// ============================================================================
// register_provider — stub always returns Ok(Nil)
// ============================================================================

pub fn register_provider_returns_ok_test() {
  let result = crypto.register_provider(Provider(name: "otp_crypto"))
  should.be_ok(result)
}

pub fn register_provider_ok_nil_test() {
  let assert Ok(value) = crypto.register_provider(Provider(name: "test"))
  should.equal(value, Nil)
}

pub fn register_provider_multiple_providers_test() {
  // Registering multiple providers should not crash
  let _ = crypto.register_provider(Provider(name: "p1"))
  let _ = crypto.register_provider(Provider(name: "p2"))
  let result = crypto.register_provider(Provider(name: "p3"))
  should.be_ok(result)
}

pub fn register_provider_same_name_twice_test() {
  // Idempotent — should not crash
  let _ = crypto.register_provider(Provider(name: "dup"))
  let result = crypto.register_provider(Provider(name: "dup"))
  should.be_ok(result)
}

// ============================================================================
// get_secure_context — stub always returns None
// ============================================================================

pub fn get_secure_context_returns_none_test() {
  let result = crypto.get_secure_context("node@host:9000")
  should.equal(result, None)
}

pub fn get_secure_context_any_node_none_test() {
  let result = crypto.get_secure_context("any_random_node")
  should.equal(result, None)
}

pub fn get_secure_context_empty_node_test() {
  let result = crypto.get_secure_context("")
  should.equal(result, None)
}

pub fn get_secure_context_after_register_test() {
  // Even after registering a provider, context is always None (stub)
  let _ = crypto.register_provider(Provider(name: "registered"))
  let result = crypto.get_secure_context("some_node")
  should.equal(result, None)
}
