import distribute/crypto/adapter
import distribute/crypto/sodium_adapter
import distribute/crypto/types
import gleam/dynamic
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// new() returns a valid CryptoAdapter
// ============================================================================

pub fn new_returns_adapter_test() {
  // Must not crash — validates all function pointers are set
  let _adapter = sodium_adapter.new()
  should.be_true(True)
}

// ============================================================================
// All operations via adapter — test through the adapter record fields
// ============================================================================

// We test through the adapter record. Since all internal fns are private,
// we can only access them via the record returned by new().

pub fn init_returns_provider_failure_test() {
  let a = sodium_adapter.new()
  let opts = adapter.default_options("sodium_test")
  let result = a.init(opts)
  should.be_error(result)
  let assert Error(err) = result
  case err {
    types.ProviderFailure(reason) -> {
      should.equal(
        reason,
        "Native libsodium backend not implemented — use otp_crypto_adapter instead",
      )
    }
    _ -> should.fail()
  }
}

pub fn shutdown_returns_ok_test() {
  let adp = sodium_adapter.new()
  // We need a dummy handle — use new_handle with a fake state
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let result = adp.shutdown(handle)
  should.be_ok(result)
}

pub fn handshake_start_returns_error_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let result = adp.handshake_start(handle, "local", "remote", None)
  should.be_error(result)
}

pub fn handshake_continue_returns_error_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let state =
    types.new_handshake_state("a", "b", types.Plain, gleam_dynamic_nil())
  let msg =
    types.HandshakeMessage(message_type: "test", payload: <<>>, metadata: None)
  let result = adp.handshake_continue(handle, state, msg)
  should.be_error(result)
}

pub fn secure_context_returns_none_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let result = adp.secure_context(handle, "any_node")
  should.equal(result, None)
}

pub fn encrypt_returns_error_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let ctx =
    types.new_secure_context(
      "node",
      types.SecureEstablished,
      0,
      "k1",
      gleam_dynamic_nil(),
    )
  let result = adp.encrypt(handle, ctx, <<"plaintext":utf8>>)
  should.be_error(result)
}

pub fn decrypt_returns_error_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let ctx =
    types.new_secure_context(
      "node",
      types.SecureEstablished,
      0,
      "k1",
      gleam_dynamic_nil(),
    )
  let result = adp.decrypt(handle, ctx, <<"ciphertext":utf8>>)
  should.be_error(result)
}

pub fn rekey_returns_error_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let result = adp.rekey(handle, "any_node")
  should.be_error(result)
}

pub fn health_returns_down_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let result = adp.health(handle)
  case result {
    types.Down(reason) -> should.equal(reason, "sodium adapter not implemented")
    _ -> should.fail()
  }
}

pub fn metrics_returns_empty_test() {
  let adp = sodium_adapter.new()
  let handle = types.new_handle("dummy", gleam_dynamic_nil())
  let m = adp.metrics(handle)
  should.equal(m.handshakes_initiated, 0)
  should.equal(m.handshakes_completed, 0)
  should.equal(m.handshakes_failed, 0)
  should.equal(m.encrypt_count, 0)
  should.equal(m.decrypt_count, 0)
  should.equal(m.rekey_count, 0)
  should.equal(m.active_contexts, 0)
}

// Helper: create a Dynamic Nil for dummy handles
@external(erlang, "distribute_ffi_utils", "dynamic_nil")
fn gleam_dynamic_nil() -> dynamic.Dynamic
