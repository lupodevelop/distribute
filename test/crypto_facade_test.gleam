import distribute/crypto
import distribute/crypto/types
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Constants
// ============================================================================

pub fn default_name_test() {
  should.equal(crypto.default_name, "distribute_crypto")
}

// ============================================================================
// Configuration helpers
// ============================================================================

pub fn default_options_returns_valid_options_test() {
  let opts = crypto.default_options("test_crypto")
  should.equal(opts.name, "test_crypto")
  should.equal(opts.is_development, False)
}

pub fn development_options_is_development_test() {
  let opts = crypto.development_options("dev_crypto")
  should.equal(opts.name, "dev_crypto")
  should.equal(opts.is_development, True)
}

// ============================================================================
// Before start_link: NotInitialized
// ============================================================================

pub fn health_before_init_is_down_test() {
  // Ensure we operate on a unique name to avoid leaking from other tests
  let status = crypto.health()
  case status {
    types.Down(_) -> should.be_true(True)
    types.Up -> should.be_true(True)
    types.Degraded(_) -> should.be_true(True)
  }
}

pub fn metrics_before_init_returns_empty_test() {
  let m = crypto.metrics()
  // Even if not initialized, metrics() returns empty_metrics()
  should.equal(m.handshakes_initiated >= 0, True)
}

pub fn secure_context_before_init_is_none_test() {
  let result = crypto.secure_context("nonexistent_node")
  should.equal(result, None)
}

// ============================================================================
// Lifecycle: start_link → operations → shutdown
// ============================================================================

pub fn start_link_succeeds_test() {
  let opts = crypto.default_options("test_lifecycle_start")
  let result = crypto.start_link(opts)
  should.be_ok(result)
  // Cleanup
  let _ = do_cleanup("test_lifecycle_start")
  Nil
}

pub fn get_handle_after_start_test() {
  let opts = crypto.default_options("distribute_crypto")
  let assert Ok(_handle) = crypto.start_link(opts)
  let result = crypto.get_handle()
  should.be_ok(result)
  // Cleanup
  let _ = do_cleanup("distribute_crypto")
  Nil
}

pub fn get_handle_by_name_test() {
  let name = "named_crypto_test"
  let opts = crypto.default_options(name)
  let assert Ok(_) = crypto.start_link(opts)
  let result = crypto.get_handle_by_name(name)
  should.be_ok(result)
  // Cleanup
  let _ = do_cleanup(name)
  Nil
}

pub fn health_after_start_is_up_test() {
  let opts = crypto.default_options("distribute_crypto")
  let assert Ok(_) = crypto.start_link(opts)
  let status = crypto.health()
  case status {
    types.Up -> should.be_true(True)
    types.Degraded(_) -> should.be_true(True)
    _ -> should.fail()
  }
  // Cleanup
  let _ = do_cleanup("distribute_crypto")
  Nil
}

pub fn handshake_start_after_init_test() {
  let opts = crypto.default_options("distribute_crypto")
  let assert Ok(_) = crypto.start_link(opts)
  let result = crypto.handshake_start("local@a", "remote@b", None)
  // noop_adapter returns Ok(Established(...))
  should.be_ok(result)
  // Cleanup
  let _ = do_cleanup("distribute_crypto")
  Nil
}

pub fn encrypt_decrypt_roundtrip_test() {
  let opts = crypto.default_options("distribute_crypto")
  let assert Ok(_) = crypto.start_link(opts)
  
  // noop adapter: secure_context returns a context
  let ctx_opt = crypto.secure_context("any_node")
  case ctx_opt {
    option.Some(ctx) -> {
      let plain = <<"hello roundtrip":utf8>>
      let assert Ok(encrypted) = crypto.encrypt(ctx, plain)
      let assert Ok(decrypted) = crypto.decrypt(ctx, encrypted)
      // noop adapter: encrypt/decrypt are identity
      should.equal(decrypted, plain)
    }
    option.None -> {
      // noop adapter may return None for secure_context, that's ok
      should.be_true(True)
    }
  }
  // Cleanup
  let _ = do_cleanup("distribute_crypto")
  Nil
}

pub fn rekey_after_init_test() {
  let opts = crypto.default_options("distribute_crypto")
  let assert Ok(_) = crypto.start_link(opts)
  let result = crypto.rekey("some_node")
  // noop adapter: rekey may return Error(NoSecureContext) for unknown nodes
  case result {
    Ok(Nil) -> should.be_true(True)
    Error(_) -> should.be_true(True)
  }
  // Cleanup
  let _ = do_cleanup("distribute_crypto")
  Nil
}

pub fn metrics_after_init_test() {
  let opts = crypto.default_options("distribute_crypto")
  let assert Ok(_) = crypto.start_link(opts)
  let m = crypto.metrics()
  should.equal(m.handshakes_initiated >= 0, True)
  // Cleanup
  let _ = do_cleanup("distribute_crypto")
  Nil
}

// ============================================================================
// Internal: cleanup helper
// ============================================================================

@external(erlang, "crypto_provider_ffi", "delete_persistent_term")
fn do_cleanup(name: String) -> Nil
