//// Tests for registry convenience wrappers and retry integration.
////
//// These tests verify the registry wrappers work correctly with the retry module.
//// Each test spawns a separate process to avoid PID registration conflicts.

import distribute/codec
import distribute/global
import distribute/registry
import distribute/retry
import distribute/settings
import gleam/erlang/process
import gleam/int
import gleeunit
import gleeunit/should

// ============================================================================
// Setup
// ============================================================================

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

/// Generate a unique name to avoid conflicts between test runs
fn unique_name(prefix: String) -> String {
  prefix <> "_" <> int.to_string(unique_integer())
}

pub fn main() {
  settings.set_allow_atom_creation(True)
  gleeunit.main()
}

// ============================================================================
// register_with_strategy Tests
// ============================================================================

pub fn register_with_strategy_default_policy_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("strategy_default")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)
      let policy = retry.default_with_jitter()

      let result = registry.register_with_strategy(subject, name, policy)
      result |> should.be_ok()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(100)
}

pub fn register_with_strategy_aggressive_policy_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("strategy_aggressive")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)
      let policy = retry.aggressive()

      let result = registry.register_with_strategy(subject, name, policy)
      result |> should.be_ok()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(100)
}

pub fn register_with_strategy_custom_policy_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("strategy_custom")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)

      let policy =
        retry.default()
        |> retry.with_max_attempts(5)
        |> retry.with_base_delay_ms(50)
        |> retry.with_full_jitter()

      let result = registry.register_with_strategy(subject, name, policy)
      result |> should.be_ok()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(100)
}

pub fn register_with_strategy_no_retry_policy_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("strategy_no_retry")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)
      let policy = retry.no_retry()

      let result = registry.register_with_strategy(subject, name, policy)
      result |> should.be_ok()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(100)
}

pub fn register_with_strategy_invalid_empty_name_test() {
  settings.set_allow_atom_creation(True)

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)
      let policy = retry.default_with_jitter()

      // Empty name should fail
      let result = registry.register_with_strategy(subject, "", policy)
      result |> should.be_error()

      process.sleep(50)
    })

  process.sleep(100)
}

pub fn register_with_strategy_invalid_name_with_spaces_test() {
  settings.set_allow_atom_creation(True)

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)
      let policy = retry.default_with_jitter()

      // Name with spaces should fail
      let result =
        registry.register_with_strategy(subject, "invalid name", policy)
      result |> should.be_error()

      process.sleep(50)
    })

  process.sleep(100)
}

// ============================================================================
// is_registered Tests
// ============================================================================

pub fn is_registered_returns_false_when_not_registered_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("not_registered")

  registry.is_registered(name)
  |> should.be_false()
}

pub fn is_registered_returns_true_when_registered_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("is_registered")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)

      registry.register_global(subject, name)
      |> should.be_ok()

      registry.is_registered(name)
      |> should.be_true()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(100)
}

// ============================================================================
// lookup_global Tests
// ============================================================================

pub fn lookup_global_not_found_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("lookup_not_found")
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()

  registry.lookup_global(name, encoder, decoder)
  |> should.be_error()
}

pub fn lookup_global_finds_registered_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("lookup_found")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)

      registry.register_global(subject, name)
      |> should.be_ok()

      // Lookup should find it
      registry.lookup_global(name, encoder, decoder)
      |> should.be_ok()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(100)
}

// ============================================================================
// lookup_with_timeout Tests
// ============================================================================

pub fn lookup_with_timeout_finds_immediately_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("lookup_timeout_immediate")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)

      // Register first
      registry.register_global(subject, name)
      |> should.be_ok()

      // Lookup should succeed immediately
      registry.lookup_with_timeout(name, encoder, decoder, 1000, 50)
      |> should.be_ok()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(150)
}

pub fn lookup_with_timeout_times_out_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("lookup_timeout_fail")
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()

  // Lookup non-existent name should timeout
  registry.lookup_with_timeout(name, encoder, decoder, 200, 50)
  |> should.be_error()
}

// ============================================================================
// Backward Compatibility Tests (deprecated API)
// ============================================================================

pub fn register_with_retry_still_works_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("legacy_retry")

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)

      // Old API should still work (deprecated but functional)
      registry.register_with_retry(subject, name, 3, 100)
      |> should.be_ok()

      // Cleanup
      let _ = registry.unregister(name)
      process.sleep(50)
    })

  process.sleep(150)
}

// ============================================================================
// Integration Tests
// ============================================================================

pub fn full_lifecycle_test() {
  settings.set_allow_atom_creation(True)
  let name = unique_name("lifecycle")

  // 1. Not registered initially
  registry.is_registered(name)
  |> should.be_false()

  let _ =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let subject = global.new(encoder, decoder)
      let policy = retry.default_with_jitter()

      // 2. Register with retry policy
      registry.register_with_strategy(subject, name, policy)
      |> should.be_ok()

      // 3. Now registered
      registry.is_registered(name)
      |> should.be_true()

      // 4. Can lookup
      registry.lookup_global(name, encoder, decoder)
      |> should.be_ok()

      // 5. Cleanup
      registry.unregister(name)
      |> should.be_ok()

      // 6. Not registered after cleanup
      registry.is_registered(name)
      |> should.be_false()

      process.sleep(50)
    })

  process.sleep(200)
}
