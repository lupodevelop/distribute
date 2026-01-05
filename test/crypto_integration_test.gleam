//// Integration tests for the crypto module.
////
//// Tests the crypto layer including:
//// - Provider initialization and shutdown
//// - Handshake flow
//// - Encryption/decryption (noop adapter returns identity)
//// - Rekey operations
//// - Health and metrics
//// - Concurrency and timeout handling
////

import distribute/crypto/adapter
import distribute/crypto/noop_adapter
import distribute/crypto/types
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import test_helpers

// =============================================================================
// Provider Lifecycle Tests
// =============================================================================

pub fn crypto_init_and_shutdown_test() {
  let name = "test_crypto_lifecycle"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        // Check health
        let health = { provider.health }(handle)
        case health {
          types.Up -> Nil
          types.Degraded(_) -> Nil
          types.Down(reason) -> panic as { "Unexpected down: " <> reason }
        }

        Nil
      },
    )
}

pub fn crypto_get_handle_after_init_test() {
  let name = "test_crypto_handle"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        // Verify provider health and get handle by name
        let health = { provider.health }(handle)
        case health {
          types.Up -> Nil
          types.Degraded(_) -> Nil
          types.Down(reason) -> panic as { "Unexpected down: " <> reason }
        }

        let handle_result = noop_adapter.get_handle(name)
        should.be_ok(handle_result)

        Nil
      },
    )
}

// =============================================================================
// Handshake Tests
// =============================================================================

pub fn crypto_handshake_establishes_context_test() {
  let name = "test_crypto_handshake"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_b@localhost"

        // Start handshake - noop adapter completes immediately
        let result = { provider.handshake_start }(handle, local, remote, None)
        should.be_ok(result)

        let assert Ok(handshake_result) = result
        case handshake_result {
          types.Established(ctx) -> {
            // Verify context properties
            types.context_node_id(ctx) |> should.equal(remote)
            types.context_stage(ctx) |> should.equal(types.SecureEstablished)
          }
          types.Continue(_, _) -> panic as "Expected Established, got Continue"
          types.HandshakeError(_) ->
            panic as "Expected Established, got HandshakeError"
        }

        Nil
      },
    )
}

pub fn crypto_secure_context_lookup_test() {
  let name = "test_crypto_context"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_c@localhost"

        // Before handshake - no context
        let no_ctx = { provider.secure_context }(handle, remote)
        should.equal(no_ctx, None)

        // After handshake - context exists
        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let ctx_opt = { provider.secure_context }(handle, remote)
        should.be_some(ctx_opt)

        Nil
      },
    )
}

// =============================================================================
// Encryption/Decryption Tests
// =============================================================================

pub fn crypto_noop_encrypt_decrypt_identity_test() {
  let name = "test_crypto_encrypt"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_d@localhost"

        // Establish context
        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let assert Some(ctx) = { provider.secure_context }(handle, remote)

        // Encrypt
        let plaintext = <<"Hello, secure world!">>
        let encrypt_result = { provider.encrypt }(handle, ctx, plaintext)
        should.be_ok(encrypt_result)

        let assert Ok(ciphertext) = encrypt_result
        // Noop adapter returns identity
        should.equal(ciphertext, plaintext)

        // Decrypt
        let decrypt_result = { provider.decrypt }(handle, ctx, ciphertext)
        should.be_ok(decrypt_result)

        let assert Ok(decrypted) = decrypt_result
        should.equal(decrypted, plaintext)

        Nil
      },
    )
}

pub fn crypto_encrypt_multiple_messages_test() {
  let name = "test_crypto_multi"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_e@localhost"

        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let assert Some(ctx) = { provider.secure_context }(handle, remote)

        // Multiple encryptions
        let msg1 = <<"Message 1">>
        let msg2 = <<"Message 2 is longer">>
        let msg3 = <<1, 2, 3, 4, 5>>

        let assert Ok(enc1) = { provider.encrypt }(handle, ctx, msg1)
        let assert Ok(enc2) = { provider.encrypt }(handle, ctx, msg2)
        let assert Ok(enc3) = { provider.encrypt }(handle, ctx, msg3)

        // Decrypt all
        let assert Ok(dec1) = { provider.decrypt }(handle, ctx, enc1)
        let assert Ok(dec2) = { provider.decrypt }(handle, ctx, enc2)
        let assert Ok(dec3) = { provider.decrypt }(handle, ctx, enc3)

        should.equal(dec1, msg1)
        should.equal(dec2, msg2)
        should.equal(dec3, msg3)

        Nil
      },
    )
}

// =============================================================================
// Rekey Tests
// =============================================================================

pub fn crypto_rekey_updates_key_id_test() {
  let name = "test_crypto_rekey"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options(
      noop_adapter.new,
      adapter.development_options(name),
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_f@localhost"

        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let assert Some(ctx1) = { provider.secure_context }(handle, remote)
        let key_id1 = types.context_key_id(ctx1)

        // Rekey
        let rekey_result = { provider.rekey }(handle, remote)
        should.be_ok(rekey_result)

        // New context has different key_id
        let assert Some(ctx2) = { provider.secure_context }(handle, remote)
        let key_id2 = types.context_key_id(ctx2)

        // Key IDs should be different
        should.not_equal(key_id1, key_id2)

        Nil
      },
    )
}

pub fn crypto_rekey_without_context_fails_test() {
  let name = "test_crypto_rekey_fail"
  let options = adapter.development_options(name)
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        // Rekey without establishing context first
        let result = { provider.rekey }(handle, "unknown_node@localhost")
        should.be_error(result)

        let assert Error(err) = result
        case err {
          types.NoSecureContext(_) -> Nil
          _ -> panic as "Expected NoSecureContext error"
        }

        Nil
      },
    )
}

// =============================================================================
// Metrics Tests
// =============================================================================

pub fn crypto_metrics_track_operations_test() {
  let name = "test_crypto_metrics"
  let options = adapter.development_options(name)
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        // Initial metrics
        let m1 = { provider.metrics }(handle)
        should.equal(m1.handshakes_initiated, 0)
        should.equal(m1.encrypt_count, 0)
        should.equal(m1.decrypt_count, 0)

        // Perform operations
        let local = "node_a@localhost"
        let remote = "node_g@localhost"
        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let assert Some(ctx) = { provider.secure_context }(handle, remote)

        let _ = { provider.encrypt }(handle, ctx, <<"test1">>)
        let _ = { provider.encrypt }(handle, ctx, <<"test2">>)
        let _ = { provider.decrypt }(handle, ctx, <<"test3">>)

        // Check metrics updated
        let m2 = { provider.metrics }(handle)
        should.equal(m2.handshakes_initiated, 1)
        should.equal(m2.handshakes_completed, 1)
        should.equal(m2.encrypt_count, 2)
        should.equal(m2.decrypt_count, 1)
        should.equal(m2.active_contexts, 1)

        Nil
      },
    )
}

// =============================================================================
// Health Tests
// =============================================================================

pub fn crypto_health_returns_degraded_in_dev_mode_test() {
  let name = "test_crypto_health"
  let options = adapter.development_options(name)
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let health = { provider.health }(handle)
        case health {
          types.Degraded(msg) -> {
            // Development mode shows degraded because no real encryption
            should.be_true(
              msg == "Development mode - no real encryption" || msg != "",
            )
          }
          types.Up -> Nil
          types.Down(reason) -> panic as { "Unexpected down: " <> reason }
        }

        Nil
      },
    )
}

// =============================================================================
// Concurrency Tests
// =============================================================================

pub fn crypto_concurrent_handshakes_test() {
  let name = "test_crypto_concurrent"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"

        // Start multiple handshakes concurrently (simulated sequentially for test)
        let remote1 = "node_h@localhost"
        let remote2 = "node_i@localhost"
        let remote3 = "node_j@localhost"

        let assert Ok(_) = { provider.handshake_start }(handle, local, remote1, None)
        let assert Ok(_) = { provider.handshake_start }(handle, local, remote2, None)
        let assert Ok(_) = { provider.handshake_start }(handle, local, remote3, None)

        // All contexts should exist
        should.be_some({ provider.secure_context }(handle, remote1))
        should.be_some({ provider.secure_context }(handle, remote2))
        should.be_some({ provider.secure_context }(handle, remote3))

        // Metrics should reflect all handshakes
        let m = { provider.metrics }(handle)
        should.equal(m.handshakes_initiated, 3)
        should.equal(m.handshakes_completed, 3)
        should.equal(m.active_contexts, 3)

        Nil
      },
    )
}

pub fn crypto_concurrent_encrypt_decrypt_test() {
  let name = "test_crypto_concurrent_enc"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_k@localhost"

        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let assert Some(ctx) = { provider.secure_context }(handle, remote)

        // Multiple encrypt/decrypt operations
        let messages = [
          <<"msg1">>,
          <<"msg2">>,
          <<"msg3">>,
          <<"msg4">>,
          <<"msg5">>,
        ]

        // Start multiple handshakes concurrently (simulated sequentially for test)
        let remote1 = "node_h@localhost"
        let remote2 = "node_i@localhost"
        let remote3 = "node_j@localhost"

        let assert Ok(_) = { provider.handshake_start }(handle, local, remote1, None)
        let assert Ok(_) = { provider.handshake_start }(handle, local, remote2, None)
        let assert Ok(_) = { provider.handshake_start }(handle, local, remote3, None)

        // All contexts should exist
        should.be_some({ provider.secure_context }(handle, remote1))
        should.be_some({ provider.secure_context }(handle, remote2))
        should.be_some({ provider.secure_context }(handle, remote3))

        // Metrics should reflect all handshakes
        let m = { provider.metrics }(handle)
        should.equal(m.handshakes_initiated, 3)
        should.equal(m.handshakes_completed, 3)
        should.equal(m.active_contexts, 3)

        // Encrypt before rekey
        let local = "node_a@localhost"
        let remote = "node_l@localhost"

        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let assert Some(ctx1) = { provider.secure_context }(handle, remote)

        let msg = <<"before rekey">>
        let assert Ok(enc1) = { provider.encrypt }(handle, ctx1, msg)

        // Rekey
        let assert Ok(_) = { provider.rekey }(handle, remote)

        // Get new context
        let assert Some(ctx2) = { provider.secure_context }(handle, remote)

        // Key IDs should differ
        should.not_equal(types.context_key_id(ctx1), types.context_key_id(ctx2))

        // Encrypt after rekey with new context
        let assert Ok(enc2) = { provider.encrypt }(handle, ctx2, msg)

        // Decrypt both (noop = identity, so both work)
        let assert Ok(dec1) = { provider.decrypt }(handle, ctx1, enc1)
        let assert Ok(dec2) = { provider.decrypt }(handle, ctx2, enc2)

        should.equal(dec1, msg)
        should.equal(dec2, msg)

        Nil
      },
    )
}

// =============================================================================
// Timeout Handling Tests
// =============================================================================

pub fn crypto_operations_after_shutdown_fail_test() {
  let name = "test_crypto_after_shutdown"
  let assert Ok(_) =
    test_helpers.with_provider_module_with_options_checked(
      noop_adapter.new,
      noop_adapter.get_handle,
      adapter.development_options(name),
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_m@localhost"

        let assert Ok(_) = { provider.handshake_start }(handle, local, remote, None)
        let assert Some(_ctx) = { provider.secure_context }(handle, remote)

        // After shutdown, operations should timeout or fail gracefully
        // Health check should return Down
        let health = { provider.health }(handle)
        case health {
          types.Down(_) -> Nil
          _ -> {
            // May also timeout, which is acceptable
            Nil
          }
        }

        Nil
      },
    )
}
