//// Integration tests for the sodium crypto adapter.
////
//// Tests the sodium adapter including:
//// - Provider initialization and shutdown
//// - Handshake flow (X25519 key exchange)
//// - Encryption/decryption (ChaCha20-Poly1305 AEAD)
//// - Rekey operations (HKDF-based key rotation)
//// - Health and metrics
//// - Concurrency and error handling
////

import distribute/crypto/adapter
import distribute/crypto/sodium_adapter
import distribute/crypto/types
import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// =============================================================================
// Provider Lifecycle Tests
// =============================================================================

pub fn sodium_init_and_shutdown_test() {
  let name = "test_sodium_lifecycle"
  let options = adapter.default_options(name)
  let provider = sodium_adapter.new()

  // Initialize
  let result = { provider.init }(options)
  should.be_ok(result)

  let assert Ok(handle) = result

  // Check health
  let health = { provider.health }(handle)
  case health {
    types.Up -> Nil
    types.Degraded(_) -> Nil
    types.Down(reason) -> panic as { "Unexpected down: " <> reason }
  }

  // Shutdown
  let shutdown_result = { provider.shutdown }(handle)
  should.be_ok(shutdown_result)
}

pub fn sodium_get_handle_after_init_test() {
  let name = "test_sodium_handle"
  let options = adapter.default_options(name)
  let provider = sodium_adapter.new()

  // Initialize
  let assert Ok(_handle) = { provider.init }(options)

  // Get handle by name
  let handle_result = sodium_adapter.get_handle(name)
  should.be_ok(handle_result)

  // Cleanup
  let assert Ok(handle) = handle_result
  let _ = { provider.shutdown }(handle)
}

// =============================================================================
// Handshake Tests
// =============================================================================

pub fn sodium_handshake_initiator_creates_pending_test() {
  let name = "test_sodium_handshake_init"
  let options = adapter.default_options(name)
  let provider = sodium_adapter.new()

  let assert Ok(handle) = { provider.init }(options)

  let local = "node_a@localhost"
  let remote = "node_b@localhost"

  // Start handshake as initiator (no initial message)
  let result = { provider.handshake_start }(handle, local, remote, None)
  should.be_ok(result)

  let assert Ok(handshake_result) = result
  case handshake_result {
    types.Continue(state, Some(msg)) -> {
      // Should have sent our ephemeral public key
      should.equal(msg.message_type, "ephemeral_pubkey")
      // Public key should be 32 bytes
      should.equal(bit_array.byte_size(msg.payload), 32)
      // State should be in progress
      types.handshake_stage(state) |> should.equal(types.KeyExchangeInProgress)
    }
    types.Continue(_, None) -> panic as "Expected message in Continue"
    types.Established(_) -> panic as "Expected Continue, got Established"
    types.HandshakeError(_) -> panic as "Expected Continue, got HandshakeError"
  }

  // Cleanup
  let _ = { provider.shutdown }(handle)
}

pub fn sodium_handshake_responder_derives_key_test() {
  let name = "test_sodium_handshake_resp"
  let options = adapter.default_options(name)
  let provider = sodium_adapter.new()

  let assert Ok(handle) = { provider.init }(options)

  let local = "node_b@localhost"
  let remote = "node_a@localhost"

  // Simulate receiving initiator's public key
  // Generate a fake peer public key (32 bytes)
  let peer_pub_key = <<
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
  >>

  let initial_msg =
    types.HandshakeMessage(
      message_type: "ephemeral_pubkey",
      payload: peer_pub_key,
      metadata: None,
    )

  // Start handshake as responder (with initial message)
  let result =
    { provider.handshake_start }(handle, local, remote, Some(initial_msg))
  should.be_ok(result)

  let assert Ok(handshake_result) = result
  case handshake_result {
    types.Continue(state, Some(msg)) -> {
      // Should have sent our ephemeral public key response
      should.equal(msg.message_type, "ephemeral_pubkey_response")
      should.equal(bit_array.byte_size(msg.payload), 32)
      // Responder should have established context
      types.handshake_stage(state) |> should.equal(types.SecureEstablished)
    }
    types.Continue(_, None) -> panic as "Expected message in Continue"
    types.Established(_) -> Nil
    // Also acceptable
    types.HandshakeError(err) ->
      panic as { "Expected Continue, got HandshakeError: " <> string.inspect(err) }
  }

  // Context should now exist for remote node
  let ctx = { provider.secure_context }(handle, remote)
  should.be_some(ctx)

  // Cleanup
  let _ = { provider.shutdown }(handle)
}

pub fn sodium_full_handshake_between_two_providers_test() {
  let name_a = "test_sodium_node_a"
  let name_b = "test_sodium_node_b"
  let options_a = adapter.default_options(name_a)
  let options_b = adapter.default_options(name_b)
  let provider = sodium_adapter.new()

  let assert Ok(handle_a) = { provider.init }(options_a)
  let assert Ok(handle_b) = { provider.init }(options_b)

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"

  // Step 1: A initiates handshake
  let assert Ok(types.Continue(state_a, Some(msg_a))) =
    { provider.handshake_start }(handle_a, node_a, node_b, None)

  // Step 2: B receives A's public key, responds with own
  let assert Ok(types.Continue(_state_b, Some(msg_b))) =
    { provider.handshake_start }(handle_b, node_b, node_a, Some(msg_a))

  // Step 3: A receives B's response, completes handshake
  let assert Ok(types.Established(ctx_a)) =
    { provider.handshake_continue }(handle_a, state_a, msg_b)

  // Both should now have secure contexts
  should.be_some({ provider.secure_context }(handle_a, node_b))
  should.be_some({ provider.secure_context }(handle_b, node_a))

  // Contexts should be in Established stage
  types.context_stage(ctx_a) |> should.equal(types.SecureEstablished)

  // Cleanup
  let _ = { provider.shutdown }(handle_a)
  let _ = { provider.shutdown }(handle_b)
}

// =============================================================================
// Encryption/Decryption Tests
// =============================================================================

pub fn sodium_encrypt_decrypt_roundtrip_test() {
  let name_a = "test_sodium_enc_a"
  let name_b = "test_sodium_enc_b"
  let options_a = adapter.default_options(name_a)
  let options_b = adapter.default_options(name_b)
  let provider = sodium_adapter.new()

  let assert Ok(handle_a) = { provider.init }(options_a)
  let assert Ok(handle_b) = { provider.init }(options_b)

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"

  // Complete handshake
  let assert Ok(types.Continue(state_a, Some(msg_a))) =
    { provider.handshake_start }(handle_a, node_a, node_b, None)
  let assert Ok(types.Continue(_state_b, Some(msg_b))) =
    { provider.handshake_start }(handle_b, node_b, node_a, Some(msg_a))
  let assert Ok(types.Established(_)) =
    { provider.handshake_continue }(handle_a, state_a, msg_b)

  // Get contexts
  let assert Some(ctx_a) = { provider.secure_context }(handle_a, node_b)
  let assert Some(ctx_b) = { provider.secure_context }(handle_b, node_a)

  // A encrypts message
  let plaintext = <<"Hello, secure world!">>
  let assert Ok(ciphertext) = { provider.encrypt }(handle_a, ctx_a, plaintext)

  // Ciphertext should be larger than plaintext (nonce + tag)
  should.be_true(bit_array.byte_size(ciphertext) > bit_array.byte_size(plaintext))

  // B decrypts message
  let assert Ok(decrypted) = { provider.decrypt }(handle_b, ctx_b, ciphertext)

  // Should match original
  should.equal(decrypted, plaintext)

  // Cleanup
  let _ = { provider.shutdown }(handle_a)
  let _ = { provider.shutdown }(handle_b)
}

pub fn sodium_encrypt_multiple_messages_test() {
  let name_a = "test_sodium_multi_enc_a"
  let name_b = "test_sodium_multi_enc_b"
  let options_a = adapter.default_options(name_a)
  let options_b = adapter.default_options(name_b)
  let provider = sodium_adapter.new()

  let assert Ok(handle_a) = { provider.init }(options_a)
  let assert Ok(handle_b) = { provider.init }(options_b)

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"

  // Complete handshake
  let assert Ok(types.Continue(state_a, Some(msg_a))) =
    { provider.handshake_start }(handle_a, node_a, node_b, None)
  let assert Ok(types.Continue(_state_b, Some(msg_b))) =
    { provider.handshake_start }(handle_b, node_b, node_a, Some(msg_a))
  let assert Ok(types.Established(_)) =
    { provider.handshake_continue }(handle_a, state_a, msg_b)

  let assert Some(ctx_a) = { provider.secure_context }(handle_a, node_b)
  let assert Some(ctx_b) = { provider.secure_context }(handle_b, node_a)

  // Multiple messages
  let messages = [
    <<"Message 1">>,
    <<"Message 2 is longer">>,
    <<1, 2, 3, 4, 5>>,
    <<"Unicode test">>,
  ]

  // Encrypt all with A, decrypt all with B
  list.each(messages, fn(msg) {
    let assert Ok(ciphertext) = { provider.encrypt }(handle_a, ctx_a, msg)
    let assert Ok(decrypted) = { provider.decrypt }(handle_b, ctx_b, ciphertext)
    should.equal(decrypted, msg)
  })

  // Cleanup
  let _ = { provider.shutdown }(handle_a)
  let _ = { provider.shutdown }(handle_b)
}

pub fn sodium_decrypt_with_wrong_key_fails_test() {
  let name_a = "test_sodium_wrong_key_a"
  let name_b = "test_sodium_wrong_key_b"
  let name_c = "test_sodium_wrong_key_c"
  let options_a = adapter.default_options(name_a)
  let options_b = adapter.default_options(name_b)
  let options_c = adapter.default_options(name_c)
  let provider = sodium_adapter.new()

  let assert Ok(handle_a) = { provider.init }(options_a)
  let assert Ok(handle_b) = { provider.init }(options_b)
  let assert Ok(handle_c) = { provider.init }(options_c)

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"
  let node_c = "node_c@localhost"

  // A <-> B handshake
  let assert Ok(types.Continue(state_a, Some(msg_a))) =
    { provider.handshake_start }(handle_a, node_a, node_b, None)
  let assert Ok(types.Continue(_state_b, Some(msg_b))) =
    { provider.handshake_start }(handle_b, node_b, node_a, Some(msg_a))
  let assert Ok(types.Established(_)) =
    { provider.handshake_continue }(handle_a, state_a, msg_b)

  // A <-> C handshake (different keys)
  let assert Ok(types.Continue(state_a2, Some(msg_a2))) =
    { provider.handshake_start }(handle_a, node_a, node_c, None)
  let assert Ok(types.Continue(_state_c, Some(msg_c))) =
    { provider.handshake_start }(handle_c, node_c, node_a, Some(msg_a2))
  let assert Ok(types.Established(_)) =
    { provider.handshake_continue }(handle_a, state_a2, msg_c)

  // A encrypts for B
  let assert Some(ctx_a_b) = { provider.secure_context }(handle_a, node_b)
  let plaintext = <<"Secret for B">>
  let assert Ok(ciphertext) = { provider.encrypt }(handle_a, ctx_a_b, plaintext)

  // C tries to decrypt with wrong key - should fail
  let assert Some(ctx_c) = { provider.secure_context }(handle_c, node_a)
  let result = { provider.decrypt }(handle_c, ctx_c, ciphertext)
  should.be_error(result)

  // Cleanup
  let _ = { provider.shutdown }(handle_a)
  let _ = { provider.shutdown }(handle_b)
  let _ = { provider.shutdown }(handle_c)
}

// =============================================================================
// Rekey Tests
// =============================================================================

pub fn sodium_rekey_changes_key_id_test() {
  let name_a = "test_sodium_rekey_a"
  let name_b = "test_sodium_rekey_b"
  let options_a = adapter.default_options(name_a)
  let options_b = adapter.default_options(name_b)
  let provider = sodium_adapter.new()

  let assert Ok(handle_a) = { provider.init }(options_a)
  let assert Ok(handle_b) = { provider.init }(options_b)

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"

  // Complete handshake
  let assert Ok(types.Continue(state_a, Some(msg_a))) =
    { provider.handshake_start }(handle_a, node_a, node_b, None)
  let assert Ok(types.Continue(_state_b, Some(msg_b))) =
    { provider.handshake_start }(handle_b, node_b, node_a, Some(msg_a))
  let assert Ok(types.Established(_)) =
    { provider.handshake_continue }(handle_a, state_a, msg_b)

  let assert Some(ctx1) = { provider.secure_context }(handle_a, node_b)
  let key_id1 = types.context_key_id(ctx1)

  // Rekey
  let assert Ok(_) = { provider.rekey }(handle_a, node_b)

  // New context has different key_id
  let assert Some(ctx2) = { provider.secure_context }(handle_a, node_b)
  let key_id2 = types.context_key_id(ctx2)

  should.not_equal(key_id1, key_id2)

  // Cleanup
  let _ = { provider.shutdown }(handle_a)
  let _ = { provider.shutdown }(handle_b)
}

pub fn sodium_rekey_without_context_fails_test() {
  let name = "test_sodium_rekey_fail"
  let options = adapter.default_options(name)
  let provider = sodium_adapter.new()

  let assert Ok(handle) = { provider.init }(options)

  // Rekey without establishing context first
  let result = { provider.rekey }(handle, "unknown_node@localhost")
  should.be_error(result)

  let assert Error(err) = result
  case err {
    types.NoSecureContext(_) -> Nil
    _ -> panic as "Expected NoSecureContext error"
  }

  // Cleanup
  let _ = { provider.shutdown }(handle)
}

// =============================================================================
// Metrics Tests
// =============================================================================

pub fn sodium_metrics_track_operations_test() {
  let name_a = "test_sodium_metrics_a"
  let name_b = "test_sodium_metrics_b"
  let options_a = adapter.default_options(name_a)
  let options_b = adapter.default_options(name_b)
  let provider = sodium_adapter.new()

  let assert Ok(handle_a) = { provider.init }(options_a)
  let assert Ok(handle_b) = { provider.init }(options_b)

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"

  // Initial metrics
  let m1 = { provider.metrics }(handle_a)
  should.equal(m1.handshakes_initiated, 0)
  should.equal(m1.encrypt_count, 0)
  should.equal(m1.decrypt_count, 0)

  // Complete handshake
  let assert Ok(types.Continue(state_a, Some(msg_a))) =
    { provider.handshake_start }(handle_a, node_a, node_b, None)
  let assert Ok(types.Continue(_state_b, Some(msg_b))) =
    { provider.handshake_start }(handle_b, node_b, node_a, Some(msg_a))
  let assert Ok(types.Established(_)) =
    { provider.handshake_continue }(handle_a, state_a, msg_b)

  let assert Some(ctx_a) = { provider.secure_context }(handle_a, node_b)

  // Encrypt/decrypt operations
  let _ = { provider.encrypt }(handle_a, ctx_a, <<"test1">>)
  let _ = { provider.encrypt }(handle_a, ctx_a, <<"test2">>)

  // Check metrics
  let m2 = { provider.metrics }(handle_a)
  should.equal(m2.handshakes_initiated, 1)
  should.equal(m2.handshakes_completed, 1)
  should.equal(m2.encrypt_count, 2)
  should.equal(m2.active_contexts, 1)

  // Cleanup
  let _ = { provider.shutdown }(handle_a)
  let _ = { provider.shutdown }(handle_b)
}

// =============================================================================
// Health Tests
// =============================================================================

pub fn sodium_health_returns_up_test() {
  let name = "test_sodium_health"
  let options = adapter.default_options(name)
  let provider = sodium_adapter.new()

  let assert Ok(handle) = { provider.init }(options)

  let health = { provider.health }(handle)
  should.equal(health, types.Up)

  // Cleanup
  let _ = { provider.shutdown }(handle)
}

// =============================================================================
// Concurrent handshake tests
// =============================================================================

pub fn concurrent_handshakes_test() {
  let n = 4
  let parent = process.new_subject()
  let provider = sodium_adapter.new()

  // Spawn N child processes performing a full handshake
  // Spawn N worker processes using a list-based loop to avoid local recursion
  let ids = list.range(1, n)
  let _ = list.each(ids, fn(i) {
    let name_a = "con_a_" <> int.to_string(i)
    let name_b = "con_b_" <> int.to_string(i)
    let options_a = adapter.default_options(name_a)
    let options_b = adapter.default_options(name_b)
    let assert Ok(handle_a) = { provider.init }(options_a)
    let assert Ok(handle_b) = { provider.init }(options_b)

    let _ = process.spawn(fn() {
      let node_a = name_a
      let node_b = name_b

      case { provider.handshake_start }(handle_a, node_a, node_b, None) {
        Ok(types.Continue(state_a, Some(msg_a))) -> {
          case { provider.handshake_start }(handle_b, node_b, node_a, Some(msg_a)) {
            Ok(types.Continue(_state_b, Some(msg_b))) -> {
              case { provider.handshake_continue }(handle_a, state_a, msg_b) {
                Ok(types.Established(_ctx_a)) -> process.send(parent, <<"ok">>)
                Ok(types.Continue(_, _)) -> process.send(parent, <<"err">>)
                Ok(types.HandshakeError(_)) -> process.send(parent, <<"err">>)
                Error(_) -> process.send(parent, <<"err">>)
              }
            }
            _ -> process.send(parent, <<"err">>)
          }
        }
        _ -> process.send(parent, <<"err">>)
      }
    })
  })

  // Wait for n replies
  let _ = list.range(1, n) |> list.each(fn(_) {
    let assert Ok(msg) = process.receive(parent, 10_000)
    should.equal(msg, <<"ok">>)
  })

  // Note: providers are not explicitly shutdown here for brevity.
}

// =============================================================================
// Import for string.inspect
// =============================================================================
