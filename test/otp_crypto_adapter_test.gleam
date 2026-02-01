//// Integration tests for the OTP crypto adapter.
////
//// Tests the real cryptographic implementation using Erlang's :crypto module:
//// - X25519 key exchange
//// - ChaCha20-Poly1305 AEAD encryption
//// - HKDF-SHA256 key derivation
//// - Handshake flow with actual crypto
//// - Encrypt/decrypt roundtrip
//// - Rekey functionality

import distribute/crypto/adapter
import distribute/crypto/otp_crypto_adapter
import distribute/crypto/types
import gleam/bit_array
import gleam/int
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// =============================================================================
// Setup Helpers
// =============================================================================

/// Generate a unique provider name for test isolation
fn unique_name(prefix: String) -> String {
  prefix <> "_" <> int.to_string(erlang_unique_integer())
}

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int

/// Run a test with the OTP crypto provider, ensuring cleanup
fn with_otp_crypto_provider(
  name: String,
  test_fn: fn(adapter.CryptoAdapter, types.ProviderHandle) -> Nil,
) -> Result(Nil, String) {
  let provider = otp_crypto_adapter.new()
  let options = adapter.development_options(name)

  case { provider.init }(options) {
    Ok(handle) -> {
      // Run test
      test_fn(provider, handle)
      // Cleanup
      let _ = { provider.shutdown }(handle)
      Ok(Nil)
    }
    Error(err) -> Error("Init failed: " <> string.inspect(err))
  }
}

// =============================================================================
// Lifecycle Tests
// =============================================================================

pub fn otp_crypto_init_and_shutdown_test() {
  let name = unique_name("otp_crypto_lifecycle")
  let provider = otp_crypto_adapter.new()
  let options = adapter.development_options(name)

  // Init should succeed
  let init_result = { provider.init }(options)
  should.be_ok(init_result)

  let assert Ok(handle) = init_result

  // Health should be Up
  let health = { provider.health }(handle)
  health |> should.equal(types.Up)

  // Shutdown should succeed
  let shutdown_result = { provider.shutdown }(handle)
  should.be_ok(shutdown_result)
}

pub fn otp_crypto_health_returns_up_test() {
  let name = unique_name("otp_crypto_health")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let health = { provider.health }(handle)
      health |> should.equal(types.Up)
      Nil
    })
}

pub fn otp_crypto_metrics_start_at_zero_test() {
  let name = unique_name("otp_crypto_metrics")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let metrics = { provider.metrics }(handle)
      metrics.handshakes_initiated |> should.equal(0)
      metrics.handshakes_completed |> should.equal(0)
      metrics.encrypt_count |> should.equal(0)
      metrics.decrypt_count |> should.equal(0)
      Nil
    })
}

// =============================================================================
// Handshake Tests
// =============================================================================

pub fn otp_crypto_handshake_initiator_test() {
  let name = unique_name("otp_crypto_hs_init")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_a@localhost"
      let remote = "node_b@localhost"

      // Start handshake as initiator (no initial message)
      let result = { provider.handshake_start }(handle, local, remote, None)
      should.be_ok(result)

      let assert Ok(hs_result) = result
      case hs_result {
        types.Continue(_state, Some(msg)) -> {
          // Should get a message to send to peer
          msg.message_type |> should.equal("ephemeral_pubkey")
          // Public key should be 32 bytes
          bit_array.byte_size(msg.payload) |> should.equal(32)
        }
        types.Continue(_, None) -> panic as "Expected message, got None"
        types.Established(_) -> panic as "Expected Continue, got Established"
        types.HandshakeError(err) ->
          panic as { "Handshake failed: " <> string.inspect(err) }
      }

      Nil
    })
}

pub fn otp_crypto_handshake_responder_test() {
  let name = unique_name("otp_crypto_hs_resp")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_b@localhost"
      let remote = "node_a@localhost"

      // Simulate receiving initiator's public key
      let fake_pubkey = generate_test_pubkey()
      let initial_msg =
        types.HandshakeMessage(
          message_type: "ephemeral_pubkey",
          payload: fake_pubkey,
          metadata: None,
        )

      // Start handshake as responder (with initial message)
      let result =
        { provider.handshake_start }(handle, local, remote, Some(initial_msg))
      should.be_ok(result)

      let assert Ok(hs_result) = result
      case hs_result {
        types.Continue(_state, msg_opt) -> {
          // Should get our public key to send back (may or may not have message)
          case msg_opt {
            Some(msg) -> {
              // Response message should be 32-byte public key
              bit_array.byte_size(msg.payload) |> should.equal(32)
            }
            None -> {
              // Also acceptable - context established without response needed
              Nil
            }
          }
        }
        types.Established(_ctx) -> {
          // Also acceptable - context established immediately
          Nil
        }
        types.HandshakeError(err) ->
          panic as { "Handshake failed: " <> string.inspect(err) }
      }

      Nil
    })
}

pub fn otp_crypto_full_handshake_flow_test() {
  // Simulate two nodes doing a full handshake
  let name_a = unique_name("otp_crypto_full_a")
  let name_b = unique_name("otp_crypto_full_b")

  let provider_a = otp_crypto_adapter.new()
  let provider_b = otp_crypto_adapter.new()

  let assert Ok(handle_a) =
    { provider_a.init }(adapter.development_options(name_a))
  let assert Ok(handle_b) =
    { provider_b.init }(adapter.development_options(name_b))

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"

  // Step 1: Node A initiates handshake
  let assert Ok(types.Continue(state_a, Some(msg_a_to_b))) =
    { provider_a.handshake_start }(handle_a, node_a, node_b, None)

  // Step 2: Node B receives A's public key and responds
  let assert Ok(result_b) =
    { provider_b.handshake_start }(handle_b, node_b, node_a, Some(msg_a_to_b))

  // Node B should either Continue with response or Establish immediately
  let msg_b_to_a = case result_b {
    types.Continue(_state_b, Some(msg)) -> msg
    types.Established(_ctx) -> {
      // B established, but we still need to complete A's side
      types.HandshakeMessage(
        message_type: "ephemeral_pubkey_response",
        payload: <<0:256>>,
        // Placeholder - in real flow B sends its pubkey
        metadata: None,
      )
    }
    _ -> panic as "Unexpected result from B"
  }

  // Step 3: Node A receives B's public key and completes
  let assert Ok(result_a_final) =
    { provider_a.handshake_continue }(handle_a, state_a, msg_b_to_a)

  case result_a_final {
    types.Established(ctx) -> {
      types.context_node_id(ctx) |> should.equal(node_b)
      types.context_stage(ctx) |> should.equal(types.SecureEstablished)
    }
    _ -> panic as "Expected A to establish context"
  }

  // Cleanup
  let _ = { provider_a.shutdown }(handle_a)
  let _ = { provider_b.shutdown }(handle_b)
}

// =============================================================================
// Encryption/Decryption Tests
// =============================================================================

pub fn otp_crypto_encrypt_decrypt_roundtrip_test() {
  let name = unique_name("otp_crypto_enc_dec")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_a@localhost"
      let remote = "node_b@localhost"

      // First establish a context via handshake (as responder for immediate context)
      let fake_pubkey = generate_test_pubkey()
      let initial_msg =
        types.HandshakeMessage(
          message_type: "ephemeral_pubkey",
          payload: fake_pubkey,
          metadata: None,
        )

      let assert Ok(_hs_result) =
        { provider.handshake_start }(handle, local, remote, Some(initial_msg))

      // Get the context
      let ctx_opt = { provider.secure_context }(handle, remote)
      should.be_some(ctx_opt)
      let assert Some(ctx) = ctx_opt

      // Test encryption/decryption roundtrip
      let plaintext = <<"Hello, distributed world!">>

      let encrypt_result = { provider.encrypt }(handle, ctx, plaintext)
      should.be_ok(encrypt_result)
      let assert Ok(ciphertext) = encrypt_result

      // Ciphertext should be different from plaintext
      should.not_equal(ciphertext, plaintext)

      // Ciphertext should be larger (nonce + tag overhead)
      let plaintext_size = bit_array.byte_size(plaintext)
      let ciphertext_size = bit_array.byte_size(ciphertext)
      // 12 byte nonce (RFC 8439) + 16 byte tag + plaintext
      ciphertext_size |> should.equal(plaintext_size + 12 + 16)

      // Decrypt should return original plaintext
      let decrypt_result = { provider.decrypt }(handle, ctx, ciphertext)
      should.be_ok(decrypt_result)
      let assert Ok(decrypted) = decrypt_result

      decrypted |> should.equal(plaintext)

      Nil
    })
}

pub fn otp_crypto_encrypt_empty_message_test() {
  let name = unique_name("otp_crypto_enc_empty")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_a@localhost"
      let remote = "node_c@localhost"

      // Establish context
      let fake_pubkey = generate_test_pubkey()
      let initial_msg =
        types.HandshakeMessage(
          message_type: "ephemeral_pubkey",
          payload: fake_pubkey,
          metadata: None,
        )
      let assert Ok(_) =
        { provider.handshake_start }(handle, local, remote, Some(initial_msg))

      let assert Some(ctx) = { provider.secure_context }(handle, remote)

      // Encrypt empty message
      let plaintext = <<>>
      let assert Ok(ciphertext) = { provider.encrypt }(handle, ctx, plaintext)

      // Should still have nonce (12 bytes RFC 8439) + tag (16 bytes)
      bit_array.byte_size(ciphertext) |> should.equal(12 + 16)

      // Decrypt should return empty
      let assert Ok(decrypted) = { provider.decrypt }(handle, ctx, ciphertext)
      decrypted |> should.equal(<<>>)

      Nil
    })
}

pub fn otp_crypto_encrypt_large_message_test() {
  let name = unique_name("otp_crypto_enc_large")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_a@localhost"
      let remote = "node_d@localhost"

      // Establish context
      let fake_pubkey = generate_test_pubkey()
      let initial_msg =
        types.HandshakeMessage(
          message_type: "ephemeral_pubkey",
          payload: fake_pubkey,
          metadata: None,
        )
      let assert Ok(_) =
        { provider.handshake_start }(handle, local, remote, Some(initial_msg))

      let assert Some(ctx) = { provider.secure_context }(handle, remote)

      // Large message (64KB)
      let plaintext = generate_random_bytes(65_536)
      let assert Ok(ciphertext) = { provider.encrypt }(handle, ctx, plaintext)
      let assert Ok(decrypted) = { provider.decrypt }(handle, ctx, ciphertext)

      decrypted |> should.equal(plaintext)

      Nil
    })
}

pub fn otp_crypto_decrypt_tampered_ciphertext_fails_test() {
  let name = unique_name("otp_crypto_tamper")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_a@localhost"
      let remote = "node_e@localhost"

      // Establish context
      let fake_pubkey = generate_test_pubkey()
      let initial_msg =
        types.HandshakeMessage(
          message_type: "ephemeral_pubkey",
          payload: fake_pubkey,
          metadata: None,
        )
      let assert Ok(_) =
        { provider.handshake_start }(handle, local, remote, Some(initial_msg))

      let assert Some(ctx) = { provider.secure_context }(handle, remote)

      let plaintext = <<"Secret message">>
      let assert Ok(ciphertext) = { provider.encrypt }(handle, ctx, plaintext)

      // Tamper with ciphertext (flip a bit)
      let tampered = tamper_ciphertext(ciphertext)

      // Decrypt should fail
      let decrypt_result = { provider.decrypt }(handle, ctx, tampered)
      should.be_error(decrypt_result)

      Nil
    })
}

// =============================================================================
// Rekey Tests
// =============================================================================

pub fn otp_crypto_rekey_test() {
  let name = unique_name("otp_crypto_rekey")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_a@localhost"
      let remote = "node_f@localhost"

      // Establish context
      let fake_pubkey = generate_test_pubkey()
      let initial_msg =
        types.HandshakeMessage(
          message_type: "ephemeral_pubkey",
          payload: fake_pubkey,
          metadata: None,
        )
      let assert Ok(_) =
        { provider.handshake_start }(handle, local, remote, Some(initial_msg))

      let assert Some(ctx_before) = { provider.secure_context }(handle, remote)
      let key_id_before = types.context_key_id(ctx_before)

      // Rekey
      let rekey_result = { provider.rekey }(handle, remote)
      should.be_ok(rekey_result)

      // Context should have new key ID
      let assert Some(ctx_after) = { provider.secure_context }(handle, remote)
      let key_id_after = types.context_key_id(ctx_after)

      // Key IDs should be different
      should.not_equal(key_id_before, key_id_after)

      // Can still encrypt/decrypt with new key
      let plaintext = <<"After rekey">>
      let assert Ok(ciphertext) =
        { provider.encrypt }(handle, ctx_after, plaintext)
      let assert Ok(decrypted) =
        { provider.decrypt }(handle, ctx_after, ciphertext)
      decrypted |> should.equal(plaintext)

      Nil
    })
}

pub fn otp_crypto_rekey_nonexistent_node_fails_test() {
  let name = unique_name("otp_crypto_rekey_fail")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      // Try to rekey without establishing context first
      let result = { provider.rekey }(handle, "nonexistent@node")
      should.be_error(result)

      Nil
    })
}

// =============================================================================
// Metrics Tests
// =============================================================================

pub fn otp_crypto_metrics_track_operations_test() {
  let name = unique_name("otp_crypto_metrics_ops")
  let assert Ok(_) =
    with_otp_crypto_provider(name, fn(provider, handle) {
      let local = "node_a@localhost"
      let remote = "node_g@localhost"

      // Check initial metrics
      let m0 = { provider.metrics }(handle)
      m0.handshakes_initiated |> should.equal(0)

      // Establish context
      let fake_pubkey = generate_test_pubkey()
      let initial_msg =
        types.HandshakeMessage(
          message_type: "ephemeral_pubkey",
          payload: fake_pubkey,
          metadata: None,
        )
      let assert Ok(_) =
        { provider.handshake_start }(handle, local, remote, Some(initial_msg))

      let m1 = { provider.metrics }(handle)
      m1.handshakes_initiated |> should.equal(1)

      // Encrypt
      let assert Some(ctx) = { provider.secure_context }(handle, remote)
      let assert Ok(ciphertext) = { provider.encrypt }(handle, ctx, <<"test">>)

      let m2 = { provider.metrics }(handle)
      m2.encrypt_count |> should.equal(1)

      // Decrypt
      let assert Ok(_) = { provider.decrypt }(handle, ctx, ciphertext)

      let m3 = { provider.metrics }(handle)
      m3.decrypt_count |> should.equal(1)

      Nil
    })
}

// =============================================================================
// FFI Helpers
// =============================================================================

@external(erlang, "crypto", "strong_rand_bytes")
fn generate_random_bytes(n: Int) -> BitArray

@external(erlang, "crypto_sodium_ffi", "gen_keypair")
fn gen_keypair_ffi() -> Result(#(BitArray, BitArray), Nil)

fn generate_test_pubkey() -> BitArray {
  case gen_keypair_ffi() {
    Ok(#(pub_key, _priv_key)) -> pub_key
    Error(_) -> {
      // Fallback: generate random 32 bytes
      generate_random_bytes(32)
    }
  }
}

fn tamper_ciphertext(ciphertext: BitArray) -> BitArray {
  // Flip the last byte
  let size = bit_array.byte_size(ciphertext)
  case bit_array.slice(ciphertext, 0, size - 1) {
    Ok(prefix) -> {
      case bit_array.slice(ciphertext, size - 1, 1) {
        Ok(<<last_byte>>) -> {
          let flipped = int.bitwise_exclusive_or(last_byte, 0xFF)
          bit_array.concat([prefix, <<flipped>>])
        }
        _ -> ciphertext
      }
    }
    _ -> ciphertext
  }
}
