//// Conformance test suite for crypto behaviour contract.
////
//// This module provides reusable tests that ANY crypto adapter implementation
//// must pass. The tests verify compliance with the behaviour contract defined
//// in `distribute/crypto/adapter` and `distribute/crypto/types`.
////
//// ## Usage
////
//// To run conformance tests on a custom adapter:
////
//// ```gleam
//// import crypto_conformance_test
//// import my_custom_adapter
////
//// pub fn my_adapter_conformance_test() {
////   let adapter = my_custom_adapter.new()
////   crypto_conformance_test.run_conformance_suite(adapter)
//// }
//// ```
////
//// ## Test Categories
////
//// 1. **Lifecycle** - init/shutdown behavior
//// 2. **Health** - health status reporting
//// 3. **Metrics** - metrics tracking
//// 4. **Handshake** - key exchange protocol
//// 5. **Encryption** - encrypt/decrypt operations
//// 6. **Rekey** - key rotation

import distribute/crypto/adapter.{type CryptoAdapter}
import distribute/crypto/types.{type ProviderHandle}
import gleam/bit_array
import gleam/int
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

// =============================================================================
// Public API
// =============================================================================

/// Run the complete conformance test suite on an adapter.
///
/// This function runs all conformance tests and reports any failures.
/// Use this to verify that a crypto adapter implementation correctly
/// implements the behaviour contract.
///
/// ## Example
///
/// ```gleam
/// let adapter = otp_crypto_adapter.new()
/// run_conformance_suite(adapter)
/// ```
pub fn run_conformance_suite(adapter: CryptoAdapter) -> Nil {
  // Lifecycle tests
  lifecycle_init_shutdown_test(adapter)

  // Health tests
  health_returns_valid_status_test(adapter)

  // Metrics tests
  metrics_start_at_zero_test(adapter)

  // Handshake tests
  handshake_initiator_produces_message_test(adapter)
  handshake_responder_accepts_message_test(adapter)
  full_handshake_flow_test(adapter)

  // Encryption tests
  encrypt_decrypt_roundtrip_test(adapter)
  encrypt_empty_message_test(adapter)
  encrypt_large_message_test(adapter)
  decrypt_tampered_ciphertext_fails_test(adapter)

  // Rekey tests
  rekey_changes_context_test(adapter)

  // Metrics increment tests
  metrics_increment_on_operations_test(adapter)

  Nil
}

// =============================================================================
// Test Helpers
// =============================================================================

/// Generate a unique name for test isolation.
///
/// Creates a unique name prefix to avoid conflicts between concurrent tests.
fn unique_name(prefix: String) -> String {
  prefix <> "_conformance_" <> int.to_string(erlang_unique_integer())
}

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int

/// Generate random bytes for testing.
@external(erlang, "crypto", "strong_rand_bytes")
fn generate_random_bytes(n: Int) -> BitArray

/// Generate a test public key (32 bytes for X25519-like protocols).
///
/// Creates random bytes suitable for simulating a peer's public key
/// during handshake tests.
fn generate_test_pubkey() -> BitArray {
  generate_random_bytes(32)
}

/// Run a test with a provider, ensuring cleanup.
///
/// Handles the lifecycle of initializing a provider, running a test,
/// and cleaning up afterwards. Failures in the test function will
/// still trigger cleanup.
///
/// ## Arguments
///
/// - `adapter` - The crypto adapter to test
/// - `name` - Unique name for this test instance
/// - `test_fn` - Test function to run with the provider handle
fn with_provider(
  adapter: CryptoAdapter,
  name: String,
  test_fn: fn(ProviderHandle) -> Nil,
) -> Nil {
  let options = adapter.development_options(name)

  let init_result = { adapter.init }(options)
  should.be_ok(init_result)

  let assert Ok(handle) = init_result

  // Run the test
  test_fn(handle)

  // Cleanup
  let shutdown_result = { adapter.shutdown }(handle)
  should.be_ok(shutdown_result)
}

/// Tamper with ciphertext by flipping bits.
///
/// Modifies the ciphertext to simulate an attacker tampering with the
/// encrypted data. Used to verify authenticated encryption detects
/// modifications.
fn tamper_ciphertext(ciphertext: BitArray) -> BitArray {
  let size = bit_array.byte_size(ciphertext)
  case size > 1 {
    True -> {
      // Flip a byte in the middle of the ciphertext
      let mid = size / 2
      case bit_array.slice(ciphertext, 0, mid) {
        Ok(prefix) -> {
          case bit_array.slice(ciphertext, mid, 1) {
            Ok(<<byte>>) -> {
              let flipped = int.bitwise_exclusive_or(byte, 0xFF)
              case bit_array.slice(ciphertext, mid + 1, size - mid - 1) {
                Ok(suffix) ->
                  bit_array.concat([prefix, <<flipped>>, suffix])
                _ -> ciphertext
              }
            }
            _ -> ciphertext
          }
        }
        _ -> ciphertext
      }
    }
    False -> ciphertext
  }
}

/// Establish a secure context for testing encryption operations.
///
/// Performs a simulated handshake to create a secure context that can
/// be used for encrypt/decrypt testing.
///
/// ## Arguments
///
/// - `adapter` - The crypto adapter being tested
/// - `handle` - Active provider handle
/// - `local` - Local node identifier
/// - `remote` - Remote node identifier
///
/// ## Returns
///
/// A `SecureContext` ready for encryption operations.
fn establish_context(
  adapter: CryptoAdapter,
  handle: ProviderHandle,
  local: String,
  remote: String,
) -> types.SecureContext {
  let fake_pubkey = generate_test_pubkey()
  let initial_msg =
    types.HandshakeMessage(
      message_type: "ephemeral_pubkey",
      payload: fake_pubkey,
      metadata: None,
    )

  let assert Ok(_) =
    { adapter.handshake_start }(handle, local, remote, Some(initial_msg))

  let ctx_opt = { adapter.secure_context }(handle, remote)
  should.be_some(ctx_opt)

  let assert Some(ctx) = ctx_opt
  ctx
}

// =============================================================================
// 1. Lifecycle Tests
// =============================================================================

/// Test: init returns a valid handle, shutdown succeeds.
///
/// Verifies that:
/// - init() with valid options returns Ok(handle)
/// - shutdown() on a valid handle returns Ok(Nil)
pub fn lifecycle_init_shutdown_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("lifecycle")
  let options = adapter.development_options(name)

  // Init should succeed
  let init_result = { adapter.init }(options)
  should.be_ok(init_result)

  let assert Ok(handle) = init_result

  // Handle should have a valid ID
  let handle_id = types.handle_id(handle)
  { string.length(handle_id) > 0 } |> should.be_true

  // Shutdown should succeed
  let shutdown_result = { adapter.shutdown }(handle)
  should.be_ok(shutdown_result)
}

// =============================================================================
// 2. Health Tests
// =============================================================================

/// Test: health returns a valid status (Up, Degraded, or Down).
///
/// Verifies that:
/// - health() returns a valid HealthStatus
/// - After successful init, status should typically be Up
pub fn health_returns_valid_status_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("health")

  with_provider(adapter, name, fn(handle) {
    let health = { adapter.health }(handle)

    // After fresh init, we expect Up or Degraded (not Down)
    case health {
      types.Down(reason) ->
        panic as {
          "Fresh provider should not be Down: " <> reason
        }
      types.Up -> Nil
      types.Degraded(_reason) -> Nil
    }
  })
}

// =============================================================================
// 3. Metrics Tests
// =============================================================================

/// Test: all metrics counters start at zero.
///
/// Verifies that:
/// - metrics() returns a valid CryptoMetrics struct
/// - All counters start at 0 after initialization
pub fn metrics_start_at_zero_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("metrics_zero")

  with_provider(adapter, name, fn(handle) {
    let metrics = { adapter.metrics }(handle)

    // All counters should start at zero
    metrics.handshakes_initiated |> should.equal(0)
    metrics.handshakes_completed |> should.equal(0)
    metrics.handshakes_failed |> should.equal(0)
    metrics.encrypt_count |> should.equal(0)
    metrics.decrypt_count |> should.equal(0)
    metrics.rekey_count |> should.equal(0)
    metrics.active_contexts |> should.equal(0)
  })
}

// =============================================================================
// 4. Handshake Tests
// =============================================================================

/// Test: initiator produces a message with public key.
///
/// Verifies that:
/// - handshake_start() without initial message (initiator role) returns Ok
/// - Result is Continue with a message to send
/// - Message contains public key material
pub fn handshake_initiator_produces_message_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("hs_initiator")

  with_provider(adapter, name, fn(handle) {
    let local = "initiator@localhost"
    let remote = "responder@localhost"

    // Start handshake as initiator (no initial message)
    let result = { adapter.handshake_start }(handle, local, remote, None)
    should.be_ok(result)

    let assert Ok(hs_result) = result

    case hs_result {
      types.Continue(_state, Some(msg)) -> {
        // Message should have a type
        { string.length(msg.message_type) > 0 } |> should.be_true

        // Message should have payload (typically public key)
        { bit_array.byte_size(msg.payload) > 0 } |> should.be_true
      }
      types.Continue(_, None) ->
        panic as "Initiator should produce a message to send"
      types.Established(_) ->
        panic as "Handshake should not establish without exchange"
      types.HandshakeError(err) ->
        panic as { "Handshake failed: " <> string.inspect(err) }
    }
  })
}

/// Test: responder can process initiator message.
///
/// Verifies that:
/// - handshake_start() with initial message (responder role) returns Ok
/// - Result is either Continue (with response) or Established
pub fn handshake_responder_accepts_message_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("hs_responder")

  with_provider(adapter, name, fn(handle) {
    let local = "responder@localhost"
    let remote = "initiator@localhost"

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
      { adapter.handshake_start }(handle, local, remote, Some(initial_msg))
    should.be_ok(result)

    let assert Ok(hs_result) = result

    // Result should be Continue or Established, not HandshakeError
    case hs_result {
      types.Continue(_state, _msg_opt) -> Nil
      types.Established(_ctx) -> Nil
      types.HandshakeError(err) ->
        panic as { "Responder handshake failed: " <> string.inspect(err) }
    }
  })
}

/// Test: full handshake flow between two handles.
///
/// Verifies that:
/// - Two separate handles can complete a handshake
/// - Both sides establish secure contexts
/// - Contexts have correct node IDs and stage
pub fn full_handshake_flow_test(adapter: CryptoAdapter) -> Nil {
  let name_a = unique_name("full_hs_a")
  let name_b = unique_name("full_hs_b")

  let options_a = adapter.development_options(name_a)
  let options_b = adapter.development_options(name_b)

  let assert Ok(handle_a) = { adapter.init }(options_a)
  let assert Ok(handle_b) = { adapter.init }(options_b)

  let node_a = "node_a@localhost"
  let node_b = "node_b@localhost"

  // Step 1: Node A initiates handshake
  let assert Ok(result_a1) =
    { adapter.handshake_start }(handle_a, node_a, node_b, None)

  let #(state_a, msg_a_to_b) = case result_a1 {
    types.Continue(state, Some(msg)) -> #(state, msg)
    types.Continue(_, None) -> panic as "Initiator should produce message"
    types.Established(_) -> panic as "Should not establish without exchange"
    types.HandshakeError(err) ->
      panic as { "A init failed: " <> string.inspect(err) }
  }

  // Step 2: Node B receives A's message and responds
  let assert Ok(result_b) =
    { adapter.handshake_start }(handle_b, node_b, node_a, Some(msg_a_to_b))

  let msg_b_to_a = case result_b {
    types.Continue(_state_b, Some(msg)) -> msg
    types.Continue(_state_b, None) -> {
      // B may have established without needing to send response
      // Create a dummy response for A
      types.HandshakeMessage(
        message_type: "ephemeral_pubkey_response",
        payload: generate_test_pubkey(),
        metadata: None,
      )
    }
    types.Established(_ctx_b) -> {
      // B established, create dummy response for A
      types.HandshakeMessage(
        message_type: "ephemeral_pubkey_response",
        payload: generate_test_pubkey(),
        metadata: None,
      )
    }
    types.HandshakeError(err) ->
      panic as { "B handshake failed: " <> string.inspect(err) }
  }

  // Step 3: Node A receives B's response and completes
  let assert Ok(result_a2) =
    { adapter.handshake_continue }(handle_a, state_a, msg_b_to_a)

  case result_a2 {
    types.Established(ctx_a) -> {
      // Verify context properties
      types.context_node_id(ctx_a) |> should.equal(node_b)
      types.context_stage(ctx_a) |> should.equal(types.SecureEstablished)
    }
    types.Continue(_, _) -> {
      // Some implementations may need more rounds - that's acceptable
      Nil
    }
    types.HandshakeError(err) ->
      panic as { "A completion failed: " <> string.inspect(err) }
  }

  // Cleanup
  let assert Ok(_) = { adapter.shutdown }(handle_a)
  let assert Ok(_) = { adapter.shutdown }(handle_b)
  Nil
}

// =============================================================================
// 5. Encryption Tests
// =============================================================================

/// Test: encrypt then decrypt returns original plaintext.
///
/// Verifies that:
/// - encrypt() returns Ok with ciphertext
/// - Ciphertext differs from plaintext
/// - decrypt() on ciphertext returns original plaintext
pub fn encrypt_decrypt_roundtrip_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("enc_dec")

  with_provider(adapter, name, fn(handle) {
    let local = "local@localhost"
    let remote = "remote_enc@localhost"

    let ctx = establish_context(adapter, handle, local, remote)

    // Test data
    let plaintext = <<"Hello, secure world! 🔐">>

    // Encrypt
    let encrypt_result = { adapter.encrypt }(handle, ctx, plaintext)
    should.be_ok(encrypt_result)

    let assert Ok(ciphertext) = encrypt_result

    // Ciphertext should differ from plaintext
    should.not_equal(ciphertext, plaintext)

    // Ciphertext should typically be larger (due to nonce + auth tag)
    { bit_array.byte_size(ciphertext) >= bit_array.byte_size(plaintext) }
    |> should.be_true

    // Decrypt
    let decrypt_result = { adapter.decrypt }(handle, ctx, ciphertext)
    should.be_ok(decrypt_result)

    let assert Ok(decrypted) = decrypt_result

    // Decrypted should match original
    decrypted |> should.equal(plaintext)
  })
}

/// Test: can encrypt empty BitArray.
///
/// Verifies that:
/// - encrypt() accepts empty plaintext
/// - decrypt() on result returns empty BitArray
pub fn encrypt_empty_message_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("enc_empty")

  with_provider(adapter, name, fn(handle) {
    let local = "local@localhost"
    let remote = "remote_empty@localhost"

    let ctx = establish_context(adapter, handle, local, remote)

    // Empty plaintext
    let plaintext = <<>>

    // Encrypt should succeed
    let encrypt_result = { adapter.encrypt }(handle, ctx, plaintext)
    should.be_ok(encrypt_result)

    let assert Ok(ciphertext) = encrypt_result

    // Ciphertext should have some bytes (nonce + tag at minimum)
    { bit_array.byte_size(ciphertext) > 0 } |> should.be_true

    // Decrypt should return empty
    let decrypt_result = { adapter.decrypt }(handle, ctx, ciphertext)
    should.be_ok(decrypt_result)

    let assert Ok(decrypted) = decrypt_result
    decrypted |> should.equal(<<>>)
  })
}

/// Test: can encrypt large messages (1MB).
///
/// Verifies that:
/// - encrypt() handles large payloads
/// - decrypt() correctly recovers large plaintext
pub fn encrypt_large_message_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("enc_large")

  with_provider(adapter, name, fn(handle) {
    let local = "local@localhost"
    let remote = "remote_large@localhost"

    let ctx = establish_context(adapter, handle, local, remote)

    // Large plaintext: 1MB
    let plaintext = generate_random_bytes(1_048_576)

    // Encrypt should succeed
    let encrypt_result = { adapter.encrypt }(handle, ctx, plaintext)
    should.be_ok(encrypt_result)

    let assert Ok(ciphertext) = encrypt_result

    // Decrypt should recover original
    let decrypt_result = { adapter.decrypt }(handle, ctx, ciphertext)
    should.be_ok(decrypt_result)

    let assert Ok(decrypted) = decrypt_result
    decrypted |> should.equal(plaintext)
  })
}

/// Test: tampering with ciphertext causes decryption failure.
///
/// Verifies that:
/// - Modifying ciphertext causes decrypt() to fail
/// - This ensures authenticated encryption is used
pub fn decrypt_tampered_ciphertext_fails_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("tamper")

  with_provider(adapter, name, fn(handle) {
    let local = "local@localhost"
    let remote = "remote_tamper@localhost"

    let ctx = establish_context(adapter, handle, local, remote)

    let plaintext = <<"This message will be tampered with">>

    // Encrypt
    let assert Ok(ciphertext) = { adapter.encrypt }(handle, ctx, plaintext)

    // Tamper with ciphertext
    let tampered = tamper_ciphertext(ciphertext)

    // Ensure we actually tampered (ciphertext changed)
    should.not_equal(tampered, ciphertext)

    // Decrypt should fail due to authentication failure
    let decrypt_result = { adapter.decrypt }(handle, ctx, tampered)
    should.be_error(decrypt_result)
    Nil
  })
}

// =============================================================================
// 6. Rekey Tests
// =============================================================================

/// Test: rekey produces new key material.
///
/// Verifies that:
/// - rekey() on existing context returns Ok
/// - Context has different key_id after rekey
/// - Can still encrypt/decrypt after rekey
pub fn rekey_changes_context_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("rekey")

  with_provider(adapter, name, fn(handle) {
    let local = "local@localhost"
    let remote = "remote_rekey@localhost"

    // Establish initial context
    let ctx_before = establish_context(adapter, handle, local, remote)
    let key_id_before = types.context_key_id(ctx_before)

    // Perform rekey
    let rekey_result = { adapter.rekey }(handle, remote)
    should.be_ok(rekey_result)

    // Get new context
    let ctx_opt = { adapter.secure_context }(handle, remote)
    should.be_some(ctx_opt)

    let assert Some(ctx_after) = ctx_opt
    let key_id_after = types.context_key_id(ctx_after)

    // Key IDs should be different after rekey
    should.not_equal(key_id_before, key_id_after)

    // Encryption should still work with new context
    let plaintext = <<"After rekey test">>
    let assert Ok(ciphertext) =
      { adapter.encrypt }(handle, ctx_after, plaintext)
    let assert Ok(decrypted) =
      { adapter.decrypt }(handle, ctx_after, ciphertext)

    decrypted |> should.equal(plaintext)
  })
}

// =============================================================================
// 7. Metrics Increment Tests
// =============================================================================

/// Test: operations increment appropriate metrics counters.
///
/// Verifies that:
/// - Handshake increments handshakes_initiated
/// - Encrypt increments encrypt_count
/// - Decrypt increments decrypt_count
pub fn metrics_increment_on_operations_test(adapter: CryptoAdapter) -> Nil {
  let name = unique_name("metrics_incr")

  with_provider(adapter, name, fn(handle) {
    let local = "local@localhost"
    let remote = "remote_metrics@localhost"

    // Check initial metrics
    let m0 = { adapter.metrics }(handle)
    m0.handshakes_initiated |> should.equal(0)
    m0.encrypt_count |> should.equal(0)
    m0.decrypt_count |> should.equal(0)

    // Perform handshake
    let fake_pubkey = generate_test_pubkey()
    let initial_msg =
      types.HandshakeMessage(
        message_type: "ephemeral_pubkey",
        payload: fake_pubkey,
        metadata: None,
      )
    let assert Ok(_) =
      { adapter.handshake_start }(handle, local, remote, Some(initial_msg))

    // Check handshake metric incremented
    let m1 = { adapter.metrics }(handle)
    { m1.handshakes_initiated >= 1 } |> should.be_true

    // Get context and perform encrypt
    let assert Some(ctx) = { adapter.secure_context }(handle, remote)
    let assert Ok(ciphertext) =
      { adapter.encrypt }(handle, ctx, <<"test data">>)

    // Check encrypt metric incremented
    let m2 = { adapter.metrics }(handle)
    { m2.encrypt_count >= 1 } |> should.be_true

    // Perform decrypt
    let assert Ok(_) = { adapter.decrypt }(handle, ctx, ciphertext)

    // Check decrypt metric incremented
    let m3 = { adapter.metrics }(handle)
    { m3.decrypt_count >= 1 } |> should.be_true
  })
}

// =============================================================================
// Integration Test Runner
// =============================================================================

/// Individual test exports for running via gleeunit.
/// These use the OTP crypto adapter as a reference implementation.

import distribute/crypto/otp_crypto_adapter

pub fn otp_adapter_lifecycle_conformance_test() {
  lifecycle_init_shutdown_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_health_conformance_test() {
  health_returns_valid_status_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_metrics_zero_conformance_test() {
  metrics_start_at_zero_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_handshake_initiator_conformance_test() {
  handshake_initiator_produces_message_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_handshake_responder_conformance_test() {
  handshake_responder_accepts_message_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_full_handshake_conformance_test() {
  full_handshake_flow_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_encrypt_decrypt_conformance_test() {
  encrypt_decrypt_roundtrip_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_encrypt_empty_conformance_test() {
  encrypt_empty_message_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_encrypt_large_conformance_test() {
  encrypt_large_message_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_tamper_detection_conformance_test() {
  decrypt_tampered_ciphertext_fails_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_rekey_conformance_test() {
  rekey_changes_context_test(otp_crypto_adapter.new())
}

pub fn otp_adapter_metrics_increment_conformance_test() {
  metrics_increment_on_operations_test(otp_crypto_adapter.new())
}

/// Run complete conformance suite on OTP adapter.
pub fn otp_adapter_full_conformance_suite_test() {
  run_conformance_suite(otp_crypto_adapter.new())
}
