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

import distribute/crypto/otp_crypto_adapter
import distribute/crypto/types
import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import test_helpers

// =============================================================================
// Provider Lifecycle Tests
// =============================================================================

pub fn sodium_init_and_shutdown_test() {
  let name = "test_sodium_lifecycle"
  let assert Ok(_) =
    test_helpers.with_provider_module_checked(
      otp_crypto_adapter.new,
      otp_crypto_adapter.get_handle,
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

pub fn sodium_get_handle_after_init_test() {
  let name = "test_sodium_handle"
  let assert Ok(_) =
    test_helpers.with_provider_module_checked(
      otp_crypto_adapter.new,
      otp_crypto_adapter.get_handle,
      name,
      fn(_provider, _handle) {
        // Get handle by name
        let handle_result = otp_crypto_adapter.get_handle(name)
        should.be_ok(handle_result)

        Nil
      },
    )
}

// =============================================================================
// Handshake Tests
// =============================================================================

pub fn sodium_handshake_initiator_creates_pending_test() {
  let name = "test_sodium_handshake_init"
  let assert Ok(_) =
    test_helpers.with_provider_module_checked(
      otp_crypto_adapter.new,
      otp_crypto_adapter.get_handle,
      name,
      fn(provider, handle) {
        let local = "node_a@localhost"
        let remote = "node_b@localhost"

        // Start handshake as initiator (no initial message)
        let result = { provider.handshake_start }(handle, local, remote, None)
        should.be_ok(result)

        let assert Ok(handshake_result) = result
        case handshake_result {
          types.Continue(state, Some(msg)) -> {
            should.equal(msg.message_type, "ephemeral_pubkey")
            should.equal(bit_array.byte_size(msg.payload), 32)
            types.handshake_stage(state)
            |> should.equal(types.KeyExchangeInProgress)
          }
          types.Continue(_, None) -> panic as "Expected message in Continue"
          types.Established(_) -> panic as "Expected Continue, got Established"
          types.HandshakeError(_) ->
            panic as "Expected Continue, got HandshakeError"
        }

        Nil
      },
    )
}

pub fn sodium_handshake_responder_derives_key_test() {
  let name = "test_sodium_handshake_resp"
  let assert Ok(_) =
    test_helpers.with_provider_module_checked(
      otp_crypto_adapter.new,
      otp_crypto_adapter.get_handle,
      name,
      fn(provider, handle) {
        let local = "node_b@localhost"
        let remote = "node_a@localhost"

        let peer_pub_key = <<
          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
          21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
        >>

        let initial_msg =
          types.HandshakeMessage(
            message_type: "ephemeral_pubkey",
            payload: peer_pub_key,
            metadata: None,
          )

        let result =
          { provider.handshake_start }(handle, local, remote, Some(initial_msg))
        should.be_ok(result)

        let assert Ok(handshake_result) = result
        case handshake_result {
          types.Continue(state, Some(msg)) -> {
            should.equal(msg.message_type, "ephemeral_pubkey_response")
            should.equal(bit_array.byte_size(msg.payload), 32)
            types.handshake_stage(state)
            |> should.equal(types.SecureEstablished)
          }
          types.Continue(_, None) -> panic as "Expected message in Continue"
          types.Established(_) -> Nil
          types.HandshakeError(err) ->
            panic as {
              "Expected Continue, got HandshakeError: " <> string.inspect(err)
            }
        }

        let ctx = { provider.secure_context }(handle, remote)
        should.be_some(ctx)

        Nil
      },
    )
}

pub fn sodium_full_handshake_between_two_providers_test() {
  let name_a = "test_sodium_node_a"
  let name_b = "test_sodium_node_b"
  let assert Ok(_) =
    test_helpers.with_provider_module(
      otp_crypto_adapter.new,
      name_a,
      fn(provider_a, handle_a) {
        let assert Ok(_) =
          test_helpers.with_provider_module_checked(
            otp_crypto_adapter.new,
            otp_crypto_adapter.get_handle,
            name_b,
            fn(provider_b, handle_b) {
              let node_a = "node_a@localhost"
              let node_b = "node_b@localhost"

              // Step 1: A initiates handshake
              let assert Ok(types.Continue(state_a, Some(msg_a))) =
                { provider_a.handshake_start }(handle_a, node_a, node_b, None)

              // Step 2: B receives A's public key, responds with own
              let assert Ok(types.Continue(_state_b, Some(msg_b))) =
                { provider_b.handshake_start }(
                  handle_b,
                  node_b,
                  node_a,
                  Some(msg_a),
                )

              // Step 3: A receives B's response, completes handshake
              let assert Ok(types.Established(ctx_a)) =
                { provider_a.handshake_continue }(handle_a, state_a, msg_b)

              // Both should now have secure contexts
              should.be_some({ provider_a.secure_context }(handle_a, node_b))
              should.be_some({ provider_b.secure_context }(handle_b, node_a))

              types.context_stage(ctx_a)
              |> should.equal(types.SecureEstablished)

              Nil
            },
          )

        Nil
      },
    )
}

// =============================================================================
// Encryption/Decryption Tests
// =============================================================================

pub fn sodium_encrypt_decrypt_roundtrip_test() {
  let name_a = "test_sodium_enc_a"
  let name_b = "test_sodium_enc_b"
  let assert Ok(_) =
    test_helpers.with_provider_module_checked(
      otp_crypto_adapter.new,
      otp_crypto_adapter.get_handle,
      name_a,
      fn(provider_a, handle_a) {
        let assert Ok(_) =
          test_helpers.with_provider_module_checked(
            otp_crypto_adapter.new,
            otp_crypto_adapter.get_handle,
            name_b,
            fn(provider_b, handle_b) {
              let node_a = "node_a@localhost"
              let node_b = "node_b@localhost"

              // Complete handshake
              let assert Ok(types.Continue(state_a, Some(msg_a))) =
                { provider_a.handshake_start }(handle_a, node_a, node_b, None)
              let assert Ok(types.Continue(_state_b, Some(msg_b))) =
                { provider_b.handshake_start }(
                  handle_b,
                  node_b,
                  node_a,
                  Some(msg_a),
                )
              let assert Ok(types.Established(_)) =
                { provider_a.handshake_continue }(handle_a, state_a, msg_b)

              // Get contexts
              let assert Some(ctx_a) =
                { provider_a.secure_context }(handle_a, node_b)
              let assert Some(ctx_b) =
                { provider_b.secure_context }(handle_b, node_a)

              // A encrypts message
              let plaintext = <<"Hello, secure world!">>
              let assert Ok(ciphertext) =
                { provider_a.encrypt }(handle_a, ctx_a, plaintext)

              should.be_true(
                bit_array.byte_size(ciphertext) > bit_array.byte_size(plaintext),
              )

              // B decrypts message
              let assert Ok(decrypted) =
                { provider_b.decrypt }(handle_b, ctx_b, ciphertext)
              should.equal(decrypted, plaintext)

              Nil
            },
          )

        Nil
      },
    )
}

pub fn sodium_encrypt_multiple_messages_test() {
  let name_a = "test_sodium_multi_enc_a"
  let name_b = "test_sodium_multi_enc_b"
  let assert Ok(_) =
    test_helpers.with_provider_module(
      otp_crypto_adapter.new,
      name_a,
      fn(provider_a, handle_a) {
        let assert Ok(_) =
          test_helpers.with_provider_module(
            otp_crypto_adapter.new,
            name_b,
            fn(provider_b, handle_b) {
              let node_a = "node_a@localhost"
              let node_b = "node_b@localhost"

              // Complete handshake
              let assert Ok(types.Continue(state_a, Some(msg_a))) =
                { provider_a.handshake_start }(handle_a, node_a, node_b, None)
              let assert Ok(types.Continue(_state_b, Some(msg_b))) =
                { provider_b.handshake_start }(
                  handle_b,
                  node_b,
                  node_a,
                  Some(msg_a),
                )
              let assert Ok(types.Established(_)) =
                { provider_a.handshake_continue }(handle_a, state_a, msg_b)

              let assert Some(ctx_a) =
                { provider_a.secure_context }(handle_a, node_b)
              let assert Some(ctx_b) =
                { provider_b.secure_context }(handle_b, node_a)

              let messages = [
                <<"Message 1">>,
                <<"Message 2 is longer">>,
                <<1, 2, 3, 4, 5>>,
                <<"Unicode test">>,
              ]

              list.each(messages, fn(msg) {
                let assert Ok(ciphertext) =
                  { provider_a.encrypt }(handle_a, ctx_a, msg)
                let assert Ok(decrypted) =
                  { provider_b.decrypt }(handle_b, ctx_b, ciphertext)
                should.equal(decrypted, msg)
              })

              Nil
            },
          )

        Nil
      },
    )
}

pub fn sodium_decrypt_with_wrong_key_fails_test() {
  let name_a = "test_sodium_wrong_key_a"
  let name_b = "test_sodium_wrong_key_b"
  let name_c = "test_sodium_wrong_key_c"
  let assert Ok(_) =
    test_helpers.with_provider_module(
      otp_crypto_adapter.new,
      name_a,
      fn(provider_a, handle_a) {
        let assert Ok(_) =
          test_helpers.with_provider_module(
            otp_crypto_adapter.new,
            name_b,
            fn(provider_b, handle_b) {
              let assert Ok(_) =
                test_helpers.with_provider_module(
                  otp_crypto_adapter.new,
                  name_c,
                  fn(provider_c, handle_c) {
                    let node_a = "node_a@localhost"
                    let node_b = "node_b@localhost"
                    let node_c = "node_c@localhost"

                    // A <-> B handshake
                    let assert Ok(types.Continue(state_a, Some(msg_a))) =
                      { provider_a.handshake_start }(
                        handle_a,
                        node_a,
                        node_b,
                        None,
                      )
                    let assert Ok(types.Continue(_state_b, Some(msg_b))) =
                      { provider_b.handshake_start }(
                        handle_b,
                        node_b,
                        node_a,
                        Some(msg_a),
                      )
                    let assert Ok(types.Established(_)) =
                      { provider_a.handshake_continue }(
                        handle_a,
                        state_a,
                        msg_b,
                      )

                    // A <-> C handshake (different keys)
                    let assert Ok(types.Continue(state_a2, Some(msg_a2))) =
                      { provider_a.handshake_start }(
                        handle_a,
                        node_a,
                        node_c,
                        None,
                      )
                    let assert Ok(types.Continue(_state_c, Some(msg_c))) =
                      { provider_c.handshake_start }(
                        handle_c,
                        node_c,
                        node_a,
                        Some(msg_a2),
                      )
                    let assert Ok(types.Established(_)) =
                      { provider_a.handshake_continue }(
                        handle_a,
                        state_a2,
                        msg_c,
                      )

                    // A encrypts for B
                    let assert Some(ctx_a_b) =
                      { provider_a.secure_context }(handle_a, node_b)
                    let plaintext = <<"Secret for B">>
                    let assert Ok(ciphertext) =
                      { provider_a.encrypt }(handle_a, ctx_a_b, plaintext)

                    // C tries to decrypt with wrong key - should fail
                    let assert Some(ctx_c) =
                      { provider_c.secure_context }(handle_c, node_a)
                    let result =
                      { provider_c.decrypt }(handle_c, ctx_c, ciphertext)
                    should.be_error(result)

                    Nil
                  },
                )

              Nil
            },
          )

        Nil
      },
    )
}

// =============================================================================
// Rekey Tests
// =============================================================================

pub fn sodium_rekey_changes_key_id_test() {
  let name_a = "test_sodium_rekey_a"
  let name_b = "test_sodium_rekey_b"
  let assert Ok(_) =
    test_helpers.with_provider_module(
      otp_crypto_adapter.new,
      name_a,
      fn(provider_a, handle_a) {
        let assert Ok(_) =
          test_helpers.with_provider_module(
            otp_crypto_adapter.new,
            name_b,
            fn(provider_b, handle_b) {
              let node_a = "node_a@localhost"
              let node_b = "node_b@localhost"

              // Complete handshake
              let assert Ok(types.Continue(state_a, Some(msg_a))) =
                { provider_a.handshake_start }(handle_a, node_a, node_b, None)
              let assert Ok(types.Continue(_state_b, Some(msg_b))) =
                { provider_b.handshake_start }(
                  handle_b,
                  node_b,
                  node_a,
                  Some(msg_a),
                )
              let assert Ok(types.Established(_)) =
                { provider_a.handshake_continue }(handle_a, state_a, msg_b)

              let assert Some(ctx1) =
                { provider_a.secure_context }(handle_a, node_b)
              let key_id1 = types.context_key_id(ctx1)

              // Rekey
              let assert Ok(_) = { provider_a.rekey }(handle_a, node_b)

              let assert Some(ctx2) =
                { provider_a.secure_context }(handle_a, node_b)
              let key_id2 = types.context_key_id(ctx2)

              should.not_equal(key_id1, key_id2)

              Nil
            },
          )

        Nil
      },
    )
}

pub fn sodium_rekey_without_context_fails_test() {
  let name = "test_sodium_rekey_fail"
  let assert Ok(_) =
    test_helpers.with_provider_module(
      otp_crypto_adapter.new,
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

pub fn sodium_metrics_track_operations_test() {
  let name_a = "test_sodium_metrics_a"
  let name_b = "test_sodium_metrics_b"
  let assert Ok(_) =
    test_helpers.with_provider_module(
      otp_crypto_adapter.new,
      name_a,
      fn(provider_a, handle_a) {
        let assert Ok(_) =
          test_helpers.with_provider_module(
            otp_crypto_adapter.new,
            name_b,
            fn(provider_b, handle_b) {
              let node_a = "node_a@localhost"
              let node_b = "node_b@localhost"

              // Initial metrics
              let m1 = { provider_a.metrics }(handle_a)
              should.equal(m1.handshakes_initiated, 0)
              should.equal(m1.encrypt_count, 0)
              should.equal(m1.decrypt_count, 0)

              // Complete handshake
              let assert Ok(types.Continue(state_a, Some(msg_a))) =
                { provider_a.handshake_start }(handle_a, node_a, node_b, None)
              let assert Ok(types.Continue(_state_b, Some(msg_b))) =
                { provider_b.handshake_start }(
                  handle_b,
                  node_b,
                  node_a,
                  Some(msg_a),
                )
              let assert Ok(types.Established(_)) =
                { provider_a.handshake_continue }(handle_a, state_a, msg_b)

              let assert Some(ctx_a) =
                { provider_a.secure_context }(handle_a, node_b)

              // Encrypt/decrypt operations
              let _ = { provider_a.encrypt }(handle_a, ctx_a, <<"test1">>)
              let _ = { provider_a.encrypt }(handle_a, ctx_a, <<"test2">>)

              // Check metrics
              let m2 = { provider_a.metrics }(handle_a)
              should.equal(m2.handshakes_initiated, 1)
              should.equal(m2.handshakes_completed, 1)
              should.equal(m2.encrypt_count, 2)
              should.equal(m2.active_contexts, 1)

              Nil
            },
          )

        Nil
      },
    )
}

// =============================================================================
// Health Tests
// =============================================================================

pub fn sodium_health_returns_up_test() {
  let name = "test_sodium_health"
  let assert Ok(_) =
    test_helpers.with_provider_module(
      otp_crypto_adapter.new,
      name,
      fn(provider, handle) {
        let health = { provider.health }(handle)
        should.equal(health, types.Up)

        Nil
      },
    )
}

// =============================================================================
// Concurrent handshake tests
// =============================================================================

pub fn concurrent_handshakes_test() {
  let n = 4
  let parent = process.new_subject()

  // Spawn N child workflows performing a full handshake
  let ids = list.range(1, n)
  let _ =
    list.each(ids, fn(i) {
      let name_a = "con_a_" <> int.to_string(i)
      let name_b = "con_b_" <> int.to_string(i)

      test_helpers.with_provider_module(
        otp_crypto_adapter.new,
        name_a,
        fn(provider_a, handle_a) {
          test_helpers.with_provider_module(
            otp_crypto_adapter.new,
            name_b,
            fn(provider_b, handle_b) {
              let _ =
                process.spawn(fn() {
                  let node_a = name_a
                  let node_b = name_b

                  let send_and_shutdown = fn(msg, _ha, _hb) {
                    let _ = process.send(parent, msg)
                    Nil
                  }

                  case
                    { provider_a.handshake_start }(
                      handle_a,
                      node_a,
                      node_b,
                      None,
                    )
                  {
                    Ok(types.Continue(state_a, Some(msg_a))) -> {
                      case
                        { provider_b.handshake_start }(
                          handle_b,
                          node_b,
                          node_a,
                          Some(msg_a),
                        )
                      {
                        Ok(types.Continue(_state_b, Some(msg_b))) -> {
                          case
                            { provider_a.handshake_continue }(
                              handle_a,
                              state_a,
                              msg_b,
                            )
                          {
                            Ok(types.Established(_ctx_a)) ->
                              send_and_shutdown(<<"ok">>, handle_a, handle_b)
                            Ok(types.Continue(_, _)) ->
                              send_and_shutdown(<<"err">>, handle_a, handle_b)
                            Ok(types.HandshakeError(_)) ->
                              send_and_shutdown(<<"err">>, handle_a, handle_b)
                            Error(_) ->
                              send_and_shutdown(<<"err">>, handle_a, handle_b)
                          }
                        }
                        _ -> send_and_shutdown(<<"err">>, handle_a, handle_b)
                      }
                    }
                    _ -> send_and_shutdown(<<"err">>, handle_a, handle_b)
                  }
                })

              // Wait for this worker to report back before allowing helper to return and cleanup
              let assert Ok(msg) = process.receive(parent, 10_000)
              should.equal(msg, <<"ok">>)

              Nil
            },
          )
        },
      )
    })
}
// =============================================================================
// Import for string.inspect
// =============================================================================
