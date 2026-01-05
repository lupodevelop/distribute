//// OTP Crypto adapter for production use.
////
//// This adapter provides real cryptographic security using Erlang's built-in
//// `:crypto` module (OpenSSL/LibreSSL backend):
//// - **X25519** for key exchange (Curve25519 ECDH)
//// - **ChaCha20-Poly1305** for AEAD encryption
//// - **HKDF-SHA256** for key derivation
////
//// Requires no external dependencies beyond standard OTP 22+.
////
//// ## Features
////
//// - **Ephemeral key exchange**: Fresh keys per handshake
//// - **Authenticated encryption**: ChaCha20-Poly1305 AEAD
//// - **Key rotation**: Rekey support with HKDF derivation
//// - **Full metrics**: Tracks all operations
//// - **OTP supervised**: Integrates with supervision trees
////
//// ## Usage
////
//// ```gleam
//// import distribute/crypto/adapter
//// import distribute/crypto/otp_crypto_adapter
////
//// let opts = adapter.default_options("my_crypto")
//// let provider = otp_crypto_adapter.new()
//// let assert Ok(handle) = provider.init(opts)
////
//// // Start handshake with remote node
//// let assert Ok(result) = provider.handshake_start(handle, local, remote, None)
//// ```
////
//// ## Security Notes
////
//// - Key material is never logged
//// - Nonces are randomly generated (24 bytes)
//// - Key IDs are unpredictable
//// - Old keys are discarded on rekey
//// - Uses Erlang `:crypto` (not libsodium) — no native secure zeroing or mlock

import distribute/crypto/adapter.{type CryptoAdapter}
import distribute/crypto/types.{
  type CryptoError, type CryptoMetrics, type HandshakeMessage,
  type HandshakeResult, type HandshakeState, type HealthStatus, type NodeId,
  type ProviderHandle, type ProviderOptions, type SecureContext,
}
import distribute/registry
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification, worker}
import gleam/result
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Commands handled by the sodium adapter actor.
pub opaque type Command {
  HandshakeStart(
    local: NodeId,
    remote: NodeId,
    initial: Option(HandshakeMessage),
    reply: Subject(Result(HandshakeResult, CryptoError)),
  )
  HandshakeContinue(
    state: HandshakeState,
    message: HandshakeMessage,
    reply: Subject(Result(HandshakeResult, CryptoError)),
  )
  GetSecureContext(node: NodeId, reply: Subject(Option(SecureContext)))
  Encrypt(
    ctx: SecureContext,
    plaintext: BitArray,
    reply: Subject(Result(BitArray, CryptoError)),
  )
  Decrypt(
    ctx: SecureContext,
    ciphertext: BitArray,
    reply: Subject(Result(BitArray, CryptoError)),
  )
  Rekey(node: NodeId, reply: Subject(Result(Nil, CryptoError)))
  Health(reply: Subject(HealthStatus))
  Metrics(reply: Subject(CryptoMetrics))
  Shutdown(reply: Subject(Result(Nil, CryptoError)))
}

/// Key material stored in SecureContext.
/// Contains the AEAD key and master secret for rekeying.
type KeyMaterial {
  KeyMaterial(aead_key: BitArray, master_secret: BitArray, rekey_counter: Int)
}

/// Ephemeral keypair for handshake.
type Keypair {
  Keypair(public_key: BitArray, secret_key: BitArray)
}

/// Pending handshake data.
type PendingHandshake {
  PendingHandshake(
    local_node: NodeId,
    remote_node: NodeId,
    our_keypair: Keypair,
    peer_public_key: Option(BitArray),
  )
}

/// Internal state.
type State {
  State(
    name: String,
    options: ProviderOptions,
    contexts: Dict(NodeId, SecureContext),
    pending_handshakes: Dict(NodeId, PendingHandshake),
    next_key_id: Int,
    handshakes_initiated: Int,
    handshakes_completed: Int,
    handshakes_failed: Int,
    encrypt_count: Int,
    decrypt_count: Int,
    rekey_count: Int,
  )
}

// =============================================================================
// Public API
// =============================================================================

/// Create a new sodium crypto adapter.
///
/// This adapter provides real cryptographic security suitable for production.
/// It uses X25519 for key exchange and ChaCha20-Poly1305 for encryption.
pub fn new() -> CryptoAdapter {
  adapter.CryptoAdapter(
    init: sodium_init,
    shutdown: sodium_shutdown,
    handshake_start: sodium_handshake_start,
    handshake_continue: sodium_handshake_continue,
    secure_context: sodium_secure_context,
    encrypt: sodium_encrypt,
    decrypt: sodium_decrypt,
    rekey: sodium_rekey,
    health: sodium_health,
    metrics: sodium_metrics,
  )
}

/// Create a child specification for OTP supervision.
pub fn child_spec(
  options: ProviderOptions,
) -> ChildSpecification(ProviderHandle) {
  worker(fn() { start_link(options) })
}

/// Start and return actor.Started for supervision.
fn start_link(
  options: ProviderOptions,
) -> Result(actor.Started(ProviderHandle), actor.StartError) {
  let initial_state =
    State(
      name: options.name,
      options: options,
      contexts: dict.new(),
      pending_handshakes: dict.new(),
      next_key_id: 1,
      handshakes_initiated: 0,
      handshakes_completed: 0,
      handshakes_failed: 0,
      encrypt_count: 0,
      decrypt_count: 0,
      rekey_count: 0,
    )

  case
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start()
  {
    Ok(started) -> {
      let _ = register_process_by_name(started.pid, options.name)
      let subject = started.data
      let _ = registry.store_subject(options.name, subject)
      let handle = types.new_handle(options.name, wrap_subject(subject))
      Ok(actor.Started(pid: started.pid, data: handle))
    }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// Adapter Implementation
// =============================================================================

fn sodium_init(options: ProviderOptions) -> Result(ProviderHandle, CryptoError) {
  let initial_state =
    State(
      name: options.name,
      options: options,
      contexts: dict.new(),
      pending_handshakes: dict.new(),
      next_key_id: 1,
      handshakes_initiated: 0,
      handshakes_completed: 0,
      handshakes_failed: 0,
      encrypt_count: 0,
      decrypt_count: 0,
      rekey_count: 0,
    )

  case
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start()
  {
    Ok(started) -> {
      let subject = started.data
      let _ = registry.store_subject(options.name, subject)
      Ok(types.new_handle(options.name, wrap_subject(subject)))
    }
    Error(err) ->
      Error(types.InitFailed("Actor start failed: " <> string.inspect(err)))
  }
}

fn sodium_shutdown(handle: ProviderHandle) -> Result(Nil, CryptoError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, Shutdown(reply))
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.ShutdownFailed("Timeout"))
      }
    }
    Error(_) -> {
      // Already shutdown or invalid handle — make shutdown idempotent
      Ok(Nil)
    }
  }
}

fn sodium_handshake_start(
  handle: ProviderHandle,
  local: NodeId,
  remote: NodeId,
  initial: Option(HandshakeMessage),
) -> Result(HandshakeResult, CryptoError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, HandshakeStart(local, remote, initial, reply))
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(err) -> Error(err)
  }
}

fn sodium_handshake_continue(
  handle: ProviderHandle,
  state: HandshakeState,
  message: HandshakeMessage,
) -> Result(HandshakeResult, CryptoError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, HandshakeContinue(state, message, reply))
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(err) -> Error(err)
  }
}

fn sodium_secure_context(
  handle: ProviderHandle,
  node: NodeId,
) -> Option(SecureContext) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, GetSecureContext(node, reply))
      case process.receive(reply, 1000) {
        Ok(ctx) -> ctx
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

fn sodium_encrypt(
  handle: ProviderHandle,
  ctx: SecureContext,
  plaintext: BitArray,
) -> Result(BitArray, CryptoError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, Encrypt(ctx, plaintext, reply))
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(err) -> Error(err)
  }
}

fn sodium_decrypt(
  handle: ProviderHandle,
  ctx: SecureContext,
  ciphertext: BitArray,
) -> Result(BitArray, CryptoError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, Decrypt(ctx, ciphertext, reply))
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(err) -> Error(err)
  }
}

fn sodium_rekey(
  handle: ProviderHandle,
  node: NodeId,
) -> Result(Nil, CryptoError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, Rekey(node, reply))
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(err) -> Error(err)
  }
}

fn sodium_health(handle: ProviderHandle) -> HealthStatus {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, Health(reply))
      case process.receive(reply, 1000) {
        Ok(health) -> health
        Error(_) -> types.Down("Health check timeout")
      }
    }
    Error(_) -> types.Down("Invalid handle")
  }
}

fn sodium_metrics(handle: ProviderHandle) -> CryptoMetrics {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, Metrics(reply))
      case process.receive(reply, 1000) {
        Ok(m) -> m
        Error(_) -> types.empty_metrics()
      }
    }
    Error(_) -> types.empty_metrics()
  }
}

// =============================================================================
// Actor Message Handler
// =============================================================================

fn handle_message(state: State, message: Command) -> actor.Next(State, Command) {
  case message {
    HandshakeStart(local, remote, initial, reply) ->
      handle_handshake_start(state, local, remote, initial, reply)

    HandshakeContinue(hs_state, hs_message, reply) ->
      handle_handshake_continue(state, hs_state, hs_message, reply)

    GetSecureContext(node, reply) -> {
      let ctx = dict.get(state.contexts, node) |> option.from_result
      process.send(reply, ctx)
      actor.continue(state)
    }

    Encrypt(ctx, plaintext, reply) ->
      handle_encrypt(state, ctx, plaintext, reply)

    Decrypt(ctx, ciphertext, reply) ->
      handle_decrypt(state, ctx, ciphertext, reply)

    Rekey(node, reply) -> handle_rekey(state, node, reply)

    Health(reply) -> {
      process.send(reply, types.Up)
      actor.continue(state)
    }

    Metrics(reply) -> {
      let m =
        types.CryptoMetrics(
          handshakes_initiated: state.handshakes_initiated,
          handshakes_completed: state.handshakes_completed,
          handshakes_failed: state.handshakes_failed,
          encrypt_count: state.encrypt_count,
          decrypt_count: state.decrypt_count,
          rekey_count: state.rekey_count,
          active_contexts: dict.size(state.contexts),
        )
      process.send(reply, m)
      actor.continue(state)
    }

    Shutdown(reply) -> {
      // Best-effort: clear references to sensitive data from state by
      // letting the actor stop and GC reclaim state. For stronger zeroing
      // of native buffers implement an FFI zeroing function in the
      // Erlang/Native layer and call it here.
      let _ = registry.remove_stored_subject(state.name)
      process.send(reply, Ok(Nil))
      actor.stop()
    }
  }
}

// =============================================================================
// Handshake Handlers
// =============================================================================

fn handle_handshake_start(
  state: State,
  _local: NodeId,
  remote: NodeId,
  initial: Option(HandshakeMessage),
  reply: Subject(Result(HandshakeResult, CryptoError)),
) -> actor.Next(State, Command) {
  // Generate our ephemeral keypair
  case ffi_gen_keypair() {
    Ok(#(pub_key, priv_key)) -> {
      let our_keypair = Keypair(public_key: pub_key, secret_key: priv_key)

      case initial {
        // We're the initiator: send our public key
        None -> {
          let pending =
            PendingHandshake(
              local_node: remote,
              remote_node: remote,
              our_keypair: our_keypair,
              peer_public_key: None,
            )
          let new_pending =
            dict.insert(state.pending_handshakes, remote, pending)

          // Create handshake state to send to peer
          let hs_state =
            types.new_handshake_state(
              remote,
              remote,
              types.KeyExchangeInProgress,
              wrap_handshake_data(pending),
            )

          // Create message with our public key
          let msg =
            types.HandshakeMessage(
              message_type: "ephemeral_pubkey",
              payload: pub_key,
              metadata: None,
            )

          let new_state =
            State(
              ..state,
              pending_handshakes: new_pending,
              handshakes_initiated: state.handshakes_initiated + 1,
            )

          process.send(reply, Ok(types.Continue(hs_state, Some(msg))))
          actor.continue(new_state)
        }

        // We're the responder: peer sent their public key
        Some(peer_msg) -> {
          let peer_pub_key = peer_msg.payload

          // Compute shared secret
          case ffi_scalarmult(peer_pub_key, priv_key) {
            Ok(shared_secret) -> {
              // Derive AEAD key using HKDF
              case derive_aead_key(shared_secret, remote) {
                Ok(aead_key) -> {
                  let key_id = ffi_generate_key_id()
                  let key_material =
                    KeyMaterial(
                      aead_key: aead_key,
                      master_secret: shared_secret,
                      rekey_counter: 0,
                    )

                  let ctx =
                    types.new_secure_context(
                      remote,
                      types.SecureEstablished,
                      ffi_system_time_ms(),
                      key_id,
                      wrap_key_material(key_material),
                    )

                  let new_contexts = dict.insert(state.contexts, remote, ctx)

                  // Send our public key back
                  let response_msg =
                    types.HandshakeMessage(
                      message_type: "ephemeral_pubkey_response",
                      payload: pub_key,
                      metadata: None,
                    )

                  let hs_state =
                    types.new_handshake_state(
                      remote,
                      remote,
                      types.SecureEstablished,
                      wrap_handshake_data(PendingHandshake(
                        remote,
                        remote,
                        our_keypair,
                        Some(peer_pub_key),
                      )),
                    )

                  let new_state =
                    State(
                      ..state,
                      contexts: new_contexts,
                      handshakes_initiated: state.handshakes_initiated + 1,
                      handshakes_completed: state.handshakes_completed + 1,
                    )

                  process.send(
                    reply,
                    Ok(types.Continue(hs_state, Some(response_msg))),
                  )
                  actor.continue(new_state)
                }
                Error(err) -> {
                  process.send(reply, Error(err))
                  actor.continue(
                    State(
                      ..state,
                      handshakes_failed: state.handshakes_failed + 1,
                    ),
                  )
                }
              }
            }
            Error(err) -> {
              process.send(reply, Error(err))
              actor.continue(
                State(..state, handshakes_failed: state.handshakes_failed + 1),
              )
            }
          }
        }
      }
    }
    Error(err) -> {
      process.send(reply, Error(err))
      actor.continue(
        State(..state, handshakes_failed: state.handshakes_failed + 1),
      )
    }
  }
}

fn handle_handshake_continue(
  state: State,
  hs_state: HandshakeState,
  hs_message: HandshakeMessage,
  reply: Subject(Result(HandshakeResult, CryptoError)),
) -> actor.Next(State, Command) {
  let remote = types.handshake_remote_node(hs_state)

  // Get our pending handshake
  case dict.get(state.pending_handshakes, remote) {
    Ok(pending) -> {
      let peer_pub_key = hs_message.payload
      let our_priv_key = pending.our_keypair.secret_key

      // Compute shared secret
      case ffi_scalarmult(peer_pub_key, our_priv_key) {
        Ok(shared_secret) -> {
          // Derive AEAD key
          case derive_aead_key(shared_secret, remote) {
            Ok(aead_key) -> {
              let key_id = ffi_generate_key_id()
              let key_material =
                KeyMaterial(
                  aead_key: aead_key,
                  master_secret: shared_secret,
                  rekey_counter: 0,
                )

              let ctx =
                types.new_secure_context(
                  remote,
                  types.SecureEstablished,
                  ffi_system_time_ms(),
                  key_id,
                  wrap_key_material(key_material),
                )

              let new_contexts = dict.insert(state.contexts, remote, ctx)
              let new_pending = dict.delete(state.pending_handshakes, remote)

              let new_state =
                State(
                  ..state,
                  contexts: new_contexts,
                  pending_handshakes: new_pending,
                  handshakes_completed: state.handshakes_completed + 1,
                )

              process.send(reply, Ok(types.Established(ctx)))
              actor.continue(new_state)
            }
            Error(err) -> {
              process.send(reply, Error(err))
              actor.continue(
                State(..state, handshakes_failed: state.handshakes_failed + 1),
              )
            }
          }
        }
        Error(err) -> {
          process.send(reply, Error(err))
          actor.continue(
            State(..state, handshakes_failed: state.handshakes_failed + 1),
          )
        }
      }
    }
    Error(_) -> {
      process.send(
        reply,
        Error(types.HandshakeFailed("No pending handshake for node")),
      )
      actor.continue(state)
    }
  }
}

// =============================================================================
// Encrypt/Decrypt Handlers
// =============================================================================

fn handle_encrypt(
  state: State,
  ctx: SecureContext,
  plaintext: BitArray,
  reply: Subject(Result(BitArray, CryptoError)),
) -> actor.Next(State, Command) {
  case unwrap_key_material(types.context_key_material(ctx)) {
    Ok(key_material) -> {
      let nonce = ffi_generate_nonce()
      let aad = <<>>
      // Empty additional authenticated data

      case ffi_aead_encrypt(key_material.aead_key, nonce, aad, plaintext) {
        Ok(ciphertext) -> {
          // Prepend nonce to ciphertext for decryption
          let result = bit_array.concat([nonce, ciphertext])
          process.send(reply, Ok(result))
          actor.continue(State(..state, encrypt_count: state.encrypt_count + 1))
        }
        Error(err) -> {
          process.send(reply, Error(err))
          actor.continue(state)
        }
      }
    }
    Error(_) -> {
      process.send(reply, Error(types.EncryptionFailed("Invalid key material")))
      actor.continue(state)
    }
  }
}

fn handle_decrypt(
  state: State,
  ctx: SecureContext,
  ciphertext_with_nonce: BitArray,
  reply: Subject(Result(BitArray, CryptoError)),
) -> actor.Next(State, Command) {
  case unwrap_key_material(types.context_key_material(ctx)) {
    Ok(key_material) -> {
      // Extract nonce (first 24 bytes) and ciphertext
      case bit_array.slice(ciphertext_with_nonce, 0, 24) {
        Ok(nonce) -> {
          let ciphertext_start = 24
          let ciphertext_len =
            bit_array.byte_size(ciphertext_with_nonce) - ciphertext_start
          case
            bit_array.slice(
              ciphertext_with_nonce,
              ciphertext_start,
              ciphertext_len,
            )
          {
            Ok(ciphertext) -> {
              let aad = <<>>

              case
                ffi_aead_decrypt(key_material.aead_key, nonce, aad, ciphertext)
              {
                Ok(plaintext) -> {
                  process.send(reply, Ok(plaintext))
                  actor.continue(
                    State(..state, decrypt_count: state.decrypt_count + 1),
                  )
                }
                Error(err) -> {
                  process.send(reply, Error(err))
                  actor.continue(state)
                }
              }
            }
            Error(_) -> {
              process.send(
                reply,
                Error(types.DecryptionFailed("Invalid ciphertext format")),
              )
              actor.continue(state)
            }
          }
        }
        Error(_) -> {
          process.send(
            reply,
            Error(types.DecryptionFailed("Ciphertext too short")),
          )
          actor.continue(state)
        }
      }
    }
    Error(_) -> {
      process.send(reply, Error(types.DecryptionFailed("Invalid key material")))
      actor.continue(state)
    }
  }
}

// =============================================================================
// Rekey Handler
// =============================================================================

fn handle_rekey(
  state: State,
  node: NodeId,
  reply: Subject(Result(Nil, CryptoError)),
) -> actor.Next(State, Command) {
  case dict.get(state.contexts, node) {
    Ok(old_ctx) -> {
      case unwrap_key_material(types.context_key_material(old_ctx)) {
        Ok(old_km) -> {
          // Derive new AEAD key using HKDF with counter
          let new_counter = old_km.rekey_counter + 1
          let counter_bytes = int.to_string(new_counter)
          let info = <<"rekey_", counter_bytes:utf8>>

          case ffi_hkdf(<<>>, old_km.master_secret, info, 32) {
            Ok(new_aead_key) -> {
              let new_key_id = ffi_generate_key_id()
              let new_km =
                KeyMaterial(
                  aead_key: new_aead_key,
                  master_secret: old_km.master_secret,
                  rekey_counter: new_counter,
                )

              let new_ctx =
                types.new_secure_context(
                  node,
                  types.SecureEstablished,
                  ffi_system_time_ms(),
                  new_key_id,
                  wrap_key_material(new_km),
                )

              let new_contexts = dict.insert(state.contexts, node, new_ctx)
              let new_state =
                State(
                  ..state,
                  contexts: new_contexts,
                  rekey_count: state.rekey_count + 1,
                )

              process.send(reply, Ok(Nil))
              actor.continue(new_state)
            }
            Error(err) -> {
              process.send(reply, Error(err))
              actor.continue(state)
            }
          }
        }
        Error(_) -> {
          process.send(reply, Error(types.RekeyFailed("Invalid key material")))
          actor.continue(state)
        }
      }
    }
    Error(_) -> {
      process.send(reply, Error(types.NoSecureContext(node)))
      actor.continue(state)
    }
  }
}

// =============================================================================
// Key Derivation
// =============================================================================

fn derive_aead_key(
  shared_secret: BitArray,
  _context: String,
) -> Result(BitArray, CryptoError) {
  // Use a constant info string so both sides derive the same key
  // The shared secret from X25519 DH is already unique per session
  let salt = <<>>
  let info = <<"distribute_aead_key_v1">>
  ffi_hkdf(salt, shared_secret, info, 32)
}

// =============================================================================
// Handle Lookup
// =============================================================================

/// Get a handle to a running provider by name.
pub fn get_handle(name: String) -> Result(ProviderHandle, Nil) {
  case registry.lookup_subject(name) {
    Ok(subject) -> Ok(types.new_handle(name, wrap_subject(subject)))
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// FFI and Helpers
// =============================================================================
// NOTE FOR NATIVE IMPLEMENTORS
// ------------------------------
// This adapter relies on a small native (Erlang/C or NIF/Port) layer named
// `crypto_sodium_ffi` and `crypto_provider_ffi` that must implement the
// functions declared below. Requirements:
// - All native functions MUST return `Result(..., CryptoError)` or `Result(..., Nil)`
//   as declared. Do not raise Erlang exceptions; return Err variants instead.
// - Sensitive buffers (keys, nonces, master secrets) MUST be stored in
//   native-managed memory and securely zeroed when no longer needed.
// - Provide a function to zero wrapped key material (see `zero_key_material`).
// - Ensure NIFs do not block the scheduler > 1-2ms; use dirty schedulers or
//   delegate heavy work to ports/threads if needed.
// - Map native errors to `types.CryptoError` values and never leak raw
//   key material in error messages or logs.
// - Export a stable API and include unit tests for each native function.

// The following `@external` declarations define the expected native functions.
// Implement these in `crypto_sodium_ffi` (Erlang module calling the NIF/port)
// or directly in a C NIF module named accordingly.

fn get_subject(handle: ProviderHandle) -> Result(Subject(Command), CryptoError) {
  let dyn_state = types.handle_state(handle)
  unwrap_subject(dyn_state)
  |> result.replace_error(types.ProviderFailure("Invalid handle state"))
}

@external(erlang, "crypto_sodium_ffi", "gen_keypair")
fn ffi_gen_keypair() -> Result(#(BitArray, BitArray), CryptoError)

@external(erlang, "crypto_sodium_ffi", "scalarmult")
fn ffi_scalarmult(
  peer_pub: BitArray,
  our_priv: BitArray,
) -> Result(BitArray, CryptoError)

@external(erlang, "crypto_sodium_ffi", "aead_encrypt")
fn ffi_aead_encrypt(
  key: BitArray,
  nonce: BitArray,
  aad: BitArray,
  plaintext: BitArray,
) -> Result(BitArray, CryptoError)

@external(erlang, "crypto_sodium_ffi", "aead_decrypt")
fn ffi_aead_decrypt(
  key: BitArray,
  nonce: BitArray,
  aad: BitArray,
  ciphertext: BitArray,
) -> Result(BitArray, CryptoError)

@external(erlang, "crypto_sodium_ffi", "hkdf")
fn ffi_hkdf(
  salt: BitArray,
  ikm: BitArray,
  info: BitArray,
  len: Int,
) -> Result(BitArray, CryptoError)

@external(erlang, "crypto_sodium_ffi", "generate_nonce")
fn ffi_generate_nonce() -> BitArray

@external(erlang, "crypto_sodium_ffi", "generate_key_id")
fn ffi_generate_key_id() -> String

@external(erlang, "crypto_sodium_ffi", "system_time_ms")
fn ffi_system_time_ms() -> Int

@external(erlang, "crypto_provider_ffi", "wrap_subject")
fn wrap_subject(subject: Subject(Command)) -> Dynamic

@external(erlang, "crypto_provider_ffi", "unwrap_subject")
fn unwrap_subject(dynamic: Dynamic) -> Result(Subject(Command), Nil)

// NOTE: Secure-zero functions (sodium_memzero, mlock) are not implemented
// in the OTP :crypto backend. For native secure memory management, implement
// the sodium_adapter using a libsodium NIF.

@external(erlang, "crypto_provider_ffi", "register_process_by_name")
fn register_process_by_name(
  pid: process.Pid,
  name: String,
) -> Result(Nil, CryptoError)

@external(erlang, "crypto_sodium_ffi", "wrap_key_material")
fn wrap_key_material(km: KeyMaterial) -> Dynamic

fn unwrap_key_material(dyn: Dynamic) -> Result(KeyMaterial, Nil) {
  case ffi_unwrap_key_material(dyn) {
    Ok(tuple) -> {
      case tuple {
        #(aead_key, master_secret, counter) ->
          Ok(KeyMaterial(
            aead_key: aead_key,
            master_secret: master_secret,
            rekey_counter: counter,
          ))
      }
    }
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "crypto_sodium_ffi", "unwrap_key_material")
fn ffi_unwrap_key_material(
  dyn: Dynamic,
) -> Result(#(BitArray, BitArray, Int), Nil)

@external(erlang, "crypto_sodium_ffi", "wrap_handshake_data")
fn wrap_handshake_data(pending: PendingHandshake) -> Dynamic
