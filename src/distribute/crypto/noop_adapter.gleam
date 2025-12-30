//// No-op crypto adapter for development and testing.
////
//// **WARNING: This adapter provides NO ACTUAL ENCRYPTION.**
//// It is intended only for development, testing, and debugging.
//// Never use this adapter in production environments.
////
//// ## Features
////
//// - **Identity encryption**: Returns plaintext unchanged
//// - **Instant handshake**: Completes immediately without key exchange
//// - **Fake secure contexts**: Creates placeholder contexts
//// - **Full metrics**: Tracks operations for testing
////
//// ## Usage
////
//// ```gleam
//// import distribute/crypto/adapter
//// import distribute/crypto/noop_adapter
////
//// let opts = adapter.development_options("test_crypto")
//// let provider = noop_adapter.new()
//// let assert Ok(handle) = provider.init(opts)
////
//// // "Encrypt" (actually just returns the input)
//// let assert Ok(ciphertext) = provider.encrypt(handle, ctx, plaintext)
//// ```

import distribute/crypto/adapter.{type CryptoAdapter}
import distribute/crypto/types.{
  type CryptoError, type CryptoMetrics, type HandshakeMessage,
  type HandshakeResult, type HandshakeState, type HealthStatus, type NodeId,
  type ProviderHandle, type ProviderOptions, type SecureContext,
}
import distribute/registry
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/option.{type Option, None}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification, worker}
import gleam/result
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Commands handled by the noop adapter actor.
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

/// Internal state.
type State {
  State(
    name: String,
    options: ProviderOptions,
    contexts: Dict(NodeId, SecureContext),
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

/// Create a new no-op crypto adapter.
///
/// **Warning:** This adapter provides no real encryption and is
/// marked as a development-only provider.
pub fn new() -> CryptoAdapter {
  adapter.CryptoAdapter(
    init: noop_init,
    shutdown: noop_shutdown,
    handshake_start: noop_handshake_start,
    handshake_continue: noop_handshake_continue,
    secure_context: noop_secure_context,
    encrypt: noop_encrypt,
    decrypt: noop_decrypt,
    rekey: noop_rekey,
    health: noop_health,
    metrics: noop_metrics,
  )
}

/// Create a child specification for OTP supervision.
pub fn child_spec(options: ProviderOptions) -> ChildSpecification(ProviderHandle) {
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

fn noop_init(options: ProviderOptions) -> Result(ProviderHandle, CryptoError) {
  let initial_state =
    State(
      name: options.name,
      options: options,
      contexts: dict.new(),
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

fn noop_shutdown(handle: ProviderHandle) -> Result(Nil, CryptoError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply = process.new_subject()
      process.send(subj, Shutdown(reply))
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.ShutdownFailed("Timeout"))
      }
    }
    Error(err) -> Error(err)
  }
}

fn noop_handshake_start(
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

fn noop_handshake_continue(
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

fn noop_secure_context(
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

fn noop_encrypt(
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

fn noop_decrypt(
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

fn noop_rekey(handle: ProviderHandle, node: NodeId) -> Result(Nil, CryptoError) {
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

fn noop_health(handle: ProviderHandle) -> HealthStatus {
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

fn noop_metrics(handle: ProviderHandle) -> CryptoMetrics {
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
    HandshakeStart(_local, remote, _initial, reply) -> {
      // Noop: immediately establish a fake secure context
      let key_id = "noop_key_" <> int.to_string(state.next_key_id)
      let ctx =
        types.new_secure_context(
          remote,
          types.SecureEstablished,
          system_time_ms(),
          key_id,
          wrap_subject(process.new_subject()),
        )
      let new_contexts = dict.insert(state.contexts, remote, ctx)
      let new_state =
        State(
          ..state,
          contexts: new_contexts,
          next_key_id: state.next_key_id + 1,
          handshakes_initiated: state.handshakes_initiated + 1,
          handshakes_completed: state.handshakes_completed + 1,
        )
      process.send(reply, Ok(types.Established(ctx)))
      actor.continue(new_state)
    }

    HandshakeContinue(_hs_state, _message, reply) -> {
      // Noop: should never be called since we establish immediately
      process.send(reply, Error(types.HandshakeFailed("Unexpected continue")))
      actor.continue(state)
    }

    GetSecureContext(node, reply) -> {
      let ctx = dict.get(state.contexts, node) |> option.from_result
      process.send(reply, ctx)
      actor.continue(state)
    }

    Encrypt(_ctx, plaintext, reply) -> {
      // Noop: return plaintext unchanged
      let new_state = State(..state, encrypt_count: state.encrypt_count + 1)
      process.send(reply, Ok(plaintext))
      actor.continue(new_state)
    }

    Decrypt(_ctx, ciphertext, reply) -> {
      // Noop: return ciphertext unchanged
      let new_state = State(..state, decrypt_count: state.decrypt_count + 1)
      process.send(reply, Ok(ciphertext))
      actor.continue(new_state)
    }

    Rekey(node, reply) -> {
      case dict.get(state.contexts, node) {
        Ok(old_ctx) -> {
          // Create new context with new key_id
          let key_id = "noop_key_" <> int.to_string(state.next_key_id)
          let new_ctx =
            types.new_secure_context(
              node,
              types.SecureEstablished,
              system_time_ms(),
              key_id,
              types.context_key_material(old_ctx),
            )
          let new_contexts = dict.insert(state.contexts, node, new_ctx)
          let new_state =
            State(
              ..state,
              contexts: new_contexts,
              next_key_id: state.next_key_id + 1,
              rekey_count: state.rekey_count + 1,
            )
          process.send(reply, Ok(Nil))
          actor.continue(new_state)
        }
        Error(_) -> {
          process.send(reply, Error(types.NoSecureContext(node)))
          actor.continue(state)
        }
      }
    }

    Health(reply) -> {
      // Noop adapter is always up (but degraded because it's insecure)
      let health = case state.options.is_development {
        True -> types.Degraded("Development mode - no real encryption")
        False -> types.Up
      }
      process.send(reply, health)
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
      // Cleanup
      let _ = registry.remove_stored_subject(state.name)
      process.send(reply, Ok(Nil))
      actor.stop()
    }
  }
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

fn get_subject(handle: ProviderHandle) -> Result(Subject(Command), CryptoError) {
  let dyn_state = types.handle_state(handle)
  unwrap_subject(dyn_state)
  |> result.replace_error(types.ProviderFailure("Invalid handle state"))
}

@external(erlang, "crypto_provider_ffi", "wrap_subject")
fn wrap_subject(subject: Subject(Command)) -> Dynamic

@external(erlang, "crypto_provider_ffi", "unwrap_subject")
fn unwrap_subject(dynamic: Dynamic) -> Result(Subject(Command), Nil)

@external(erlang, "crypto_provider_ffi", "register_process_by_name")
fn register_process_by_name(
  pid: process.Pid,
  name: String,
) -> Result(Nil, CryptoError)

@external(erlang, "erlang", "system_time")
fn system_time_ms() -> Int
