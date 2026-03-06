/// Global name registration via Erlang's :global module.
/// TypedName(msg) binds a name to an encoder/decoder pair.
/// Define it once, use it for both registration and lookup.
import distribute/codec
import distribute/global
import distribute/internal/telemetry
import gleam/dynamic
import gleam/erlang/process
import gleam/int
import gleam/string

// ---------------------------------------------------------------------------
// TypedName — compile-time protocol safety
// ---------------------------------------------------------------------------

/// A name bound to an encoder/decoder pair.
/// The msg type links registration and lookup at compile time.
pub opaque type TypedName(msg) {
  TypedName(
    name: String,
    encoder: codec.Encoder(msg),
    decoder: codec.Decoder(msg),
  )
}

/// Create a typed name from separate encoder and decoder.
pub fn typed_name(
  name: String,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> TypedName(msg) {
  TypedName(name:, encoder:, decoder:)
}

/// Create a typed name from a bundled Codec.
pub fn named(name: String, c: codec.Codec(msg)) -> TypedName(msg) {
  TypedName(name:, encoder: c.encoder, decoder: c.decoder)
}

/// Get the name string from a `TypedName`.
pub fn typed_name_to_string(tn: TypedName(msg)) -> String {
  tn.name
}

/// Get the encoder from a `TypedName`.
pub fn typed_name_encoder(tn: TypedName(msg)) -> codec.Encoder(msg) {
  tn.encoder
}

/// Get the decoder from a `TypedName`.
pub fn typed_name_decoder(tn: TypedName(msg)) -> codec.Decoder(msg) {
  tn.decoder
}

/// Derive a pool member name by appending the index.
pub fn pool_member(base: TypedName(msg), index: Int) -> TypedName(msg) {
  TypedName(..base, name: base.name <> "_" <> int.to_string(index))
}

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

/// Errors from registry operations.
pub type RegisterError {
  /// Name is already registered by another process.
  AlreadyExists
  /// Process is not alive or invalid.
  InvalidProcess
  /// Name is empty, too long, or contains invalid characters.
  InvalidArgument(String)
  /// Network partition or connectivity issue.
  NetworkError(String)
  /// Generic registration failure.
  RegistrationFailed(String)
}

// ---------------------------------------------------------------------------
// FFI bindings (private)
// ---------------------------------------------------------------------------

@external(erlang, "registry_ffi", "register")
fn register_ffi(name: String, pid: process.Pid) -> dynamic.Dynamic

@external(erlang, "registry_ffi", "unregister")
fn unregister_ffi(name: String) -> dynamic.Dynamic

@external(erlang, "registry_ffi", "whereis")
fn whereis_ffi(name: String) -> dynamic.Dynamic

@external(erlang, "registry_ffi", "is_ok_atom")
fn is_ok_atom(value: dynamic.Dynamic) -> Bool

@external(erlang, "registry_ffi", "is_already_registered")
fn is_already_registered(value: dynamic.Dynamic) -> Bool

@external(erlang, "registry_ffi", "get_error_reason")
fn get_error_reason(value: dynamic.Dynamic) -> String

@external(erlang, "registry_ffi", "is_pid")
fn is_pid(value: dynamic.Dynamic) -> Bool

@external(erlang, "registry_ffi", "dynamic_to_pid")
fn dynamic_to_pid(value: dynamic.Dynamic) -> process.Pid

@external(erlang, "distribute_ffi_utils", "system_time_ms")
fn system_time_ms() -> Int

// ---------------------------------------------------------------------------
// Core API
// ---------------------------------------------------------------------------

/// Register a PID under a global name.
pub fn register(name: String, pid: process.Pid) -> Result(Nil, RegisterError) {
  case validate_name(name) {
    Error(e) -> Error(e)
    Ok(_) -> {
      let start = telemetry.system_time()
      let res = register_ffi(name, pid)
      let duration = telemetry.system_time() - start
      case is_ok_atom(res) {
        True -> {
          telemetry.emit_registry_register_stop(name, True, duration)
          Ok(Nil)
        }
        False ->
          case is_already_registered(res) {
            True -> {
              telemetry.emit_registry_register_stop(name, False, duration)
              Error(AlreadyExists)
            }
            False -> {
              telemetry.emit_registry_register_stop(name, False, duration)
              Error(classify_error(get_error_reason(res)))
            }
          }
      }
    }
  }
}

/// Register a `Subject`'s owner PID under a global name.
pub fn register_typed(
  name: String,
  subject: process.Subject(msg),
) -> Result(Nil, RegisterError) {
  case process.subject_owner(subject) {
    Ok(pid) -> register(name, pid)
    Error(Nil) -> Error(InvalidProcess)
  }
}

/// Register a GlobalSubject under a typed name.
/// The compiler enforces that both sides share the same msg type.
pub fn register_global(
  tn: TypedName(msg),
  global_subject: global.GlobalSubject(msg),
) -> Result(Nil, RegisterError) {
  register_typed(tn.name, global.subject(global_subject))
}

/// Unregister a global name.
///
/// Always succeeds — unregistering a non-existent name is a no-op.
pub fn unregister(name: String) -> Result(Nil, RegisterError) {
  let _res = unregister_ffi(name)
  Ok(Nil)
}

/// Look up a globally registered PID by name.
pub fn whereis(name: String) -> Result(process.Pid, Nil) {
  let res = whereis_ffi(name)
  case is_pid(res) {
    True -> Ok(dynamic_to_pid(res))
    False -> Error(Nil)
  }
}

/// Look up a globally registered GlobalSubject by TypedName.
/// Reconstructs the Subject with a deterministic tag from the name.
pub fn lookup(tn: TypedName(msg)) -> Result(global.GlobalSubject(msg), Nil) {
  let start = telemetry.system_time()
  case whereis(tn.name) {
    Ok(pid) -> {
      telemetry.emit_registry_lookup_stop(
        tn.name,
        True,
        telemetry.system_time() - start,
      )
      Ok(global.from_name(tn.name, pid, tn.encoder, tn.decoder))
    }
    Error(Nil) -> {
      telemetry.emit_registry_lookup_stop(
        tn.name,
        False,
        telemetry.system_time() - start,
      )
      Error(Nil)
    }
  }
}

/// Check whether a name is currently registered.
pub fn is_registered(name: String) -> Bool {
  case whereis(name) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Look up with polling. Retries every poll_interval_ms until
/// found or timeout_ms elapses.
pub fn lookup_with_timeout(
  tn: TypedName(msg),
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Result(global.GlobalSubject(msg), Nil) {
  let start = system_time_ms()
  do_lookup_with_timeout(tn, start, timeout_ms, poll_interval_ms)
}

fn do_lookup_with_timeout(
  tn: TypedName(msg),
  start: Int,
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Result(global.GlobalSubject(msg), Nil) {
  case lookup(tn) {
    Ok(gs) -> Ok(gs)
    Error(_) -> {
      let elapsed = system_time_ms() - start
      case elapsed >= timeout_ms {
        True -> Error(Nil)
        False -> {
          process.sleep(poll_interval_ms)
          do_lookup_with_timeout(tn, start, timeout_ms, poll_interval_ms)
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

fn validate_name(name: String) -> Result(Nil, RegisterError) {
  case string.length(name) {
    0 -> Error(InvalidArgument("Name cannot be empty"))
    len ->
      case len > 255 {
        True -> Error(InvalidArgument("Name too long (max 255 chars)"))
        False ->
          case string.contains(name, " ") {
            True -> Error(InvalidArgument("Name cannot contain spaces"))
            False -> Ok(Nil)
          }
      }
  }
}

fn classify_error(reason: String) -> RegisterError {
  case reason {
    "already_registered" -> AlreadyExists
    "not_a_pid" -> InvalidProcess
    _ ->
      case
        string.contains(reason, "partition")
        || string.contains(reason, "network")
        || string.contains(reason, "connection")
      {
        True -> NetworkError(reason)
        False -> RegistrationFailed(reason)
      }
  }
}
