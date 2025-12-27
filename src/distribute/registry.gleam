/// Global process registry for cross-node name resolution.
///
/// This module wraps Erlang's `:global` module to provide cluster-wide
/// process registration. Registered names are visible across all connected nodes.
///
/// When a process is registered globally, it can be looked up by name from any
/// node in the cluster. If a network partition occurs, the registry will
/// eventually resolve conflicts when the partition heals.
import distribute/log
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/string

/// Re-export Pid from gleam/erlang/process for API compatibility.
/// Use `gleam/erlang/process.Pid` directly in new code.
pub type Pid =
  process.Pid

pub type RegisterError {
  /// Name is already registered by another process.
  AlreadyRegistered
  /// Process is not alive or invalid.
  InvalidProcess
  /// Name contains invalid characters or is too long.
  InvalidName(String)
  /// Network partition or connectivity issue.
  NetworkError(String)
  /// Generic registration failure.
  RegisterFailed(String)
}

@external(erlang, "registry_ffi", "register")
fn register_ffi(name: String, pid: Pid) -> Dynamic

@external(erlang, "registry_ffi", "unregister")
fn unregister_ffi(name: String) -> Dynamic

@external(erlang, "registry_ffi", "whereis")
fn whereis_ffi(name: String) -> Dynamic

@external(erlang, "registry_ffi", "is_ok_atom")
fn is_ok_atom(value: Dynamic) -> Bool

@external(erlang, "registry_ffi", "is_already_registered")
fn is_already_registered(value: Dynamic) -> Bool

@external(erlang, "registry_ffi", "get_error_reason")
fn get_error_reason(value: Dynamic) -> String

@external(erlang, "registry_ffi", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "registry_ffi", "dynamic_to_pid")
fn dynamic_to_pid(value: Dynamic) -> Pid

/// Classify error reason into structured RegisterError
fn classify_register_error(reason: String) -> RegisterError {
  case reason {
    "already_registered" -> AlreadyRegistered
    "invalid_process" -> InvalidProcess
    "invalid_name" -> InvalidName("Name contains invalid characters")
    _ ->
      case is_network_error(reason) {
        True -> NetworkError(reason)
        False -> RegisterFailed(reason)
      }
  }
}

/// Convert RegisterError to string for logging
fn classify_register_error_to_string(error: RegisterError) -> String {
  case error {
    AlreadyRegistered -> "already_registered"
    InvalidProcess -> "invalid_process"
    InvalidName(msg) -> "invalid_name: " <> msg
    NetworkError(msg) -> "network_error: " <> msg
    RegisterFailed(msg) -> "register_failed: " <> msg
  }
}

/// Check if error is network-related
fn is_network_error(reason: String) -> Bool {
  string.contains(reason, "partition")
  || string.contains(reason, "network")
  || string.contains(reason, "connection")
}

/// Validate registry name
fn validate_name(name: String) -> Result(Nil, RegisterError) {
  case string.length(name) {
    0 -> Error(InvalidName("Name cannot be empty"))
    len if len > 255 -> Error(InvalidName("Name too long (max 255 chars)"))
    _ ->
      case string.contains(name, " ") {
        True -> Error(InvalidName("Name cannot contain spaces"))
        False -> Ok(Nil)
      }
  }
}

/// Register a typed Subject globally.
///
/// This registers the owner Pid of the subject.
///
/// ⚠️ **WARNING**: This function discards the Subject's tag.
/// The `Subject` returned by `whereis_typed` will have a `Nil` tag.
/// Standard `gleam_otp` actors (started via `actor.start`) require a specific
/// tag and will **NOT** receive messages sent to the Subject returned by `whereis_typed`.
///
/// Use this only for processes that accept messages with a `Nil` tag,
/// such as those started with `receiver.start_global_receiver`.
pub fn register_typed(
  name: String,
  subject: process.Subject(msg),
) -> Result(Nil, RegisterError) {
  case process.subject_owner(subject) {
    Ok(pid) -> register(name, pid)
    Error(Nil) -> Error(InvalidProcess)
  }
}

/// Register a Subject globally (alias for register_typed).
@deprecated("Use register_typed instead")
pub fn register_subject(
  name: String,
  subject: process.Subject(a),
) -> Result(Nil, RegisterError) {
  register_typed(name, subject)
}

/// Register a process globally under the given name.
pub fn register(name: String, pid: Pid) -> Result(Nil, RegisterError) {
  log.debug("Registering global name", [#("name", name)])
  case validate_name(name) {
    Error(e) -> {
      log.warn("Name validation failed during registration", [
        #("name", name),
        #("error", classify_register_error_to_string(e)),
      ])
      Error(e)
    }
    Ok(_) -> {
      let res = register_ffi(name, pid)
      case is_ok_atom(res) {
        True -> {
          log.info("Successfully registered global name", [#("name", name)])
          Ok(Nil)
        }
        False ->
          case is_already_registered(res) {
            True -> {
              log.warn("Name already registered", [#("name", name)])
              Error(AlreadyRegistered)
            }
            False -> {
              let error = classify_register_error(get_error_reason(res))
              log.error("Failed to register global name", [
                #("name", name),
                #("error", classify_register_error_to_string(error)),
              ])
              Error(error)
            }
          }
      }
    }
  }
}

/// Unregister a globally registered name.
pub fn unregister(name: String) -> Result(Nil, RegisterError) {
  let res = unregister_ffi(name)
  case is_ok_atom(res) {
    True -> Ok(Nil)
    False -> Error(classify_register_error(get_error_reason(res)))
  }
}

/// Look up a globally registered process by name.
/// Returns Ok(pid) if found, Error(Nil) otherwise.
pub fn whereis(name: String) -> Result(Pid, Nil) {
  log.debug("Resolving global name", [#("name", name)])
  let res = whereis_ffi(name)
  case is_pid(res) {
    True -> Ok(dynamic_to_pid(res))
    False -> Error(Nil)
  }
}

/// Look up a globally registered process and return a typed Subject.
///
/// The returned Subject will have a `Nil` tag.
/// Ensure the target process accepts messages with a `Nil` tag.
pub fn whereis_typed(name: String) -> Result(process.Subject(msg), Nil) {
  case whereis(name) {
    Ok(pid) -> Ok(process.unsafely_create_subject(pid, dynamic.nil()))
    Error(Nil) -> Error(Nil)
  }
}

/// Safer variant of `whereis_typed` that returns a typed `Result` with a
/// `RegisterError` on failure. This makes it easier for callers to handle
/// lookup failures in a type-safe way.
pub fn whereis_subject(name: String) -> Result(process.Subject(msg), RegisterError) {
  case whereis(name) {
    Ok(pid) -> Ok(process.unsafely_create_subject(pid, dynamic.nil()))
    Error(Nil) -> Error(InvalidProcess)
  }
}
