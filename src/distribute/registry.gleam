/// Global process registry for cross-node name resolution.
///
/// This module wraps Erlang's `:global` module to provide cluster-wide
/// process registration. Registered names are visible across all connected nodes.
///
/// When a process is registered globally, it can be looked up by name from any
/// node in the cluster. If a network partition occurs, the registry will
/// eventually resolve conflicts when the partition heals.
import gleam/option.{type Option, None, Some}
import gleam/string
import distribute/log

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

type Dynamic

pub type Pid

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
/// Returns Some(pid) if found, None otherwise.
pub fn whereis(name: String) -> Option(Pid) {
  log.debug("Resolving global name", [#("name", name)])
  let res = whereis_ffi(name)
  case is_pid(res) {
    True -> Some(dynamic_to_pid(res))
    False -> None
  }
}
