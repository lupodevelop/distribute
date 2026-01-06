/// Node management for BEAM distributed clustering.
///
/// This module provides functions to start a distributed Gleam/Erlang node,
/// connect to other nodes, query connected nodes, and ping remote nodes.
///
/// It wraps the underlying Erlang `net_kernel` and `erlang` modules to provide
/// a type-safe API for cluster management.
import distribute/log
import gleam/string

pub type StartError {
  /// Node name is invalid (must contain '@' and be a valid atom).
  InvalidNodeName(String)
  /// Node is already started as a distributed node.
  AlreadyStarted
  /// Cookie is too long (max 255 characters).
  CookieTooLong
  /// Network-related error (e.g., port binding failure).
  NetworkError(String)
  /// Permission denied or other system error.
  SystemError(String)
  /// Generic startup failure with detailed reason.
  StartFailed(String)
}

/// Error type for connection failures.
pub type ConnectError {
  /// Connection timed out.
  ConnectTimeout
  /// Node not found or unreachable.
  NodeNotFound
  /// Network connectivity issue.
  ConnectNetworkError(String)
  /// Connection was ignored (already connected or self).
  ConnectIgnored
}

/// Opaque value returned by low-level Erlang FFI calls in the
/// `cluster_ffi` module. This type is intentionally untyped because the
/// underlying Erlang implementation may return atoms, tuples or other
/// runtime values. Callers should treat `Dynamic` as an implementation
/// detail and prefer the high-level, type-safe wrappers exported from
/// this module (for example `start_node/2`, `connect/1`).
pub type Dynamic

/// Low-level FFI: start a distributed BEAM node.
///
/// Returns a `Dynamic` value produced by the Erlang side. This binding
/// does no validation and does not classify errors — use `start_node/2`
/// for input validation and structured error handling.
@external(erlang, "cluster_ffi", "start_node")
fn start_node_ffi(name: String, cookie: String) -> Dynamic

/// Low-level FFI: attempt to connect to a remote node.
///
/// Returns a `Dynamic` result that must not be inspected directly by
/// callers; prefer `connect/1` which converts the result to a
/// `Result(Nil, ConnectError)`.
@external(erlang, "cluster_ffi", "connect")
fn connect_ffi(node: String) -> Dynamic

/// Low-level helper: check whether a `Dynamic` result represents a
/// successful outcome. Implementation detail of the FFI layer.
///
/// Returns `True` when the `Dynamic` value encodes a success marker.
@external(erlang, "cluster_ffi", "is_true")
fn is_true_ffi(value: Dynamic) -> Bool

/// Low-level helper: check whether a `Dynamic` connect result should be
/// considered ignored (already connected or a no-op). Used by the FFI.
@external(erlang, "cluster_ffi", "is_ignored")
fn is_ignored_ffi(value: Dynamic) -> Bool

/// Low-level FFI: return the list of currently connected node names.
///
/// This returns a snapshot `List(String)` of known nodes from the
/// Erlang runtime. Prefer the wrapper `nodes/0` which exposes a typed
/// API to the rest of the library.
@external(erlang, "cluster_ffi", "nodes")
fn nodes_ffi() -> List(String)

/// Low-level FFI: return the current node name as a string. Use the
/// typed wrapper `self_node/0` in application code.
@external(erlang, "cluster_ffi", "self_node")
fn self_node_ffi() -> String

/// Low-level FFI: ping a remote node. Returns `True` if the node
/// responded, `False` otherwise. This is a thin wrapper; higher-level
/// code may add retries or timeouts as needed.
@external(erlang, "cluster_ffi", "ping")
fn ping_ffi(node: String) -> Bool

/// Low-level helper: return `True` when the `Dynamic` value is the atom
/// `ok` on the Erlang side. This function is for FFI internals; use the
/// library wrappers for robust error handling.
@external(erlang, "cluster_ffi", "is_ok_atom")
fn is_ok_atom(value: Dynamic) -> Bool

/// Low-level helper: extract the textual error reason from a `Dynamic`
/// failure returned by the Erlang FFI. Wrapper functions should convert
/// this string into structured `StartError` / `ConnectError` values.
@external(erlang, "cluster_ffi", "get_error_reason")
fn get_error_reason(value: Dynamic) -> String

/// Classify error reason into structured StartError
pub fn classify_start_reason(reason: String) -> StartError {
  case reason {
    "already_started" -> AlreadyStarted
    "invalid_node_name" -> InvalidNodeName("Node name must contain '@'")
    "cookie_too_long" -> CookieTooLong
    _ ->
      case is_network_error(reason) {
        True -> NetworkError(reason)
        False ->
          case is_system_error(reason) {
            True -> SystemError(reason)
            False -> StartFailed(reason)
          }
      }
  }
}

/// Convert StartError to string for logging
fn classify_start_error_to_string(error: StartError) -> String {
  case error {
    InvalidNodeName(msg) -> "invalid_node_name: " <> msg
    AlreadyStarted -> "already_started"
    CookieTooLong -> "cookie_too_long"
    NetworkError(msg) -> "network_error: " <> msg
    SystemError(msg) -> "system_error: " <> msg
    StartFailed(msg) -> "start_failed: " <> msg
  }
}

/// Check if error is network-related
fn is_network_error(reason: String) -> Bool {
  string.contains(reason, "network")
  || string.contains(reason, "eaddrinuse")
  || string.contains(reason, "connection")
}

/// Check if error is system/permission related
fn is_system_error(reason: String) -> Bool {
  string.contains(reason, "permission")
  || string.contains(reason, "access")
  || string.contains(reason, "enoent")
}

/// Start distributed node with the given name and cookie.
/// Returns Ok(Nil) on success, Error with specific failure reason.
pub fn start_node(name: String, cookie: String) -> Result(Nil, StartError) {
  log.info("Starting distributed node", [#("node", name)])
  // Pre-validation
  case validate_node_name(name) {
    Error(e) -> {
      log.error("Node name validation failed", [
        #("node", name),
        #("error", classify_start_error_to_string(e)),
      ])
      Error(e)
    }
    Ok(_) ->
      case validate_cookie(cookie) {
        Error(e) -> {
          log.error("Cookie validation failed", [
            #("node", name),
            #("error", classify_start_error_to_string(e)),
          ])
          Error(e)
        }
        Ok(_) -> {
          let res = start_node_ffi(name, cookie)
          case is_ok_atom(res) {
            True -> {
              log.info("Node started successfully", [#("node", name)])
              Ok(Nil)
            }
            False -> {
              let error = classify_start_reason(get_error_reason(res))
              log.error("Failed to start node", [
                #("node", name),
                #("error", classify_start_error_to_string(error)),
              ])
              Error(error)
            }
          }
        }
      }
  }
}

/// Validate node name format
fn validate_node_name(name: String) -> Result(Nil, StartError) {
  case string.contains(name, "@") {
    True -> Ok(Nil)
    False -> Error(InvalidNodeName("Node name must contain '@' symbol"))
  }
}

/// Validate cookie length
fn validate_cookie(cookie: String) -> Result(Nil, StartError) {
  case string.length(cookie) > 255 {
    True -> Error(CookieTooLong)
    False -> Ok(Nil)
  }
}

/// Connect to another distributed node.
/// Returns Ok(Nil) if connected, Error with reason otherwise.
pub fn connect(node: String) -> Result(Nil, ConnectError) {
  // Validate node string early to provide fast, type-safe errors.
  case validate_connect_node(node) {
    Error(e) -> Error(e)
    Ok(_) -> {
      let result = connect_ffi(node)
      case is_true_ffi(result) {
        True -> Ok(Nil)
        False ->
          case is_ignored_ffi(result) {
            True -> Error(ConnectIgnored)
            False -> Error(classify_connect_error(result))
          }
      }
    }
  }
}

/// Connect to another distributed node (legacy API).
/// Returns True if connected successfully, False otherwise.
@deprecated("Use connect() which returns Result instead")
pub fn connect_bool(node: String) -> Bool {
  is_true_ffi(connect_ffi(node))
}

// Validate connect input. Ensures the node looks like an Erlang node
// name (contains an `@`). Returns an immediate typed error if invalid.
fn validate_connect_node(node: String) -> Result(Nil, ConnectError) {
  case string.contains(node, "@") {
    True -> Ok(Nil)
    False -> Error(NodeNotFound)
  }
}

// Classify the `Dynamic` result returned by the FFI into a
// structured `ConnectError`. This keeps all runtime inspection of
// untyped values inside this module and exposes a type-safe API.
pub fn classify_connect_reason(reason: String) -> ConnectError {
  case string.contains(reason, "timeout") {
    True -> ConnectTimeout
    False -> ConnectNetworkError(reason)
  }
}

fn classify_connect_error(value: Dynamic) -> ConnectError {
  case is_ignored_ffi(value) {
    True -> ConnectIgnored
    False -> classify_connect_reason(get_error_reason(value))
  }
}

/// Get the list of currently connected nodes.
pub fn nodes() -> List(String) {
  nodes_ffi()
}

/// Get the name of the current node.
pub fn self_node() -> String {
  self_node_ffi()
}

/// Ping a remote node to check if it is reachable.
/// Returns True if the node responds with pong, False otherwise.
pub fn ping(node: String) -> Bool {
  ping_ffi(node)
}
