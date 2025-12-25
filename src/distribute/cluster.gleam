/// Node management for BEAM distributed clustering.
///
/// This module provides functions to start a distributed Gleam/Erlang node,
/// connect to other nodes, query connected nodes, and ping remote nodes.
///
/// It wraps the underlying Erlang `net_kernel` and `erlang` modules to provide
/// a type-safe API for cluster management.
import gleam/string
import distribute/log

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

type Dynamic

@external(erlang, "cluster_ffi", "start_node")
fn start_node_ffi(name: String, cookie: String) -> Dynamic

@external(erlang, "cluster_ffi", "connect")
fn connect_ffi(node: String) -> Dynamic

@external(erlang, "cluster_ffi", "is_true")
fn is_true_ffi(value: Dynamic) -> Bool

@external(erlang, "cluster_ffi", "is_ignored")
fn is_ignored_ffi(value: Dynamic) -> Bool

@external(erlang, "cluster_ffi", "nodes")
fn nodes_ffi() -> List(String)

@external(erlang, "cluster_ffi", "self_node")
fn self_node_ffi() -> String

@external(erlang, "cluster_ffi", "ping")
fn ping_ffi(node: String) -> Bool

@external(erlang, "cluster_ffi", "is_ok_atom")
fn is_ok_atom(value: Dynamic) -> Bool

@external(erlang, "cluster_ffi", "get_error_reason")
fn get_error_reason(value: Dynamic) -> String

/// Classify error reason into structured StartError
fn classify_start_error(reason: String) -> StartError {
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
              let error = classify_start_error(get_error_reason(res))
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
  let result = connect_ffi(node)
  case is_true_ffi(result) {
    True -> Ok(Nil)
    False ->
      case is_ignored_ffi(result) {
        True -> Error(ConnectIgnored)
        False -> Error(NodeNotFound)
      }
  }
}

/// Connect to another distributed node (legacy API).
/// Returns True if connected successfully, False otherwise.
/// @deprecated Use connect() which returns Result instead
pub fn connect_bool(node: String) -> Bool {
  is_true_ffi(connect_ffi(node))
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
