/// Cluster management: start a distributed node, connect to others,
/// check health.
import gleam/dynamic
import gleam/list
import gleam/string

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

pub type StartError {
  InvalidNodeName(String)
  AlreadyStarted
  CookieTooLong
  NetworkError(String)
  SystemError(String)
  StartFailed(String)
}

pub type ConnectError {
  ConnectTimeout
  NodeNotFound
  ConnectNetworkError(String)
  ConnectIgnored
}

// ---------------------------------------------------------------------------
// FFI bindings (private)
// ---------------------------------------------------------------------------

@external(erlang, "cluster_ffi", "start_node")
fn start_node_ffi(name: String, cookie: String) -> dynamic.Dynamic

@external(erlang, "cluster_ffi", "connect")
fn connect_ffi(node: String) -> dynamic.Dynamic

@external(erlang, "cluster_ffi", "is_true")
fn is_true_ffi(value: dynamic.Dynamic) -> Bool

@external(erlang, "cluster_ffi", "is_ignored")
fn is_ignored_ffi(value: dynamic.Dynamic) -> Bool

@external(erlang, "cluster_ffi", "nodes")
fn nodes_ffi() -> List(String)

@external(erlang, "cluster_ffi", "self_node")
fn self_node_ffi() -> String

@external(erlang, "cluster_ffi", "ping")
fn ping_ffi(node: String) -> Bool

@external(erlang, "cluster_ffi", "is_ok_atom")
fn is_ok_atom(value: dynamic.Dynamic) -> Bool

@external(erlang, "cluster_ffi", "get_error_reason")
fn get_error_reason(value: dynamic.Dynamic) -> String

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Start a distributed BEAM node.
///
/// `name` must contain `@` (e.g. `"myapp@127.0.0.1"`).
/// `cookie` must be ≤ 255 characters.
pub fn start_node(name: String, cookie: String) -> Result(Nil, StartError) {
  case validate_node_name(name) {
    Error(e) -> Error(e)
    Ok(_) ->
      case validate_cookie(cookie) {
        Error(e) -> Error(e)
        Ok(_) -> {
          let res = start_node_ffi(name, cookie)
          case is_ok_atom(res) {
            True -> Ok(Nil)
            False -> Error(classify_start_reason(get_error_reason(res)))
          }
        }
      }
  }
}

/// Connect to a remote node. Returns `Ok(Nil)` on success.
pub fn connect(node: String) -> Result(Nil, ConnectError) {
  case string.contains(node, "@") {
    False -> Error(NodeNotFound)
    True -> {
      let result = connect_ffi(node)
      case is_true_ffi(result) {
        True -> Ok(Nil)
        False ->
          case is_ignored_ffi(result) {
            True -> Error(ConnectIgnored)
            False -> Error(classify_connect_reason(get_error_reason(result)))
          }
      }
    }
  }
}

/// List all currently connected nodes.
pub fn nodes() -> List(String) {
  nodes_ffi()
}

/// Get the current node's name.
pub fn self_node() -> String {
  self_node_ffi()
}

/// Ping a remote node. Returns `True` if it responds.
pub fn ping(node: String) -> Bool {
  ping_ffi(node)
}

/// Whether this node is running as a distributed node.
pub fn is_distributed() -> Bool {
  self_node() != "nonode@nohost"
}

/// Number of currently connected nodes.
pub fn connected_count() -> Int {
  list.length(nodes())
}

/// Quick health check: distributed and has at least one connection.
pub fn is_healthy() -> Bool {
  is_distributed() && nodes() != []
}

// ---------------------------------------------------------------------------
// Health check
// ---------------------------------------------------------------------------

/// Detailed health status of the cluster from this node's perspective.
pub type ClusterHealth {
  ClusterHealth(
    self_node: String,
    is_distributed: Bool,
    connected_nodes: List(String),
    connected_count: Int,
    reachable_nodes: List(String),
    unreachable_nodes: List(String),
  )
}

/// Perform a cluster health check.
pub fn health() -> ClusterHealth {
  let self = self_node()
  let is_dist = is_distributed()
  case is_dist {
    False ->
      ClusterHealth(
        self_node: self,
        is_distributed: False,
        connected_nodes: [],
        connected_count: 0,
        reachable_nodes: [],
        unreachable_nodes: [],
      )
    True -> {
      let connected = nodes()
      let #(reachable, unreachable) = partition_by_ping(connected)
      ClusterHealth(
        self_node: self,
        is_distributed: True,
        connected_nodes: connected,
        connected_count: list.length(connected),
        reachable_nodes: reachable,
        unreachable_nodes: unreachable,
      )
    }
  }
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

fn validate_node_name(name: String) -> Result(Nil, StartError) {
  case string.contains(name, "@") {
    True -> Ok(Nil)
    False -> Error(InvalidNodeName("Node name must contain '@'"))
  }
}

fn validate_cookie(cookie: String) -> Result(Nil, StartError) {
  case string.length(cookie) > 255 {
    True -> Error(CookieTooLong)
    False -> Ok(Nil)
  }
}

fn classify_start_reason(reason: String) -> StartError {
  case reason {
    "already_started" -> AlreadyStarted
    "invalid_node_name" -> InvalidNodeName("Invalid node name")
    "cookie_too_long" -> CookieTooLong
    _ ->
      case
        string.contains(reason, "network")
        || string.contains(reason, "eaddrinuse")
        || string.contains(reason, "connection")
      {
        True -> NetworkError(reason)
        False ->
          case
            string.contains(reason, "permission")
            || string.contains(reason, "access")
          {
            True -> SystemError(reason)
            False -> StartFailed(reason)
          }
      }
  }
}

fn classify_connect_reason(reason: String) -> ConnectError {
  case string.contains(reason, "timeout") {
    True -> ConnectTimeout
    False -> ConnectNetworkError(reason)
  }
}

fn partition_by_ping(node_list: List(String)) -> #(List(String), List(String)) {
  do_partition(node_list, [], [])
}

fn do_partition(
  remaining: List(String),
  reachable: List(String),
  unreachable: List(String),
) -> #(List(String), List(String)) {
  case remaining {
    [] -> #(list.reverse(reachable), list.reverse(unreachable))
    [node, ..rest] ->
      case ping(node) {
        True -> do_partition(rest, [node, ..reachable], unreachable)
        False -> do_partition(rest, reachable, [node, ..unreachable])
      }
  }
}
