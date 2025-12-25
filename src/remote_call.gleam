/// Remote procedure calls (RPC) to other nodes.
///
/// This module wraps Erlang's `:rpc` module to call functions on remote nodes.
/// It supports synchronous calls with configurable timeouts.
import gleam/dynamic.{type Dynamic}

pub type RpcError {
  /// The RPC call timed out.
  RpcTimeout
  /// The RPC returned a badrpc error.
  RpcBadRpc(String)
  /// The RPC failed for another reason.
  RpcFailed(String)
}

type Atom

@external(erlang, "rpc_ffi", "call_with_timeout")
fn rpc_call_with_timeout_ffi(
  node: Atom,
  module: Atom,
  function: Atom,
  args: List(Dynamic),
  timeout_ms: Int,
) -> Dynamic

@external(erlang, "rpc_ffi", "to_atom")
fn to_atom(name: String) -> Atom

@external(erlang, "rpc_ffi", "is_badrpc")
fn is_badrpc(value: Dynamic) -> Bool

@external(erlang, "rpc_ffi", "get_badrpc_reason")
fn get_badrpc_reason(value: Dynamic) -> String

/// Perform a remote procedure call on a node.
/// Calls module:function(args) on the given node.
pub fn call(
  node: String,
  module: String,
  function: String,
  args: List(Dynamic),
) -> Result(Dynamic, RpcError) {
  // Defau`module:function(args)` on the given node.
  let res =
    rpc_call_with_timeout_ffi(
      to_atom(node),
      to_atom(module),
      to_atom(function),
      args,
      5000,
    )
  case is_badrpc(res) {
    True -> Error(RpcBadRpc(get_badrpc_reason(res)))
    False -> Ok(res)
  }
}

///
/// Uses a default timeout of 5000ms
/// Call with an explicit timeout in milliseconds
pub fn call_with_timeout(
  node: String,
  module: String,
  function: String,
  args: List(Dynamic),
  timeout_ms: Int,
) -> Result(Dynamic, RpcError) {
  let res =
    rpc_call_with_timeout_ffi(
      to_atom(node),
      to_atom(module),
      to_atom(function),
      args,
      timeout_ms,
    )

  case is_badrpc(res) {
    True -> {
      let reason = get_badrpc_reason(res)
      case reason == "timeout" {
        True -> Error(RpcTimeout)
        False -> Error(RpcBadRpc(reason))
      }
    }
    False -> Ok(res)
  }
}
