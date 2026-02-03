/// Remote procedure calls (RPC) to other nodes.
///
/// This module wraps Erlang's `:rpc` module to call functions on remote nodes.
/// It supports synchronous calls with configurable timeouts.
///
/// For type-safe RPC, use `call_typed` which requires a `Decoder` and expects
/// the remote function to return envelope-wrapped binary data.
import distribute/codec
import gleam/dynamic.{type Dynamic}

pub type RpcError {
  /// The RPC call timed out.
  RpcTimeout
  /// The RPC returned a badrpc error.
  RpcBadRpc(String)
  /// The RPC failed for another reason.
  RpcFailed(String)
  /// The result could not be decoded.
  DecodeFailed(codec.DecodeError)
  /// The result was not a binary (for typed RPC).
  NotBinary(String)
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

@external(erlang, "rpc_ffi", "call_binary_with_timeout")
fn rpc_call_binary_with_timeout_ffi(
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

@external(erlang, "erlang", "is_binary")
fn is_binary(value: Dynamic) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn to_bitarray(value: Dynamic) -> BitArray

/// Perform a remote procedure call on a node.
/// Calls module:function(args) on the given node.
/// This function bypasses all type checking and encoding validation.
@deprecated("Use call_typed or call_typed_with_timeout with codecs for type-safe RPC")
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

/// Call with an explicit timeout in milliseconds.
/// This function bypasses all type checking and encoding validation.
@deprecated("Use call_typed_with_timeout with codecs for type-safe RPC")
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

// ============================================================================
// Typed RPC API
// ============================================================================

/// Perform a typed remote procedure call on a node.
/// The remote function must return envelope-wrapped binary data.
/// The result is decoded using the provided `Decoder` with tag and version validation.
///
/// Uses a default timeout of 5000ms.
pub fn call_typed(
  node: String,
  module: String,
  function: String,
  args: List(Dynamic),
  decoder: codec.Decoder(a),
  expected_tag: String,
  expected_version: Int,
) -> Result(a, RpcError) {
  call_typed_with_timeout(
    node,
    module,
    function,
    args,
    decoder,
    expected_tag,
    expected_version,
    5000,
  )
}

/// Perform a typed remote procedure call with an explicit timeout.
/// The remote function must return envelope-wrapped binary data.
/// The result is decoded using the provided `Decoder` with tag and version validation.
pub fn call_typed_with_timeout(
  node: String,
  module: String,
  function: String,
  args: List(Dynamic),
  decoder: codec.Decoder(a),
  expected_tag: String,
  expected_version: Int,
  timeout_ms: Int,
) -> Result(a, RpcError) {
  let res =
    rpc_call_binary_with_timeout_ffi(
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
    False -> {
      case is_binary(res) {
        False -> Error(NotBinary("RPC result is not binary"))
        True -> {
          let binary = to_bitarray(res)
          case
            codec.receive_with_decoder(
              decoder,
              expected_tag,
              expected_version,
              binary,
            )
          {
            Ok(value) -> Ok(value)
            Error(decode_error) -> Error(DecodeFailed(decode_error))
          }
        }
      }
    }
  }
}
