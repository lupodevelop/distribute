/// Test/dev helpers for starting and communicating with peer BEAM nodes.
///
/// Requires OTP 25+ (`peer` module). Used to write multi-node integration
/// tests without an external cluster.
import gleam/erlang/process

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// An opaque handle to a running peer node.
/// Wraps the Erlang map returned by peer_ffi:start_peer/1 as an uninterpreted
/// phantom type. Callers cannot inspect the internals from Gleam.
pub opaque type Peer {
  Peer(internal: PeerInternal)
}

type PeerInternal

// ---------------------------------------------------------------------------
// FFI (src/peer_ffi.erl)
// ---------------------------------------------------------------------------

@external(erlang, "peer_ffi", "start_peer")
fn start_peer_ffi(name: String) -> Result(PeerInternal, String)

@external(erlang, "peer_ffi", "stop_peer")
fn stop_peer_ffi(internal: PeerInternal) -> Nil

@external(erlang, "peer_ffi", "peer_node")
fn peer_node_ffi(internal: PeerInternal) -> String

@external(erlang, "peer_ffi", "peer_connected_nodes")
fn peer_connected_nodes_ffi(internal: PeerInternal) -> List(String)

@external(erlang, "peer_ffi", "send_to_actor")
fn send_to_actor_ffi(
  internal: PeerInternal,
  actor_name: String,
  encoded: BitArray,
) -> Result(Nil, String)

@external(erlang, "peer_ffi", "send_pid_from_peer")
fn send_pid_from_peer_ffi(
  internal: PeerInternal,
  pid: process.Pid,
  tag: String,
  encoded: BitArray,
) -> Result(Nil, String)

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Start a peer BEAM node. The peer inherits this node's code path and
/// establishes Erlang distribution back to the calling node automatically.
/// `short_name` is the node name before `@host` (e.g. `"peer1"`).
pub fn start(short_name: String) -> Result(Peer, String) {
  case start_peer_ffi(short_name) {
    Ok(internal) -> Ok(Peer(internal:))
    Error(reason) -> Error(reason)
  }
}

/// Stop a peer node. Always call this in test cleanup.
pub fn stop(peer: Peer) -> Nil {
  stop_peer_ffi(peer.internal)
}

/// Get the full node name of a peer (e.g. `"peer1@127.0.0.1"`).
pub fn node(peer: Peer) -> String {
  peer_node_ffi(peer.internal)
}

/// Get the list of Erlang distribution nodes visible from the peer.
/// Useful for asserting the peer is connected to the expected nodes.
pub fn connected_nodes(peer: Peer) -> List(String) {
  peer_connected_nodes_ffi(peer.internal)
}

/// Send a directly-encoded binary to an actor identified by PID and tag,
/// executed on the peer node via the TCP control channel.
///
/// This bypasses `:global` lookup and avoids the sync window race. Use this
/// when you already know the actor's PID (e.g. from `registry.whereis/1`).
pub fn send_pid(
  peer: Peer,
  pid: process.Pid,
  tag: String,
  encoded: BitArray,
) -> Result(Nil, String) {
  send_pid_from_peer_ffi(peer.internal, pid, tag, encoded)
}

/// Send an already-encoded binary to a distribute actor registered under
/// `actor_name` on the peer node, resolved via `:global`.
///
/// Note: performs a `:global.sync()` before lookup to reduce race conditions.
/// Prefer `send_pid/4` when the PID is known.
pub fn send_to_actor(
  peer: Peer,
  actor_name: String,
  encoded: BitArray,
) -> Result(Nil, String) {
  send_to_actor_ffi(peer.internal, actor_name, encoded)
}
