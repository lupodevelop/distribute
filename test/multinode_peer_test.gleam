/// Real peer-node smoke test.
///
/// Unlike `multinode_test.gleam`, this file starts a distributed local node,
/// spawns a real OTP `peer` node, and sends an encoded message across the
/// network path. It is intentionally small: one end-to-end proof that the
/// real two-node path works, without turning the whole suite into a cluster
/// orchestrator.
import distribute/actor
import distribute/cluster
import distribute/codec
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleam/list
import gleeunit
import gleeunit/should
import test_helpers

type PeerInternal

@external(erlang, "multinode_peer_test_ffi", "start_peer")
fn start_peer_ffi(name: String) -> Result(PeerInternal, String)

@external(erlang, "multinode_peer_test_ffi", "stop_peer")
fn stop_peer_ffi(peer: PeerInternal) -> Nil

@external(erlang, "multinode_peer_test_ffi", "peer_connected_nodes")
fn peer_connected_nodes_ffi(peer: PeerInternal) -> List(String)

@external(erlang, "multinode_peer_test_ffi", "send_to_actor")
fn send_to_actor_ffi(
  peer: PeerInternal,
  actor_name: String,
  encoded: BitArray,
) -> Result(Nil, String)

pub fn main() {
  gleeunit.main()
}

pub fn real_peer_global_lookup_roundtrip_test() {
  case check_distributed() {
    False -> Nil
    True -> real_peer_global_lookup_roundtrip()
  }
}

fn real_peer_global_lookup_roundtrip() -> Nil {
  let name = "peer_roundtrip_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let result_subj = process.new_subject()

  let assert Ok(_) =
    actor.start_registered(
      tn,
      0,
      fn(msg, state) {
        process.send(result_subj, msg)
        receiver.Continue(state)
      },
      5000,
    )

  let assert Ok(peer) = start_peer_ffi("peer_" <> test_helpers.unique_id())

  // Give `global` a brief moment to settle before the peer-side lookup.
  process.sleep(100)

  let connected = peer_connected_nodes_ffi(peer)
  let assert Ok(encoded) = codec.encode(codec.int_encoder(), 42)
  let send_result = send_to_actor_ffi(peer, name, encoded)
  let received = process.receive(result_subj, 2000)

  stop_peer_ffi(peer)
  let _ = registry.unregister(name)

  should.be_true(
    list.any(connected, fn(node_name) { node_name == cluster.self_node() }),
  )
  should.equal(send_result, Ok(Nil))

  let assert Ok(message) = received
  should.equal(message, 42)
}

fn check_distributed() -> Bool {
  case cluster.start_node("distribute_test@127.0.0.1", "testcookie") {
    Ok(Nil) -> True
    Error(cluster.AlreadyStarted) -> True
    Error(_) -> False
  }
}
