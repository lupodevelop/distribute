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

@external(erlang, "multinode_real_ffi", "with_peer")
fn with_peer(
  test_id: String,
  name: String,
  run: fn(PeerInternal) -> Nil,
) -> Result(Nil, String)

@external(erlang, "multinode_real_ffi", "ensure_distribution")
fn ensure_distribution() -> Result(String, String)

@external(erlang, "multinode_real_ffi", "prerequisite_unavailable")
fn prerequisite_unavailable(
  test_id: String,
  reason: String,
) -> Result(Nil, String)

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
  let prev = test_helpers.silence_logger()
  let distribution = ensure_distribution()
  test_helpers.restore_logger(prev)
  case distribution {
    Error(reason) -> {
      let assert Ok(Nil) = prerequisite_unavailable("peer roundtrip", reason)
      Nil
    }
    Ok(_) -> {
      let assert Ok(Nil) =
        with_peer(
          "peer roundtrip",
          "peer_" <> test_helpers.unique_id(),
          real_peer_global_lookup_roundtrip,
        )
      Nil
    }
  }
}

fn real_peer_global_lookup_roundtrip(peer: PeerInternal) -> Nil {
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

  let connected = peer_connected_nodes_ffi(peer)
  let assert Ok(encoded) = codec.encode(codec.int_encoder(), 42)
  let send_result = send_to_actor_ffi(peer, name, encoded)
  let received = process.receive(result_subj, 2000)

  let _ = registry.unregister(name)

  should.be_true(
    list.any(connected, fn(node_name) { node_name == cluster.self_node() }),
  )
  should.equal(send_result, Ok(Nil))

  let assert Ok(message) = received
  should.equal(message, 42)
}
