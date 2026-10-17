//// Real cross-node integration tests using OTP peer nodes.
//// Missing prerequisites fail in CI and in strict local runs. Other local
//// runs print an explicit skip reason. Every started peer is cleaned up
//// even when an assertion fails.

import distribute/registry
import gleam/erlang/process
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

type PeerInternal

@external(erlang, "multinode_real_ffi", "ensure_distribution")
fn ensure_distribution_ffi() -> Result(String, String)

@external(erlang, "multinode_real_ffi", "prerequisite_unavailable")
fn prerequisite_unavailable(
  test_id: String,
  reason: String,
) -> Result(Nil, String)

@external(erlang, "multinode_real_ffi", "with_peer")
fn with_peer(
  test_id: String,
  short_name: String,
  run: fn(PeerInternal) -> Nil,
) -> Result(Nil, String)

@external(erlang, "multinode_real_ffi", "peer_call_register_global")
fn peer_register_global_ffi(
  peer: PeerInternal,
  name: String,
) -> Result(Bool, String)

@external(erlang, "multinode_real_ffi", "peer_call_whereis_global")
fn peer_whereis_global_ffi(
  peer: PeerInternal,
  name: String,
) -> Result(process.Pid, String)

@external(erlang, "multinode_real_ffi", "connect_peers")
fn connect_peers_ffi(a: PeerInternal, b: PeerInternal) -> Result(Nil, String)

@external(erlang, "multinode_real_ffi", "sync_global")
fn sync_global_ffi(peer: PeerInternal) -> Result(Nil, String)

@external(erlang, "multinode_real_ffi", "race_register_global")
fn race_register_global_ffi(
  a: PeerInternal,
  b: PeerInternal,
  name: String,
) -> Result(#(Bool, Bool), String)

fn with_distribution(test_id: String, run: fn() -> Nil) -> Nil {
  let prev = test_helpers.silence_logger()
  let result = ensure_distribution_ffi()
  test_helpers.restore_logger(prev)
  case result {
    Ok(_) -> run()
    Error(reason) -> {
      let assert Ok(Nil) =
        prerequisite_unavailable(
          test_id,
          "distribution unavailable: " <> reason,
        )
      Nil
    }
  }
}

pub fn z2_global_sync_race_resolves_within_window_test() {
  with_distribution("Z2", fn() {
    let id = test_helpers.unique_id()
    let assert Ok(Nil) =
      with_peer("Z2", "z2_peer_" <> id, fn(peer) {
        let name = "z2_name_" <> id
        let assert Ok(True) = peer_register_global_ffi(peer, name)
        let tn = registry.typed_name(name, fn(_) { Ok(<<>>) }, fn(_) { Ok(0) })
        let assert Ok(_) = registry.lookup_with_timeout(tn, 5000, 100)
        Nil
      })
    Nil
  })
}

pub fn z3_cross_node_register_global_picks_one_winner_test() {
  with_distribution("Z3", fn() {
    let id = test_helpers.unique_id()
    let assert Ok(Nil) =
      with_peer("Z3", "z3_p1_" <> id, fn(p1) {
        let assert Ok(Nil) =
          with_peer("Z3", "z3_p2_" <> id, fn(p2) {
            let name = "z3_shared_" <> id
            let assert Ok(Nil) = connect_peers_ffi(p1, p2)

            // Both peer-side contenders report ready before either is released.
            let assert Ok(#(first_won, second_won)) =
              race_register_global_ffi(p1, p2, name)
            should.equal(first_won != second_won, True)

            let assert Ok(Nil) = sync_global_ffi(p1)
            let assert Ok(Nil) = sync_global_ffi(p2)
            let assert Ok(first_view) = peer_whereis_global_ffi(p1, name)
            let assert Ok(second_view) = peer_whereis_global_ffi(p2, name)
            should.equal(first_view, second_view)
          })
        Nil
      })
    Nil
  })
}
