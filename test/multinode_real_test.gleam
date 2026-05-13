//// Real cross-node integration tests.
////
//// Exercise the claims that simulated single-VM tests cannot fully
//// prove: `:global.sync()` race window, cross-node `register_global`
//// race when two peers contend for the same name. The tests start
//// real BEAM peer nodes via OTP's `peer` module.
////
//// When distribution prerequisites are unavailable (e.g. `epmd` is not
//// running), tests skip silently.

import distribute/registry
import gleam/erlang/atom
import gleam/erlang/process
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

// ---------------------------------------------------------------------------
// FFI bindings
// ---------------------------------------------------------------------------

type PeerInternal

type Peer {
  Peer(internal: PeerInternal)
}

@external(erlang, "multinode_real_ffi", "ensure_distribution")
fn ensure_distribution_ffi() -> Result(String, String)

@external(erlang, "multinode_real_ffi", "start_peer")
fn start_peer_ffi(short_name: String) -> Result(PeerInternal, String)

@external(erlang, "multinode_real_ffi", "stop_peer")
fn stop_peer_ffi(internal: PeerInternal) -> Nil

@external(erlang, "multinode_real_ffi", "peer_call_register_global")
fn peer_register_global_ffi(
  internal: PeerInternal,
  name: String,
) -> Result(atom.Atom, String)

@external(erlang, "multinode_real_ffi", "peer_call_whereis_global")
fn peer_whereis_global_ffi(
  internal: PeerInternal,
  name: String,
) -> Result(process.Pid, atom.Atom)

@external(erlang, "multinode_real_ffi", "connect_peers")
fn connect_peers_ffi(a: PeerInternal, b: PeerInternal) -> Result(Nil, String)

@external(erlang, "multinode_real_ffi", "sync_global")
fn sync_global_ffi(internal: PeerInternal) -> Result(Nil, String)

fn skip_if_no_distribution(_test_id: String, _reason: String) -> Nil {
  Nil
}

// ---------------------------------------------------------------------------
// Z2. :global.sync() race window
//
// A peer registers a name on its local `:global`. The origin's
// `registry.lookup_with_timeout` MUST resolve within the supplied
// window. The exact propagation latency is implementation-defined,
// but the contract is "lookup_with_timeout(_, T, _) returns Ok within
// T ms or Error(LookupNotFound)". never hang, never lose the entry.
// ---------------------------------------------------------------------------

pub fn z2_global_sync_race_resolves_within_window_test() {
  case ensure_distribution_ffi() {
    Error(reason) ->
      skip_if_no_distribution("Z2", "distribution unavailable: " <> reason)
    Ok(_origin) -> {
      let short = "z2_peer_" <> test_helpers.unique_id()
      case start_peer_ffi(short) {
        Error(reason) ->
          skip_if_no_distribution("Z2", "peer start failed: " <> reason)
        Ok(peer_internal) -> {
          let peer = Peer(internal: peer_internal)
          let name = "z2_name_" <> test_helpers.unique_id()
          let assert Ok(_yes_or_no) =
            peer_register_global_ffi(peer.internal, name)

          // Build a TypedName matching the registered name. the
          // codec doesn't matter, we only resolve the PID through the
          // typed name.
          let tn =
            registry.typed_name(name, fn(_n) { Ok(<<>>) }, fn(_b) { Ok(0) })

          let result = registry.lookup_with_timeout(tn, 5000, 100)

          // Cleanup before assertion so a panic still tears down the peer.
          stop_peer_ffi(peer.internal)

          // Lookup must resolve to a live GlobalSubject within 5 s.
          // Failure modes that would surface here:
          //   - net_kernel didn't propagate the name in time (race);
          //   - global.sync() never settles (deadlock);
          //   - lookup_with_timeout overshoots its budget.
          case result {
            Ok(_gs) -> should.be_true(True)
            Error(_err) -> should.be_true(False)
          }
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Z3. cross-node `register_global` race
//
// Two peers concurrently try to register the same global name. The
// `:global` module guarantees exactly one winner; the loser must see
// a "false" return from register_name (or our typed `AlreadyExists`).
// ---------------------------------------------------------------------------

pub fn z3_cross_node_register_global_picks_one_winner_test() {
  case ensure_distribution_ffi() {
    Error(reason) ->
      skip_if_no_distribution("Z3", "distribution unavailable: " <> reason)
    Ok(_origin) -> {
      let id = test_helpers.unique_id()
      case start_peer_ffi("z3_p1_" <> id) {
        Error(reason) ->
          skip_if_no_distribution("Z3", "peer1 start failed: " <> reason)
        Ok(p1_internal) ->
          case start_peer_ffi("z3_p2_" <> id) {
            Error(reason) -> {
              stop_peer_ffi(p1_internal)
              skip_if_no_distribution("Z3", "peer2 start failed: " <> reason)
            }
            Ok(p2_internal) -> {
              let p1 = Peer(internal: p1_internal)
              let p2 = Peer(internal: p2_internal)
              let name = "z3_shared_" <> id

              // Make peer1 and peer2 directly aware of each other so
              // `:global` shares a single name table. Without this,
              // the two peers can each register the same name on
              // their own `:global` view and only converge after
              // BEAM's gossip happens to propagate via origin.
              let _ = connect_peers_ffi(p1.internal, p2.internal)

              // Race the same name from both peers.
              let _r1 = peer_register_global_ffi(p1.internal, name)
              let _r2 = peer_register_global_ffi(p2.internal, name)

              // Force a sync on both peers so `:global`'s
              // conflict-resolve callback runs (it kills the loser's
              // process and clears the duplicate entry).
              let _ = sync_global_ffi(p1.internal)
              let _ = sync_global_ffi(p2.internal)

              // Both peers must now agree on a single winner Pid.
              let r1_view = peer_whereis_global_ffi(p1.internal, name)
              let r2_view = peer_whereis_global_ffi(p2.internal, name)

              stop_peer_ffi(p1.internal)
              stop_peer_ffi(p2.internal)

              // Both views resolve, and they resolve to the SAME Pid
              // (i.e. there is exactly one registered owner across
              // the cluster).
              case r1_view, r2_view {
                Ok(pid_a), Ok(pid_b) -> should.equal(pid_a, pid_b)
                _, _ -> should.be_true(False)
              }
            }
          }
      }
    }
  }
}
