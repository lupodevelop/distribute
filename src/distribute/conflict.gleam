//// Conflict resolvers for `:global` registrations across split-brain
//// healing.
////
//// ## Why this module exists
////
//// Erlang's `:global` registry is the backbone of `distribute`'s
//// `register_global` path. It is also the part of the BEAM with the
//// most teeth on a partitioned cluster: when a network split heals,
//// `:global` discovers any name that was claimed on both halves and
//// resolves the conflict by sending an **uncatchable**
//// `exit(loser, kill)` to one of the two PIDs, picked by a
//// resolver function.
////
//// The default resolver (`:global.random_notify_name/3`) is a coin
//// flip. For workloads where the choice matters. A leader that
//// must always sit on a specific node, a router whose state lives
//// on a particular shard, a singleton whose oldest instance is
//// Authoritative. Coin-flip resolution turns split-brain healing
//// into a non-deterministic outage.
////
//// `register_global_with_resolver` (in `distribute/registry`) lets
//// you swap that default for a function you control. This module
//// supplies the `Resolver` type and a small set of pure built-in
//// resolvers covering the most common policies. Users compose or
//// write their own.
////
//// ## Operational guard rails
////
//// The resolver is invoked by `:global` *inside the global_name_server
//// process*. A slow or panicking resolver stalls every other
//// `:global` operation cluster-wide for the duration. The library
//// wraps every user resolver in an FFI shim that:
////
//// - Spawns a short-lived worker to run the user fn (so a panic
////   does not propagate into global_name_server).
//// - Imposes a hard deadline from config (default **1 000 ms**, see
////   `conflict_ffi:default_resolver_timeout_ms/0`).
//// - On timeout or crash, applies a deterministic fallback (lowest
////   term-ordered Pid wins) so the cluster never wedges.
//// - Emits `telemetry.ConflictResolved` on every resolution and
////   `telemetry.ConflictResolverFailed` whenever the user fn
////   misbehaved and the fallback fired.
////
//// Watch the latter event in production: a steady stream of
//// `ConflictResolverFailed` means your resolver itself is broken.
//// A steady stream of `ConflictResolved` (without failures) means
//// the cluster is flapping. Partitions are healing repeatedly
//// And the application can't fix it on its own.
////
//// ## Data-loss honesty box
////
//// The "lowest term-ordered Pid wins" fallback that fires after a
//// resolver crash or timeout is **deterministic**, not **state-
//// aware**. If your resolver was supposed to keep the side with
//// the most recent state (an oldest-wins, an authoritative-leader,
//// a longest-lived counter) and it fails for any reason, the
//// fallback can pick the wrong PID and the cluster loses whatever
//// state the loser was holding.
////
//// Two pragmatic choices:
////
//// - **Stateless / idempotent actors** (routers, dispatchers,
////   stateless coordinators): the fallback is fine, the cluster
////   converges to *some* PID, the loss is bounded.
//// - **Stateful actors** (counters, caches, leaders with quorum
////   state): use `kill_both()` as the *primary* resolver, watch
////   `ConflictResolved(_, None)` in telemetry, and trigger an
////   application-level recovery path (re-elect, re-bootstrap from
////   durable storage). Better to admit "no winner" than silently
////   pick the side without the state.
////
//// The library cannot know which category a given actor falls
//// into, so the default fallback prioritises convergence over
//// state preservation. This is the right call for the BEAM's
//// "let it crash" baseline; it is the wrong call for systems
//// where state divergence costs money. Pick consciously.
////
//// ## What this does NOT do
////
//// Conflict resolution at the registry level cannot reconcile the
//// **state** of two diverged actors. The loser dies, the winner
//// keeps its state, anything pending on the loser is gone.
//// Reconciling state is the next layer up (snapshot the loser
//// before kill, merge into the winner) and intentionally not in
//// scope here. The plan to ship that as a higher-level
//// "merge_resolver" composes on top of this module without changing
//// the `Resolver` type.

import gleam/erlang/process

/// Decision returned by a conflict resolver.
///
/// - `Keep(pid)`: keep this PID alive, kill the other one. Standard
///   `:global` semantics: the surviving Pid stays registered, the
///   loser receives an uncatchable `exit(_, kill)`.
/// - `KillBoth`: refuse to pick. Both Pids die. `:global` removes
///   the name; downstream `lookup` returns `Error(Nil)` until one
///   side re-registers. Useful when neither candidate is
///   trustworthy (e.g. both have lost their backing storage).
pub type ConflictOutcome {
  Keep(process.Pid)
  KillBoth
}

/// Pure function the conflict path calls with the contended name
/// and the two competing PIDs. Must return inside
/// `conflict_ffi:default_resolver_timeout_ms/0` (1 000 ms by
/// default) or the FFI shim falls back to a deterministic
/// "lowest-Pid-wins" pick and emits
/// `telemetry.ConflictResolverFailed`.
///
/// The function runs in a dedicated worker process spawned by the
/// FFI; do not assume it runs on either of the two contending
/// nodes. RPCs to the contending PIDs are allowed but each must
/// finish well inside the timeout budget. It is safer to make the resolver
/// pure (e.g. pick by node name, by Pid order) and leave any
/// snapshot-and-merge logic to a higher layer.
pub type Resolver =
  fn(String, process.Pid, process.Pid) -> ConflictOutcome

// ---------------------------------------------------------------------------
// Built-in resolvers
//
// All built-ins are pure, total, and run in microseconds. They are
// the safe default starting point for any production deployment.
// ---------------------------------------------------------------------------

/// Pick the PID with the lowest Erlang term order. Deterministic,
/// no I/O, no node-locality bias. Usable as a tiebreaker even
/// when no semantic policy fits the workload.
///
/// Term ordering on Pids is stable across the cluster but not
/// human-meaningful: it is essentially "which Pid was created
/// first on the lower-numbered node". Use this when you need
/// reproducibility, not when you need a specific side to win.
pub fn lowest_pid_wins() -> Resolver {
  fn(_name, pid1, pid2) {
    case pid_lt(pid1, pid2) {
      True -> Keep(pid1)
      False -> Keep(pid2)
    }
  }
}

/// Pick the PID with the highest Erlang term order. Mirror image
/// of `lowest_pid_wins`. Mostly useful when paired with another
/// strategy in a fallback chain.
pub fn highest_pid_wins() -> Resolver {
  fn(_name, pid1, pid2) {
    case pid_lt(pid1, pid2) {
      True -> Keep(pid2)
      False -> Keep(pid1)
    }
  }
}

/// Always keep the PID running on the local node, if either does.
/// Falls back to `lowest_pid_wins` when neither (or both) PID is
/// Local. The resolver runs on the global_name_server worker,
/// which is co-located with one of the two PIDs only if that
/// side is the lock-holder.
///
/// Useful in deployments where one node is the canonical "primary"
/// and remote duplicates from a partition heal should always
/// defer.
pub fn keep_local() -> Resolver {
  fn(name, pid1, pid2) {
    let p1_local = pid_node_is_local(pid1)
    let p2_local = pid_node_is_local(pid2)
    case p1_local, p2_local {
      True, False -> Keep(pid1)
      False, True -> Keep(pid2)
      True, True -> lowest_pid_wins()(name, pid1, pid2)
      False, False -> lowest_pid_wins()(name, pid1, pid2)
    }
  }
}

/// Refuse to pick. Both PIDs die, the name is removed from
/// `:global`, and the next `lookup` returns `Error(Nil)` until
/// one side re-registers.
///
/// Use this for **stateful** actors where killing the wrong PID
/// would lose authoritative state and the application has its
/// own reconciliation path (re-elect a leader, re-bootstrap from
/// durable storage). The other built-in resolvers
/// (`lowest_pid_wins`, `keep_local`, `node_priority`) all preserve
/// *one* PID at the cost of potentially picking the wrong one;
/// `kill_both` is the safer choice when "wrong choice" is worse
/// than "no choice".
///
/// Operational pairing: subscribe to `cluster_monitor` and watch
/// `telemetry.ConflictResolved(_, None)` events; on each, trigger
/// the application-specific recovery (e.g. re-register from a
/// snapshot, kick off a leader-election round).
pub fn kill_both() -> Resolver {
  fn(_name, _pid1, _pid2) { KillBoth }
}

/// Pick by static node priority. The resolver checks each PID's
/// node against the supplied list (highest priority first) and
/// keeps the one whose node appears earlier. PIDs on nodes not in
/// the list lose to PIDs on nodes that are; ties are broken by
/// `lowest_pid_wins`.
///
/// Use this in deployments with a clear hierarchy. e.g.
/// `["primary@host", "secondary@host"]`, where you can name
/// the authoritative side ahead of time.
pub fn node_priority(order: List(String)) -> Resolver {
  fn(name, pid1, pid2) {
    let p1_rank = node_rank(order, pid_node_name(pid1))
    let p2_rank = node_rank(order, pid_node_name(pid2))
    case p1_rank == p2_rank {
      True -> lowest_pid_wins()(name, pid1, pid2)
      False ->
        case p1_rank < p2_rank {
          True -> Keep(pid1)
          False -> Keep(pid2)
        }
    }
  }
}

// ---------------------------------------------------------------------------
// Internal helpers (Erlang-side primitives)
// ---------------------------------------------------------------------------

@external(erlang, "erlang", "<")
fn pid_lt(a: process.Pid, b: process.Pid) -> Bool

@external(erlang, "conflict_ffi_helpers", "pid_node_is_local")
fn pid_node_is_local(pid: process.Pid) -> Bool

@external(erlang, "conflict_ffi_helpers", "pid_node_name")
fn pid_node_name(pid: process.Pid) -> String

/// Rank of a node name in the priority list. Index 0 = highest
/// priority. Names not in the list rank as `length + 1` so any
/// listed node beats any unlisted node.
fn node_rank(order: List(String), node: String) -> Int {
  case order {
    [] -> 0
    _ -> rank_loop(order, node, 0, 0)
  }
}

fn rank_loop(order: List(String), node: String, idx: Int, len: Int) -> Int {
  case order {
    [] -> len + 1
    [head, ..rest] ->
      case head == node {
        True -> idx
        False -> rank_loop(rest, node, idx + 1, len + 1)
      }
  }
}
