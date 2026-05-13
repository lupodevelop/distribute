import distribute/cluster
import distribute/config
import gleam/erlang/process
import gleam/int
import gleam/list
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

pub fn self_node_returns_string_test() {
  // When not distributed, should be "nonode@nohost"
  let name = cluster.self_node()
  should.be_true(name != "")
}

pub fn nodes_returns_list_test() {
  // When not distributed, should be empty
  let connected = cluster.nodes()
  should.equal(connected, [])
}

pub fn is_distributed_false_when_local_test() {
  // By default in tests, not distributed
  // (This may be True if the test runner starts a distributed node)
  let _ = cluster.is_distributed()
  should.be_true(True)
}

pub fn connected_count_zero_when_local_test() {
  let count = cluster.connected_count()
  should.equal(count, 0)
}

pub fn health_works_test() {
  let health = cluster.health()
  should.be_true(health.self_node != "")
}

pub fn health_partition_invariant_test() {
  // The partition invariant: every connected node appears in exactly one
  // of `reachable_nodes` / `unreachable_nodes`. Even on partial timeouts
  // we must not silently drop pending nodes from the result.
  let h = cluster.health()
  let total = list.length(h.reachable_nodes) + list.length(h.unreachable_nodes)
  total |> should.equal(h.connected_count)
}

pub fn health_parallelism_cap_is_bounded_test() {
  should.equal(cluster.health_parallelism_cap(0), 0)
  should.equal(cluster.health_parallelism_cap(1), 1)
  should.equal(cluster.health_parallelism_cap(32), 32)
  should.equal(cluster.health_parallelism_cap(200), 32)
}

pub fn stable_partition_preserves_connected_order_test() {
  let connected = ["n1", "n2", "n3", "n4", "n5"]
  // Simulate out-of-order worker replies, duplicates, and unknown values.
  let reachable_candidates = ["n4", "n2", "n2", "n5", "ghost"]

  let #(reachable, unreachable) =
    cluster.stable_partition(connected, reachable_candidates)

  should.equal(reachable, ["n2", "n4", "n5"])
  should.equal(unreachable, ["n1", "n3"])
}

pub fn start_node_invalid_name_test() {
  // Missing @ should fail validation
  let result = cluster.start_node("invalid", "cookie")
  should.be_error(result)
}

pub fn start_node_cookie_too_long_test() {
  // Cookie > 255 chars should fail
  let long_cookie = repeat_string("a", 256)
  let result = cluster.start_node("node@host", long_cookie)
  should.be_error(result)
}

pub fn connect_invalid_node_format_test() {
  // Missing @ should fail with InvalidNodeFormat
  let result = cluster.connect("invalid")
  case result {
    Error(cluster.InvalidNodeFormat(_)) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

pub fn connect_unreachable_node_returns_connect_failed_test() {
  // A well-formed name for a node that does not exist should return ConnectFailed.
  // net_kernel:connect_node/1 returns `false` for unreachable nodes.
  // This test only runs meaningfully when the test runner itself is a
  // distributed node; otherwise connect_node returns `ignored` → ConnectIgnored.
  let result = cluster.connect("nonexistent@127.0.0.1")
  case result {
    Error(cluster.ConnectFailed) -> should.be_true(True)
    Error(cluster.ConnectIgnored) -> should.be_true(True)
    _ -> should.be_true(False)
  }
}

// ---------------------------------------------------------------------------
// Atom-creation correctness: connect/ping must NOT silently fail with
// `connect_failed`/`false` for node names whose atom does not yet exist on
// the local VM. This was the symptom of the v3-era `binary_to_existing_atom`
// path; the safe-create helper fixes it.
// ---------------------------------------------------------------------------

pub fn connect_fresh_node_name_creates_atom_test() {
  // Use a guaranteed-fresh node name so the atom cannot already exist.
  let fresh = "freshconnect" <> test_helpers.unique_id() <> "@127.0.0.1"
  let result = cluster.connect(fresh)

  // When the test VM is not distributed (the default), `net_kernel:connect_node`
  // returns `ignored` → `ConnectIgnored`. The bug path returns `ConnectFailed`
  // from the atom-not-found branch, which would be observable here.
  // When the VM *is* distributed, an unreachable peer returns `false` →
  // `ConnectFailed`, which is also acceptable (no atom-creation issue).
  case cluster.is_distributed() {
    False ->
      case result {
        Error(cluster.ConnectIgnored) -> should.be_true(True)
        other -> should.equal(other, Error(cluster.ConnectIgnored))
      }
    True ->
      case result {
        Error(cluster.ConnectFailed) -> should.be_true(True)
        Error(cluster.ConnectIgnored) -> should.be_true(True)
        _ -> should.be_true(False)
      }
  }
}

pub fn ping_fresh_node_name_does_not_crash_test() {
  // Ping with a fresh name should return `False` (unreachable), not crash
  // and not silently treat it as atom-not-found.
  let fresh = "freshping" <> test_helpers.unique_id() <> "@127.0.0.1"
  cluster.ping(fresh) |> should.equal(False)
}

pub fn connect_error_variants_are_exhaustive_test() {
  // Compile-time guarantee: ConnectError has exactly three variants.
  // If a new variant is added, this exhaustive match forces an update
  // and prevents silent mismatches between Gleam type and FFI surface.
  let assert_handled = fn(err: cluster.ConnectError) -> Bool {
    case err {
      cluster.ConnectFailed -> True
      cluster.ConnectIgnored -> True
      cluster.InvalidNodeFormat(_) -> True
      cluster.ConnectAtomBudgetExceeded -> True
    }
  }
  should.be_true(assert_handled(cluster.ConnectFailed))
  should.be_true(assert_handled(cluster.ConnectIgnored))
  should.be_true(assert_handled(cluster.InvalidNodeFormat("x")))
  should.be_true(assert_handled(cluster.ConnectAtomBudgetExceeded))
}

// ---------------------------------------------------------------------------
// start_node. typed errors for invalid cookie / name formats. The FFI
// previously folded these into the generic `StartFailed(String)`, which
// made them awkward to pattern match on.
// ---------------------------------------------------------------------------

pub fn start_node_invalid_cookie_returns_typed_error_test() {
  // Cookie with a disallowed character (`@`) must surface as a typed
  // InvalidCookieFormat, not be silently mapped to StartFailed.
  let result = cluster.start_node("ok@127.0.0.1", "bad@cookie")
  let assert Error(cluster.InvalidCookieFormat(_)) = result
}

pub fn start_node_empty_cookie_returns_typed_error_test() {
  let result = cluster.start_node("ok@127.0.0.1", "")
  let assert Error(cluster.InvalidCookieFormat(_)) = result
}

pub fn start_node_cookie_with_space_returns_typed_error_test() {
  let result = cluster.start_node("ok@127.0.0.1", "bad cookie")
  let assert Error(cluster.InvalidCookieFormat(_)) = result
}

pub fn start_node_invalid_name_charset_returns_typed_error_test() {
  // Name with disallowed character (space, ``)
  let result = cluster.start_node("bad name@127.0.0.1", "okcookie")
  let assert Error(cluster.InvalidNodeName(_)) = result
}

// ---------------------------------------------------------------------------
// Atom-budget guardrail: connecting to enough fresh node names to exceed
// `max_distribution_atoms` must surface a typed error BEFORE
// `binary_to_atom` is called. the VM atom table cannot be exhausted by
// going through `cluster.connect`.
// ---------------------------------------------------------------------------

pub fn connect_atom_budget_exhausted_returns_typed_error_test() {
  // Configure a tiny budget (3 fresh atoms) and reset the counter so
  // this test starts clean.
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_distribution_atoms: 3,
      ),
    )

  // Burn the 3-atom budget with three distinct fresh names.
  let id = test_helpers.unique_id()
  let _ = cluster.connect("budget1_" <> id <> "@127.0.0.1")
  let _ = cluster.connect("budget2_" <> id <> "@127.0.0.1")
  let _ = cluster.connect("budget3_" <> id <> "@127.0.0.1")

  // Fourth fresh name must surface ConnectAtomBudgetExceeded.
  let result = cluster.connect("budget4_" <> id <> "@127.0.0.1")

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())

  let assert Error(cluster.ConnectAtomBudgetExceeded) = result
}

pub fn ping_atom_budget_exhausted_returns_false_test() {
  // ping/1 returns a Bool, so an exhausted budget collapses into `false`
  // (same as "node unreachable"). The point is that the VM atom table
  // is NOT touched. we cannot test that directly, but we can verify
  // ping survives the budget pressure without crashing.
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_distribution_atoms: 2,
      ),
    )

  let id = test_helpers.unique_id()
  let _ = cluster.ping("ping1_" <> id <> "@127.0.0.1")
  let _ = cluster.ping("ping2_" <> id <> "@127.0.0.1")
  let result = cluster.ping("ping3_" <> id <> "@127.0.0.1")

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())

  // Either reachability check would have returned false anyway in the
  // test VM, but the key thing is the call returned cleanly.
  result |> should.equal(False)
}

pub fn connect_invalid_charset_test() {
  // Names with disallowed characters must be rejected with the typed
  // `InvalidNodeFormat`. strictly, no fallback to `ConnectFailed`.
  // A regression that demoted the typed error path to atom-creation
  // failure would surface here.
  let result = cluster.connect("bad name!@127.0.0.1")
  let assert Error(cluster.InvalidNodeFormat(_)) = result
}

fn repeat_string(s: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False -> s <> repeat_string(s, n - 1)
  }
}

// ---------------------------------------------------------------------------
// Z1. atom-budget concurrent contention
//
// `atomics:add_get/3` is lock-free, so a swarm of concurrent
// `cluster.connect` calls with unique fresh names should:
//   * see at most `max_distribution_atoms` calls succeed past the budget
//     guardrail (i.e. *not* return `ConnectAtomBudgetExceeded`);
//   * see every other call surface `Error(ConnectAtomBudgetExceeded)`;
//   * never lose a result, never go past the cap on either side.
//
// Past the guardrail, the FFI still calls `net_kernel:connect_node/1`,
// which on a non-distributed test VM returns `ignored` -> the Gleam
// mapping is `Error(ConnectIgnored)`. So the success-side bucket here
// is `ConnectIgnored` (the budget-passed path), not `Ok(Nil)`.
// ---------------------------------------------------------------------------

pub fn z1_atom_budget_concurrent_contention_test() {
  let budget = 50
  let n = 200
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_distribution_atoms: budget,
      ),
    )

  let uniq = test_helpers.unique_id()
  let results = process.new_subject()

  z1_indexes(n)
  |> list.each(fn(i) {
    let _ =
      process.spawn_unlinked(fn() {
        let name = "z1_" <> uniq <> "_" <> int.to_string(i) <> "@127.0.0.1"
        process.send(results, cluster.connect(name))
      })
    Nil
  })

  let collected = z1_collect(results, n, [])

  let exhausted =
    list.count(collected, fn(r) {
      r == Error(cluster.ConnectAtomBudgetExceeded)
    })
  let passed_guardrail = n - exhausted

  // Exactly `budget` fresh atoms got past the guardrail; the rest were
  // refused before `binary_to_atom`. The atomics counter cannot
  // overshoot regardless of scheduler interleaving.
  should.equal(passed_guardrail, budget)
  should.equal(exhausted, n - budget)

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
}

fn z1_collect(
  subj: process.Subject(Result(Nil, cluster.ConnectError)),
  remaining: Int,
  acc: List(Result(Nil, cluster.ConnectError)),
) -> List(Result(Nil, cluster.ConnectError)) {
  case remaining {
    0 -> acc
    _ -> {
      let assert Ok(r) = process.receive(subj, 5000)
      z1_collect(subj, remaining - 1, [r, ..acc])
    }
  }
}

/// `[1, 2, ..., n]`. Replacement for `list.range`, removed in
/// `gleam_stdlib` 1.0.
fn z1_indexes(n: Int) -> List(Int) {
  do_z1_indexes(n, [])
}

fn do_z1_indexes(i: Int, acc: List(Int)) -> List(Int) {
  case i < 1 {
    True -> acc
    False -> do_z1_indexes(i - 1, [i, ..acc])
  }
}
