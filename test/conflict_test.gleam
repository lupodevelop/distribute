//// Tests for `distribute/conflict`: the built-in resolvers and the
//// FFI shim that protects `:global` from slow or panicking user
//// resolvers.
////
//// The resolvers themselves are pure 3-arity functions over Pids,
//// so the bulk of the suite runs single-node with synthetic Pids
//// spawned via `process.spawn_unlinked`. The FFI safe-invoke shim
//// is exercised through `register_global_with_resolver` plus a
//// helper that calls the wrapped resolver directly to verify the
//// timeout and crash branches end up in `telemetry.ConflictResolverFailed`
//// without locking up the test runner.

import distribute/codec
import distribute/conflict
import distribute/global
import distribute/registry
import distribute/telemetry
import gleam/erlang/process
import gleam/list
import gleam/option
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn long_lived_pid() -> process.Pid {
  process.spawn_unlinked(fn() { process.sleep(60_000) })
}

fn capture_into(events: process.Subject(telemetry.Event)) -> Nil {
  telemetry.install(fn(event) {
    process.send(events, event)
    Nil
  })
}

fn drain_events(
  events: process.Subject(telemetry.Event),
  acc: List(telemetry.Event),
) -> List(telemetry.Event) {
  case process.receive(events, 0) {
    Ok(e) -> drain_events(events, [e, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

@external(erlang, "conflict_ffi", "register_with_resolver")
fn register_with_resolver_ffi(
  name: String,
  pid: process.Pid,
  resolver: conflict.Resolver,
) -> Result(Nil, Nil)

@external(erlang, "conflict_test_ffi", "invoke_wrapped_resolver")
fn invoke_wrapped_resolver(
  name: String,
  pid1: process.Pid,
  pid2: process.Pid,
  resolver: conflict.Resolver,
) -> process.Pid

@external(erlang, "conflict_test_ffi", "invoke_wrapped_resolver_kill_both")
fn invoke_wrapped_resolver_kill_both(
  name: String,
  pid1: process.Pid,
  pid2: process.Pid,
  resolver: conflict.Resolver,
) -> Bool

@external(erlang, "conflict_test_ffi", "node_name_for_self")
fn node_name_for_self() -> String

// ---------------------------------------------------------------------------
// Built-in resolvers (pure)
// ---------------------------------------------------------------------------

pub fn lowest_pid_wins_picks_smaller_pid_test() {
  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = conflict.lowest_pid_wins()
  // Pids are spawned in order; the first allocated has the lower
  // term-order on the same VM. Verify against `<` directly so the
  // test cannot drift if the BEAM ever reverses the convention.
  case is_pid_lt(p1, p2) {
    True -> should.equal(resolver("name", p1, p2), conflict.Keep(p1))
    False -> should.equal(resolver("name", p1, p2), conflict.Keep(p2))
  }
  process.kill(p1)
  process.kill(p2)
}

pub fn highest_pid_wins_picks_larger_pid_test() {
  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = conflict.highest_pid_wins()
  case is_pid_lt(p1, p2) {
    True -> should.equal(resolver("name", p1, p2), conflict.Keep(p2))
    False -> should.equal(resolver("name", p1, p2), conflict.Keep(p1))
  }
  process.kill(p1)
  process.kill(p2)
}

pub fn kill_both_returns_kill_both_unconditionally_test() {
  // `kill_both` is a one-liner but the contract is load-bearing:
  // it is the resolver users pick when state divergence is worse
  // than no winner. Pin it.
  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = conflict.kill_both()
  should.equal(resolver("name_a", p1, p2), conflict.KillBoth)
  should.equal(resolver("name_b", p2, p1), conflict.KillBoth)
  process.kill(p1)
  process.kill(p2)
}

pub fn keep_local_falls_back_when_both_local_test() {
  // Single-node test: both PIDs are local. `keep_local` falls back
  // to lowest_pid_wins, which is what we assert.
  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let local = conflict.keep_local()
  let lowest = conflict.lowest_pid_wins()
  should.equal(local("name", p1, p2), lowest("name", p1, p2))
  process.kill(p1)
  process.kill(p2)
}

pub fn node_priority_falls_back_when_both_unlisted_test() {
  // Single-node tests cannot meaningfully exercise multi-node
  // priority. We verify the documented "tie -> lowest_pid_wins"
  // fallback with an empty / non-matching priority list.
  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = conflict.node_priority(["nonexistent@host"])
  let lowest = conflict.lowest_pid_wins()
  should.equal(resolver("name", p1, p2), lowest("name", p1, p2))
  process.kill(p1)
  process.kill(p2)
}

pub fn node_priority_local_node_listed_picks_listed_test() {
  // When the priority list includes the local node, both PIDs land
  // on it and tie-break via lowest_pid_wins. This is the reachable
  // path single-node; the multi-node ordering case is covered by
  // construction (deterministic int comparison on rank indices).
  let local = node_name_for_self()
  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = conflict.node_priority([local])
  let lowest = conflict.lowest_pid_wins()
  should.equal(resolver("name", p1, p2), lowest("name", p1, p2))
  process.kill(p1)
  process.kill(p2)
}

@external(erlang, "erlang", "<")
fn is_pid_lt(a: process.Pid, b: process.Pid) -> Bool

// ---------------------------------------------------------------------------
// FFI shim: timeout + crash + telemetry
// ---------------------------------------------------------------------------

pub fn ffi_shim_timeout_falls_back_and_emits_failed_test() {
  // A resolver that sleeps forever exceeds the configured timeout. The
  // shim must kill the worker, fall back to lowest_pid_wins, and
  // emit `ConflictResolverFailed`. We use a 50 ms sleep so the
  // test does not wait for the production timeout because the test
  // FFI helper passes a tightened timeout for this specific case.
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = fn(_name, _pid1, _pid2) {
    process.sleep(2000)
    conflict.Keep(p1)
  }

  // The helper invokes the wrapped resolver with a 50 ms internal
  // timeout instead of the production value. Either way the resolver
  // sleeps too long.
  let winner =
    invoke_wrapped_resolver("conflict_timeout_test", p1, p2, resolver)

  // Winner must be the lowest-pid fallback.
  let expected_winner = case is_pid_lt(p1, p2) {
    True -> p1
    False -> p2
  }
  should.equal(winner, expected_winner)

  let drained = drain_events(events, [])
  let saw_failed =
    list.any(drained, fn(e) {
      case e {
        telemetry.ConflictResolverFailed(_, _) -> True
        _ -> False
      }
    })
  let saw_resolved =
    list.any(drained, fn(e) {
      case e {
        telemetry.ConflictResolved(_, option.Some(_)) -> True
        _ -> False
      }
    })
  should.be_true(saw_failed)
  should.be_true(saw_resolved)

  process.kill(p1)
  process.kill(p2)
  telemetry.reset()
}

pub fn ffi_shim_panic_falls_back_and_emits_failed_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = fn(_name, _pid1, _pid2) { panic as "buggy resolver" }

  let winner = invoke_wrapped_resolver("conflict_panic_test", p1, p2, resolver)
  let expected_winner = case is_pid_lt(p1, p2) {
    True -> p1
    False -> p2
  }
  should.equal(winner, expected_winner)

  let drained = drain_events(events, [])
  let saw_failed =
    list.any(drained, fn(e) {
      case e {
        telemetry.ConflictResolverFailed(_, _) -> True
        _ -> False
      }
    })
  should.be_true(saw_failed)

  process.kill(p1)
  process.kill(p2)
  telemetry.reset()
}

pub fn ffi_shim_worker_down_emits_non_timeout_failure_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = fn(_name, _pid1, _pid2) {
    process.kill(process.self())
    conflict.Keep(p1)
  }

  let winner =
    invoke_wrapped_resolver("conflict_worker_down_test", p1, p2, resolver)
  let expected_winner = case is_pid_lt(p1, p2) {
    True -> p1
    False -> p2
  }
  should.equal(winner, expected_winner)

  let drained = drain_events(events, [])
  let saw_non_timeout_failure =
    list.any(drained, fn(e) {
      case e {
        telemetry.ConflictResolverFailed(_, reason)
          if reason != "resolver timed out"
        -> True
        _ -> False
      }
    })
  should.be_true(saw_non_timeout_failure)

  process.kill(p1)
  process.kill(p2)
  telemetry.reset()
}

pub fn ffi_shim_keep_emits_resolved_with_winner_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = fn(_name, _pid1, _pid2) { conflict.Keep(p1) }

  let winner = invoke_wrapped_resolver("conflict_keep_test", p1, p2, resolver)
  should.equal(winner, p1)

  let drained = drain_events(events, [])
  let saw_resolved =
    list.any(drained, fn(e) {
      case e {
        telemetry.ConflictResolved(_, option.Some(w)) if w == p1 -> True
        _ -> False
      }
    })
  should.be_true(saw_resolved)

  process.kill(p1)
  process.kill(p2)
  telemetry.reset()
}

pub fn ffi_shim_kill_both_emits_resolved_with_none_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let p1 = long_lived_pid()
  let p2 = long_lived_pid()
  let resolver = fn(_name, _pid1, _pid2) { conflict.KillBoth }

  let none_returned =
    invoke_wrapped_resolver_kill_both(
      "conflict_kill_both_test",
      p1,
      p2,
      resolver,
    )
  should.be_true(none_returned)

  let drained = drain_events(events, [])
  let saw_resolved_none =
    list.any(drained, fn(e) {
      case e {
        telemetry.ConflictResolved(_, option.None) -> True
        _ -> False
      }
    })
  should.be_true(saw_resolved_none)

  process.kill(p1)
  process.kill(p2)
  telemetry.reset()
}

// ---------------------------------------------------------------------------
// Public API plumbing
// ---------------------------------------------------------------------------

pub fn register_global_with_resolver_happy_path_test() {
  // Registering with a custom resolver succeeds locally just like
  // the regular `register_global`. The resolver itself is not
  // invoked single-node (no conflict to resolve), but the call
  // path verifies the FFI plumbing compiles and the FFI shim
  // accepts the function value.
  let name = "conflict_reg_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )

  let assert Ok(Nil) =
    registry.register_global_with_resolver(tn, gs, conflict.lowest_pid_wins())

  let _ = registry.unregister(name)
}

pub fn register_with_resolver_ffi_invalid_pid_returns_error_test() {
  // Direct FFI call with a non-Pid term: must surface as
  // `invalid_process` rather than panic.
  let name = "conflict_invalid_" <> test_helpers.unique_id()
  let bogus_pid = bogus_term_as_pid()
  let resolver = conflict.lowest_pid_wins()
  should.be_error(register_with_resolver_ffi(name, bogus_pid, resolver))
}

@external(erlang, "conflict_test_ffi", "bogus_term_as_pid")
fn bogus_term_as_pid() -> process.Pid
