//// Tests for the `distribute/telemetry` event sink.
////
//// Each test installs a fresh sink that captures events into a
//// process Subject, triggers a code path, and asserts on the
//// captured stream. `telemetry.reset()` runs in setup to keep each
//// case isolated from previous installs.

import distribute/actor as dist_actor
import distribute/cluster
import distribute/codec
import distribute/config
import distribute/global
import distribute/receiver
import distribute/registry
import distribute/telemetry
import gleam/erlang/process
import gleam/list
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

fn no_op_handler(_msg: msg, state: state) -> receiver.HandlerStep(state) {
  receiver.Continue(state)
}

fn repeat_string(s: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False -> s <> repeat_string(s, n - 1)
  }
}

@external(erlang, "telemetry_test_ffi", "spawn_short_lived")
fn spawn_short_lived() -> process.Pid

/// Synchronously exit the calling process with reason `shutdown`.
/// Used by the proxy-crash test to land in the `IsolatedProxyDown`
/// branch without raising a panic (which would print a SASL CRASH
/// REPORT to stderr and pollute the test log). The branch is
/// reason-agnostic: any DOWN signal triggers the same code path.
/// Return type declared as `Int` so this can stand in for a
/// `make_request` body; at runtime the call never returns.
@external(erlang, "telemetry_test_ffi", "exit_self_shutdown")
fn exit_self_shutdown() -> Int

/// Disable the primary logger so deliberate panic exercises do not
/// pollute the test stdout. Returns the previous level for restore.
@external(erlang, "telemetry_test_ffi", "silence_logger")
fn silence_logger() -> Atom

/// Restore the primary logger to its prior level.
@external(erlang, "telemetry_test_ffi", "restore_logger")
fn restore_logger(prev: Atom) -> Nil

type Atom

// ---------------------------------------------------------------------------
// Infrastructure
// ---------------------------------------------------------------------------

pub fn install_then_emit_invokes_sink_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  telemetry.emit(telemetry.ActorRegistered("synthetic"))

  let assert Ok(telemetry.ActorRegistered("synthetic")) =
    process.receive(events, 100)
  telemetry.reset()
}

pub fn no_sink_installed_emit_is_noop_test() {
  telemetry.reset()

  // Must not panic, must not block. We only assert the call returns.
  telemetry.emit(telemetry.CallTargetDown)
  telemetry.emit(telemetry.ActorRegistered("nobody_listens"))

  let canary = process.new_subject()
  let result = process.receive(canary, 10)
  should.be_error(result)
}

pub fn sink_panic_does_not_propagate_to_caller_test() {
  // A sink that raises must not take down the calling process.
  // Library-internal processes (actor handlers, call selectors, the
  // call_isolated proxy) emit telemetry from hot paths; a buggy
  // user sink would otherwise kill them. The FFI catches and logs
  // via `logger:warning/2`, leaving the caller alive. The logger
  // is silenced for the duration of this test so the deliberate
  // warnings do not pollute stdout.
  let prev_level = silence_logger()
  telemetry.reset()

  telemetry.install(fn(_event) { panic as "buggy sink" })

  // The emit must return without propagating the panic.
  telemetry.emit(telemetry.CallTargetDown)

  // Subsequent emits must keep working: the catch is per-call, not
  // sticky.
  telemetry.emit(telemetry.ActorRegistered("after_panic"))

  // And we can install a healthy sink afterwards and observe events.
  let events = process.new_subject()
  capture_into(events)
  telemetry.emit(telemetry.CallTargetDown)
  let assert Ok(telemetry.CallTargetDown) = process.receive(events, 100)

  telemetry.reset()
  restore_logger(prev_level)
}

pub fn install_replaces_previous_sink_last_wins_test() {
  telemetry.reset()
  let first = process.new_subject()
  let second = process.new_subject()

  telemetry.install(fn(e) {
    process.send(first, e)
    Nil
  })
  telemetry.install(fn(e) {
    process.send(second, e)
    Nil
  })

  telemetry.emit(telemetry.CallTargetDown)

  let first_drained = drain_events(first, [])
  let second_drained = drain_events(second, [])

  should.equal(first_drained, [])
  should.equal(second_drained, [telemetry.CallTargetDown])
  telemetry.reset()
}

// ---------------------------------------------------------------------------
// Registry events
// ---------------------------------------------------------------------------

pub fn actor_registered_event_fires_on_register_global_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let name = "tel_reg_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let assert Ok(_gs) = dist_actor.start_registered(tn, 0, no_op_handler, 1000)

  let drained = drain_events(events, [])
  let saw_registered =
    list.any(drained, fn(e) {
      case e {
        telemetry.ActorRegistered(n) if n == name -> True
        _ -> False
      }
    })
  should.be_true(saw_registered)

  let _ = registry.unregister(name)
  telemetry.reset()
}

pub fn actor_registration_failed_event_fires_on_duplicate_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let name = "tel_dup_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let assert Ok(_gs) = dist_actor.start_registered(tn, 0, no_op_handler, 1000)

  let _ = dist_actor.start_registered(tn, 0, no_op_handler, 1000)

  let drained = drain_events(events, [])
  let saw_failed =
    list.any(drained, fn(e) {
      case e {
        telemetry.ActorRegistrationFailed(n, _) if n == name -> True
        _ -> False
      }
    })
  should.be_true(saw_failed)

  let _ = registry.unregister(name)
  telemetry.reset()
}

pub fn actor_unregistered_event_fires_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let name = "tel_unreg_" <> test_helpers.unique_id()
  let _ = registry.unregister(name)

  let drained = drain_events(events, [])
  let saw_unreg =
    list.any(drained, fn(e) {
      case e {
        telemetry.ActorUnregistered(n, _) if n == name -> True
        _ -> False
      }
    })
  should.be_true(saw_unreg)
  telemetry.reset()
}

pub fn register_global_tag_mismatch_emits_failed_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let name = "tel_tagmis_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  // global.new uses a random Ref tag, mismatched against `name`.
  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let _ = registry.register_global(tn, gs)

  let drained = drain_events(events, [])
  let saw_failed =
    list.any(drained, fn(e) {
      case e {
        telemetry.ActorRegistrationFailed(n, _) if n == name -> True
        _ -> False
      }
    })
  should.be_true(saw_failed)
  telemetry.reset()
}

pub fn register_typed_tag_mismatch_emits_failed_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let name = "tel_typed_tagmis_" <> test_helpers.unique_id()
  let raw_subj: process.Subject(BitArray) = process.new_subject()
  let _ = registry.register_typed(name, raw_subj)

  let drained = drain_events(events, [])
  let saw_failed =
    list.any(drained, fn(e) {
      case e {
        telemetry.ActorRegistrationFailed(n, _) if n == name -> True
        _ -> False
      }
    })
  should.be_true(saw_failed)
  telemetry.reset()
}

pub fn start_node_cookie_budget_emits_with_cookie_as_input_test() {
  // Burn the budget down to exactly 1 with a connect, so the node
  // name becomes an existing atom (free on lookup). Then call
  // start_node with the SAME name plus a fresh cookie: name lookup
  // succeeds via `binary_to_existing_atom`, cookie creation tries to
  // reserve a fresh slot, and the budget refuses. The FFI must emit
  // `AtomBudgetExhausted` with the *cookie* as `attempted_input`,
  // not the name. otherwise observability cannot distinguish
  // which input ran the budget out.
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(..config.default(), max_distribution_atoms: 1),
    )
  let id = test_helpers.unique_id()
  let node_name = "z_cookie_node_" <> id <> "@127.0.0.1"
  let cookie = "z_cookie_" <> id
  let _ = cluster.connect(node_name)

  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let _ = cluster.start_node(node_name, cookie)

  let drained = drain_events(events, [])
  let saw_cookie =
    list.any(drained, fn(e) {
      case e {
        telemetry.AtomBudgetExhausted(input, telemetry.AtomBudgetOnStartNode)
          if input == cookie
        -> True
        _ -> False
      }
    })
  should.be_true(saw_cookie)

  // And the (incorrect) attribution to the node name must NOT be present.
  let saw_name =
    list.any(drained, fn(e) {
      case e {
        telemetry.AtomBudgetExhausted(input, telemetry.AtomBudgetOnStartNode)
          if input == node_name
        -> True
        _ -> False
      }
    })
  should.be_false(saw_name)

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  telemetry.reset()
}

pub fn ping_atom_budget_exhausted_emits_test() {
  // Burn the budget down to 1 and use ping (Bool return) to trigger
  // a budget refusal. The FFI emits `AtomBudgetExhausted` with
  // origin `AtomBudgetOnPing` even though the public return is `Bool`.
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(..config.default(), max_distribution_atoms: 1),
    )
  // Burn the budget with a connect (creates the first fresh atom).
  let id = test_helpers.unique_id()
  let _ = cluster.connect("z_burn_" <> id <> "@127.0.0.1")

  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  // Ping a never-seen-before node -> budget refused at FFI -> emit.
  let _ = cluster.ping("z_ping_" <> id <> "@127.0.0.1")

  let drained = drain_events(events, [])
  let saw_emit =
    list.any(drained, fn(e) {
      case e {
        telemetry.AtomBudgetExhausted(_, telemetry.AtomBudgetOnPing) -> True
        _ -> False
      }
    })
  should.be_true(saw_emit)

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  telemetry.reset()
}

// ---------------------------------------------------------------------------
// Boundary events
// ---------------------------------------------------------------------------

pub fn payload_rejected_event_fires_on_oversize_send_test() {
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(..config.default(), max_payload_size_bytes: 64),
    )
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let subj = global.new(codec.string_encoder(), codec.string_decoder())
  let big = repeat_string("x", 200)
  let result = global.send(subj, big)
  should.be_error(result)

  let drained = drain_events(events, [])
  let saw_rejected =
    list.any(drained, fn(e) {
      case e {
        telemetry.PayloadRejected(_, _, telemetry.PayloadOnSend) -> True
        _ -> False
      }
    })
  should.be_true(saw_rejected)

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  telemetry.reset()
}

// ---------------------------------------------------------------------------
// Call lifecycle events
// ---------------------------------------------------------------------------

pub fn call_timed_out_event_fires_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let _ = global.call(gs, fn(_reply) { 1 }, codec.int_decoder(), 30)

  let drained = drain_events(events, [])
  let saw_timeout =
    list.any(drained, fn(e) {
      case e {
        telemetry.CallTimedOut(_) -> True
        _ -> False
      }
    })
  should.be_true(saw_timeout)
  telemetry.reset()
}

pub fn call_negative_timeout_emits_clamped_timeout_value_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let _ = global.call(gs, fn(_reply) { 1 }, codec.int_decoder(), -50)

  let drained = drain_events(events, [])
  let saw_clamped_timeout =
    list.any(drained, fn(e) {
      case e {
        telemetry.CallTimedOut(ms) if ms == 0 -> True
        _ -> False
      }
    })
  should.be_true(saw_clamped_timeout)
  telemetry.reset()
}

pub fn call_isolated_caller_timeout_emits_call_timed_out_test() {
  // Use a target that never replies, with timeout=1ms so the
  // **inner** `call` times out (which already emits its own
  // `CallTimedOut`). We then assert the outer caller-side path
  // *also* emits exactly once for the user-visible timeout. With a
  // 1ms inner timeout the inner emits during normal flow; the
  // caller-side guard only emits when no inner result was drained.
  // This double-test shape is intentional: it pins the contract
  // that *every* `Error(Timeout)` returned by `call_isolated`
  // surfaces at least one `CallTimedOut` to observers.
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let _ = global.call_isolated(gs, fn(_reply) { 1 }, codec.int_decoder(), 1)

  let drained = drain_events(events, [])
  let saw_timeout =
    list.any(drained, fn(e) {
      case e {
        telemetry.CallTimedOut(_) -> True
        _ -> False
      }
    })
  should.be_true(saw_timeout)
  telemetry.reset()
}

pub fn call_isolated_negative_timeout_emits_clamped_timeout_value_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let _ = global.call_isolated(gs, fn(_reply) { 1 }, codec.int_decoder(), -50)

  let drained = drain_events(events, [])
  let saw_clamped_timeout =
    list.any(drained, fn(e) {
      case e {
        telemetry.CallTimedOut(ms) if ms == 0 -> True
        _ -> False
      }
    })
  should.be_true(saw_clamped_timeout)
  telemetry.reset()
}

pub fn call_isolated_proxy_crash_emits_proxy_crashed_test() {
  // A `make_request` that exits the proxy before it can send a
  // result lands in the `IsolatedProxyDown` branch. In production
  // this branch fires when user code panics inside `make_request`
  // or the response codec; here we use a clean `shutdown` exit via
  // `exit_self_shutdown()` to avoid SASL CRASH REPORT noise. The
  // branch is reason-agnostic: any DOWN signal exercises the same
  // code path. The caller still observes `Error(Timeout)` for
  // typed-error consistency, but the dedicated `CallProxyCrashed`
  // event lets observability separate "proxy died before sending"
  // from "real RPC timeout".
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let _ =
    global.call_isolated(
      gs,
      fn(_reply) { exit_self_shutdown() },
      codec.int_decoder(),
      1000,
    )

  let drained = drain_events(events, [])
  let saw_proxy_crashed =
    list.any(drained, fn(e) {
      case e {
        telemetry.CallProxyCrashed -> True
        _ -> False
      }
    })
  should.be_true(saw_proxy_crashed)

  // Verify CallTimedOut is NOT emitted on the proxy-crash branch:
  // the contract is that crash-cause and timeout-cause produce
  // distinct events.
  let saw_timeout =
    list.any(drained, fn(e) {
      case e {
        telemetry.CallTimedOut(_) -> True
        _ -> False
      }
    })
  should.be_false(saw_timeout)
  telemetry.reset()
}

pub fn call_target_down_event_fires_test() {
  telemetry.reset()
  let events = process.new_subject()
  capture_into(events)

  let dead_pid = spawn_short_lived()
  process.sleep(20)
  let gs = global.from_pid(dead_pid, codec.int_encoder(), codec.int_decoder())
  let _ = global.call(gs, fn(_reply) { 1 }, codec.int_decoder(), 30)

  let drained = drain_events(events, [])
  let saw_target_down =
    list.any(drained, fn(e) {
      case e {
        telemetry.CallTargetDown -> True
        _ -> False
      }
    })
  should.be_true(saw_target_down)
  telemetry.reset()
}
