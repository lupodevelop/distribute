//// Named actor lifecycle: start, register, supervise, pool.
////
//// ## Resource cleanup and the `{terminate, Reason}` gap
////
//// `gleam/otp/actor` 1.x does **not** implement OTP's
//// `{terminate, Reason}` system message
//// (see `gleam_otp_external.erl`; there is an explicit `TODO`).
//// That has two consequences for any actor that owns external
//// resources (file handles, ETS tables, ports, locks, sockets):
////
//// 1. **Handler-controlled exits run user code first.** If your
////    handler returns `receiver.Stop` or `receiver.StopAbnormal`,
////    you have full control: release every resource you own *before*
////    returning the stop variant. The actor will then exit and the
////    BEAM frees its mailbox.
//// 2. **External terminations skip the actor.** When the actor is
////    `process.kill`-ed (uncatchable), or when the supervisor sends
////    `exit(Pid, shutdown)`, gleam/otp/actor does not currently
////    convert it into a callback. Resources owned only on the
////    actor's heap are released by the BEAM (memory), but any
////    *external* handle (a file, a TCP connection, an ETS table you
////    forgot to make public) leaks.
////
//// **The OTP-pure mitigation** is the "linked resource owner"
//// pattern: spawn a tiny dedicated process to own the external
//// resource, monitor the actor, and run a `close` callback when the
//// actor dies for any reason. The library ships
//// `actor.start_resource_owner/3` as a ready-made helper for this
//// pattern. See `docs/recipes.md` for the underlying recipe.
////
//// When upstream gleam/otp ships termination callbacks (tracking
//// issue: gleam-lang/otp#126) the helper becomes redundant for the
//// actor case. Its API is shaped so the migration is a one-line
//// change in user code and a deprecation here, no semantics shift.

import distribute/codec
import distribute/config
import distribute/global
import distribute/receiver
import distribute/registry
import distribute/telemetry
import gleam/erlang/process

import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{worker}

/// Grace period (ms) given to an orphan to clean up after `shutdown` before
/// we escalate to brutal `kill`. Mirrors the OTP supervisor convention.
const orphan_shutdown_grace_ms: Int = 100

@external(erlang, "distribute_ffi_utils", "exit_shutdown")
fn exit_shutdown(pid: process.Pid) -> Bool

/// Two-phase orphan termination, mirroring `supervisor:terminate_child/2`.
///
/// 1. Unlink so the brutal phase (if reached) does not propagate to the
///    caller via the default actor link.
/// 2. Send `exit(Pid, shutdown)`. Non-trapping actors die immediately
///    with reason `shutdown`; the monitor we set fires within the grace
///    window and we return cleanly. Trapping actors receive
///    `{'EXIT', From, shutdown}` as a mailbox message.
/// 3. If the orphan is still alive after `orphan_shutdown_grace_ms`,
///    escalate to `process.kill` (uncatchable).
///
/// ## Gleam OTP caveat
///
/// `gleam/otp/actor` (as of 1.x) does not yet implement the
/// `{terminate, Reason}` system message (see TODO in
/// `gleam_otp_external.erl`), so a true OTP-style graceful shutdown via
/// `sys:terminate/2` or `gen:stop/3` is currently a no-op for those
/// actors. The system message is logged as "unexpected" and the actor
/// keeps running until the kill phase. We use the simpler
/// `exit(Pid, shutdown)` path: non-trapping actors die immediately,
/// trapping actors fall through to brutal kill within the grace window.
/// When upstream `gleam/otp` ships terminate support, switching to
/// `gen:stop` here will give trapping actors a real `terminate` callback
/// invocation without changing this function's contract.
///
/// Used by `start_registered`, `start_registered_observed`, and `child_spec`
/// when global registration fails after a successful actor init.
fn terminate_orphan_gracefully(pid: process.Pid) -> Nil {
  process.unlink(pid)
  let mon = process.monitor(pid)
  let _ = exit_shutdown(pid)
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(mon, fn(_) { Nil })
  case process.selector_receive(selector, orphan_shutdown_grace_ms) {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      // Grace expired, orphan still alive. Brutal kill, demonitor.
      process.kill(pid)
      process.demonitor_process(mon)
      telemetry.emit(telemetry.OrphanKillEscalated(pid))
    }
  }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Start a named actor, with a callback for decode errors.
///
/// Useful for logging or metering malformed messages across nodes
/// (e.g. during rolling deploys with mismatched codec versions).
pub fn start_observed(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
  init_timeout_ms: Int,
  on_decode_error: fn(codec.DecodeError) -> Nil,
) -> Result(global.GlobalSubject(msg), actor.StartError) {
  case
    receiver.start_distributed_worker_observed(
      registry.typed_name_to_string(typed_name),
      initial_state,
      registry.typed_name_encoder(typed_name),
      registry.typed_name_decoder(typed_name),
      handler,
      on_decode_error,
      init_timeout_ms,
    )
  {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Start a named actor and register it globally, with a callback for decode errors.
pub fn start_registered_observed(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
  init_timeout_ms: Int,
  on_decode_error: fn(codec.DecodeError) -> Nil,
) -> Result(global.GlobalSubject(msg), StartRegisteredError) {
  let name = registry.typed_name_to_string(typed_name)
  case
    receiver.start_distributed_worker_observed(
      name,
      initial_state,
      registry.typed_name_encoder(typed_name),
      registry.typed_name_decoder(typed_name),
      handler,
      on_decode_error,
      init_timeout_ms,
    )
  {
    Ok(started) -> {
      case registry.register_global(typed_name, started.data) {
        Ok(_) -> Ok(started.data)
        Error(err) -> {
          terminate_orphan_gracefully(started.pid)
          Error(GlobalRegisterFailed(err))
        }
      }
    }
    Error(err) -> Error(ActorStartFailed(err))
  }
}

/// Start a named actor.
pub fn start(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(global.GlobalSubject(msg), actor.StartError) {
  case
    receiver.start_distributed_worker(
      registry.typed_name_to_string(typed_name),
      initial_state,
      registry.typed_name_encoder(typed_name),
      registry.typed_name_decoder(typed_name),
      handler,
      init_timeout_ms,
    )
  {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Like `start`, but uses `config.get().default_init_timeout_ms` as the init timeout.
pub fn start_default(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
) -> Result(global.GlobalSubject(msg), actor.StartError) {
  start(
    typed_name,
    initial_state,
    handler,
    config.get().default_init_timeout_ms,
  )
}

/// Like `start_registered`, but uses `config.get().default_init_timeout_ms` as the init timeout.
pub fn start_registered_default(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
) -> Result(global.GlobalSubject(msg), StartRegisteredError) {
  start_registered(
    typed_name,
    initial_state,
    handler,
    config.get().default_init_timeout_ms,
  )
}

/// Like `child_spec`, with `config.get().default_init_timeout_ms`.
pub fn child_spec_default(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
) -> supervision.ChildSpecification(global.GlobalSubject(msg)) {
  child_spec(
    typed_name,
    initial_state,
    handler,
    config.get().default_init_timeout_ms,
  )
}

/// OTP child spec for a named actor that auto-registers on (re)start.
pub fn child_spec(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
  init_timeout_ms: Int,
) -> supervision.ChildSpecification(global.GlobalSubject(msg)) {
  let name = registry.typed_name_to_string(typed_name)
  worker(fn() {
    case
      receiver.start_distributed_worker(
        name,
        initial_state,
        registry.typed_name_encoder(typed_name),
        registry.typed_name_decoder(typed_name),
        handler,
        init_timeout_ms,
      )
    {
      Ok(started) -> {
        // We do NOT pre-emptively unregister `name`. With the local-
        // ownership ACL in place, unregistering would also remove
        // entries owned by *unrelated* live processes on this same VM
        // For instance, two co-located applications that both use
        // `distribute` and happen to register a name with the same
        // string. The OTP-pure recovery path for a stale entry (a
        // crashed previous incarnation that `:global` has not yet
        // garbage-collected) is the supervisor's restart-with-backoff
        // loop: `register_global` fails with `AlreadyExists`, we kill
        // our orphan, the supervisor retries; by the next attempt
        // `:global`'s PID monitor has cleaned the dead entry and the
        // registration succeeds.
        case registry.register_global(typed_name, started.data) {
          Ok(_) -> Ok(started)
          Error(_) -> {
            terminate_orphan_gracefully(started.pid)
            Error(actor.InitFailed("registration_failed"))
          }
        }
      }
      Error(err) -> Error(err)
    }
  })
}

/// Error from `start_registered`: distinguishes an actor init failure from
/// a `:global` registration failure after a successful start.
pub type StartRegisteredError {
  /// The OTP actor failed to start (init crashed, timeout, etc.).
  ActorStartFailed(actor.StartError)
  /// The actor started but `:global` registration failed; the orphaned
  /// actor process has been killed.
  GlobalRegisterFailed(registry.RegisterError)
}

/// Render a `StartRegisteredError` as a human-readable string. Mirrors
/// the `*_error_to_string` formatters published by the other error
/// modules so observability paths can `io.println` an error directly
/// without pattern-matching at every call site.
pub fn start_registered_error_to_string(err: StartRegisteredError) -> String {
  case err {
    ActorStartFailed(actor.InitTimeout) -> "Actor init timed out"
    ActorStartFailed(actor.InitFailed(reason)) ->
      "Actor init failed: " <> reason
    ActorStartFailed(actor.InitExited(_)) -> "Actor init process exited"
    GlobalRegisterFailed(re) ->
      "Global registration failed: " <> registry.register_error_to_string(re)
  }
}

/// Start an actor and register it globally. Kills the actor if
/// registration fails.
///
/// See also: `start_registered_default/3` (configured init timeout),
/// `start_registered_observed/5` (decode-error hook),
/// `start_supervised/4` (auto-restart on crash).
pub fn start_registered(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(global.GlobalSubject(msg), StartRegisteredError) {
  let name = registry.typed_name_to_string(typed_name)
  case
    receiver.start_distributed_worker(
      name,
      initial_state,
      registry.typed_name_encoder(typed_name),
      registry.typed_name_decoder(typed_name),
      handler,
      init_timeout_ms,
    )
  {
    Ok(started) -> {
      case registry.register_global(typed_name, started.data) {
        Ok(_) -> Ok(started.data)
        Error(err) -> {
          terminate_orphan_gracefully(started.pid)
          Error(GlobalRegisterFailed(err))
        }
      }
    }
    Error(err) -> Error(ActorStartFailed(err))
  }
}

/// Like `start_supervised`, with `config.get().default_init_timeout_ms`.
pub fn start_supervised_default(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
) -> Result(process.Pid, actor.StartError) {
  start_supervised(
    typed_name,
    initial_state,
    handler,
    config.get().default_init_timeout_ms,
  )
}

/// Start a supervised actor that auto-registers on (re)start.
///
/// If registration fails, the worker crashes and the supervisor retries,
/// which is the correct OTP pattern for transient registration failures.
pub fn start_supervised(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(process.Pid, actor.StartError) {
  let spec = child_spec(typed_name, initial_state, handler, init_timeout_ms)
  let builder =
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.add(spec)

  case static_supervisor.start(builder) {
    Ok(sup) -> Ok(sup.pid)
    Error(err) -> Error(err)
  }
}

/// Like `pool`, with `config.get().default_init_timeout_ms`.
pub fn pool_default(
  typed_name: registry.TypedName(msg),
  size: Int,
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
) -> Result(process.Pid, actor.StartError) {
  pool(
    typed_name,
    size,
    initial_state,
    handler,
    config.get().default_init_timeout_ms,
  )
}

/// Start N supervised actors, registered as `name_1` .. `name_N`.
///
/// Returns `Error(actor.InitFailed(...))` if `size < 1`. `list.range`
/// would otherwise produce a degenerate list of weird worker names
/// (e.g. `name_0`, `name_-1`) and silently start a useless supervisor.
///
/// > [!WARNING]
/// > **Cascading Failure Risk**:
/// > `:global` and fixed pools do not mix perfectly. If a single worker
/// > in the pool fails its global registration (e.g. because `name_4` is
/// > legitimately held by a node on the other side of a network split),
/// > the worker crashes. The pool supervisor will try to restart it. If the
/// > conflict persists, the supervisor exhausts its `MaxR` restart intensity
/// > and **crashes the entire pool**. This means a collision on 1 worker
/// > brings down all N workers, causing the parent supervisor to restart the
/// > whole pool. This architectural limit will be solved natively when the
/// > `syn` registry backend is introduced in v4.2.0 (via `syn` process groups).
pub fn pool(
  typed_name: registry.TypedName(msg),
  size: Int,
  initial_state: state,
  handler: fn(msg, state) -> receiver.HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(process.Pid, actor.StartError) {
  case size < 1 {
    True ->
      Error(actor.InitFailed(
        "pool size must be >= 1, got " <> int.to_string(size),
      ))
    False -> {
      let specs =
        index_list(size)
        |> list.map(fn(i) {
          let worker_tn = registry.pool_member(typed_name, i)
          child_spec(worker_tn, initial_state, handler, init_timeout_ms)
        })

      let builder =
        list.fold(
          specs,
          static_supervisor.new(static_supervisor.OneForOne),
          fn(acc, spec) { static_supervisor.add(acc, spec) },
        )

      case static_supervisor.start(builder) {
        Ok(sup) -> Ok(sup.pid)
        Error(err) -> Error(err)
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Resource-owner helper (workaround for the missing terminate callback)
// ---------------------------------------------------------------------------

/// How long the owner waits between liveness re-checks while watching
/// `lifetime`. Picked to be long enough that an idle owner is essentially
/// free, short enough that a `DOWN` we somehow missed (cosmic ray,
/// runtime-tracing tool dropping the message) is detected within seconds.
fn resource_owner_poll_ms() -> Int {
  config.get().resource_owner_poll_ms
}

/// Spawn a dedicated process that owns an external resource and runs
/// `close(resource)` when `lifetime` dies for any reason. Workaround
/// for the missing `{terminate, Reason}` callback in
/// `gleam/otp/actor` 1.x: external resources (database connections,
/// distributed locks, NIF handles, OS pipes) cannot rely on the
/// actor's exit to free them, but a dedicated observer process can.
///
/// Returns the owner's PID, in case you need to stop the owner
/// independently of the lifetime PID.
///
/// ## Why this shape
///
/// The resource is opened **inside the owner**, so it is BEAM-process-
/// owned by the owner. That matters for resources whose lifetime is
/// tied to a specific process at the runtime level (ETS tables, ports,
/// `gen_tcp` controlling process). 
///
/// The owner is **linked** to the caller (which is usually the actor's 
/// `init` function). It also traps exits (`process.trap_exits(True)`). 
/// This guarantees a **fail-fast** behaviour: if `open()` crashes before 
/// it returns, the owner dies, and because it is linked, it pulls the 
/// actor down with it immediately. This prevents the "partial failure" 
/// scenario where the actor survives but its critical resource failed 
/// to initialize.
///
/// The owner uses a `process.monitor` on `lifetime`. Monitor delivery
/// is asynchronous but reliable: the BEAM guarantees a `DOWN` for
/// every monitored PID, and a monitor on an already-dead PID fires
/// immediately. That makes the helper safe to call right after the
/// actor has started: even if there is a microsecond gap before the
/// monitor goes up, the BEAM resolves it correctly.
///
/// ## Usage
///
/// ```gleam
/// import distribute
/// import distribute/actor as dist_actor
/// import distribute/global
///
/// let assert Ok(gs) =
///   distribute.start_registered(name, init, handler)
/// let assert Ok(actor_pid) = global.owner(gs)
/// let _owner = dist_actor.start_resource_owner(
///   fn() { open_postgres_pool() },
///   fn(pool) { close_postgres_pool(pool) },
///   actor_pid,
/// )
/// ```
///
/// If the actor needs to *use* the resource, have `open` build a
/// `process.Subject` the owner serves and pass it through your
/// actor's `init`.
///
/// ## Failure modes
///
/// - `open` raises: the owner dies before the resource is opened. Because
///   the owner is linked to the caller (the actor), the actor receives an
///   exit signal and crashes immediately. This fail-fast behaviour avoids
///   partial failures where the actor runs without its resource.
/// - `close` raises: the owner dies after the panic, no further
///   cleanup runs. The resource may be in a partially-closed state.
/// - `lifetime` is already dead at call time: monitor fires `DOWN`
///   immediately, `close` runs on the next scheduler tick.
/// - The owner is `process.kill`-ed: cleanup is uncatchable, BEAM
///   reaps the resource via process death (only effective for
///   BEAM-process-tied resources). External handles leak. This is
///   the same uncatchable-kill caveat OTP itself has.
///
/// ## Future
///
/// When `gleam/otp/actor` ships native termination callbacks
/// (gleam-lang/otp#126), this helper becomes redundant for the
/// actor case. The contract will not shift: callers can replace the
/// `actor.start_resource_owner(open, close, pid)` line with the
/// upstream API and delete the helper call site. The function will
/// stay (deprecated) for the more general "tie cleanup to any PID"
/// pattern.
pub fn start_resource_owner(
  open: fn() -> resource,
  close: fn(resource) -> Nil,
  lifetime: process.Pid,
) -> process.Pid {
  process.spawn(fn() {
    process.trap_exits(True)
    let resource = open()
    let mon = process.monitor(lifetime)
    let down_selector =
      process.new_selector()
      |> process.select_specific_monitor(mon, fn(_) { Nil })
    await_lifetime_death(down_selector, lifetime)
    close(resource)
    Nil
  })
}

/// Block the owner until the lifetime PID is dead. Long-polls the
/// `DOWN` selector and re-checks `process.is_alive` on each timeout.
/// Without the re-check, a buggy or instrumented runtime that loses a
/// `DOWN` message would leave us waiting forever; with it, we converge
/// on the truth within `resource_owner_poll_ms`.
fn await_lifetime_death(
  selector: process.Selector(Nil),
  lifetime: process.Pid,
) -> Nil {
  case process.selector_receive(selector, resource_owner_poll_ms()) {
    Ok(Nil) -> Nil
    Error(Nil) ->
      case process.is_alive(lifetime) {
        False -> Nil
        True -> await_lifetime_death(selector, lifetime)
      }
  }
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

/// Build `[1, 2, ..., n]` for `n >= 1`. Replacement for `list.range`,
/// which was removed in `gleam_stdlib` 1.0.
fn index_list(n: Int) -> List(Int) {
  do_index_list(n, [])
}

fn do_index_list(i: Int, acc: List(Int)) -> List(Int) {
  case i < 1 {
    True -> acc
    False -> do_index_list(i - 1, [i, ..acc])
  }
}
