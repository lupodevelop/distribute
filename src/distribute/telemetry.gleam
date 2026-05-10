//// Single-event observability sink.
////
//// `distribute` emits structured `Event` values at every load-bearing
//// boundary: registry operations, atom-budget exhaustion, payload
//// rejection, call lifecycle, decode failures, orphan cleanup. The
//// sink is **opt-in**: with no sink installed, every emit point is a
//// branch on a missing ETS entry and a return. Microseconds, lock-
//// free, zero allocation.
////
//// ## Why one global sink instead of `_observed` variants
////
//// `_observed` start variants already cover decode errors at the
//// per-actor boundary. They are kept for that use case, where the
//// hook needs to capture the exact handler context. The telemetry
//// sink is for cross-cutting observability: a single subscriber that
//// wants visibility into *all* registry/cluster/payload/call events
//// without instrumenting every call site.
////
//// ## No structured error types in `Event`
////
//// `Event` variants carry **stringified** failure reasons (via
//// `*_error_to_string`) instead of structured `RegisterError` /
//// `DecodeError` / `UnregisterError` references. Two reasons:
////
//// 1. **No import cycles**. `telemetry` is a leaf module that every
////    other module can depend on; the moment we re-exported
////    `registry.RegisterError` here, `registry → telemetry → registry`
////    would close. Stringified reasons keep `telemetry` independent.
//// 2. **Operationally, downstream sinks log/meter strings anyway**.
////    A `Prometheus` exporter increments a counter labelled with the
////    failure kind; a structured logger writes the rendered message.
////    Pattern-matching on a typed sum is rarely the right shape for
////    observability output.
////
//// ## Building on top
////
//// Downstream packages (`distribute_telemetry`, `distribute_audit`,
//// `distribute_metrics`, `distribute_otel`) install a single sink and
//// fan out from there. The contract is intentionally narrow: `Event`
//// is a pure data sum, the sink is `fn(Event) -> Nil`. No subscription
//// manager, no filtering, no async fan-out. Compose with whatever
//// your stack already has.
////
//// ## Example
////
//// ```gleam
//// import distribute/telemetry
//// import gleam/io
//// import gleam/int
////
//// pub fn main() {
////   telemetry.install(fn(event) {
////     case event {
////       telemetry.ActorRegistered(name) ->
////         io.println("registered: " <> name)
////       telemetry.PayloadRejected(size, _, _) ->
////         io.println("payload rejected: " <> int.to_string(size))
////       _ -> Nil
////     }
////   })
////   // ... rest of your app
//// }
//// ```
////
//// ## Contract for downstream consumers
////
//// **Stability**. `Event` variants are append-only within a major
//// version: a 4.x release will never remove or rename a variant, but
//// it may add new ones. Downstream sinks must include a `_ -> Nil`
//// catch-all to stay forward-compatible across minor releases. New
//// variants will appear in `CHANGELOG.md` under "Added".
////
//// **Field semantics**, by event:
////
//// - `ActorRegistered(name)`, `ActorRegistrationFailed(name, reason)`,
////   `ActorUnregistered(name, removed)`: `name` is the binary name
////   string the caller passed; `reason` is the rendered
////   `register_error_to_string`; `removed` is `True` only when
////   `:global` actually removed the entry on this VM (i.e. local
////   ownership ACL passed and `whereis` succeeded).
//// - `AtomBudgetExhausted(attempted_input, where)`:`attempted_input`
////   is the raw binary the caller passed (treat as untrusted: log
////   bytes, never feed back to atom creation). `where` distinguishes
////   `connect` / `start_node` / `ping`. All three paths emit on
////   budget refusal; `ping`'s emit is wired at the FFI layer
////   (`cluster_ffi:ping/1`) because its public Gleam return is `Bool`
////   with no error variant to carry the typed refusal.
//// - `PayloadRejected(size_bytes, cap_bytes, where)`:`size_bytes`
////   is the **encoded** payload size (post-codec), not the source
////   value's heap size. `cap_bytes` is the active
////   `config.max_payload_size_bytes` at the moment of rejection.
//// - `DecodeFailed(error_message, where)`: `error_message` is the
////   rendered `decode_error_to_string`. `where` distinguishes
////   one-shot `receive` / `call` reply / actor-inbound paths.
//// - `CallTimedOut(elapsed_ms)`: `elapsed_ms` is the **effective**
////   timeout used by the receive path (user input clamped to
////   non-negative), not the real wall-clock elapsed (the BEAM
////   does not expose that without monotonic measurement at the call
////   site). For real elapsed measurement, time around the call in
////   the sink itself. For `call_isolated`, treat this as
////   at-least-once telemetry: in a timeout race you may observe two
////   `CallTimedOut` emits for one user call (inner `call` timeout +
////   outer caller-side timeout).
//// - `CallTargetDown` carries no payload. Two emit paths fold here:
////   the caller observed a dead target at call time, or the monitor
////   fired during the wait.
//// - `OrphanKillEscalated(pid)`: the carried Pid is the orphan
////   actor that ignored `exit(_, shutdown)` past the grace window
////   and was force-killed. Operationally significant: log and alert.
////
//// **Concurrency model**. `emit/1` runs the installed sink **inline
//// in the calling process**. There is no buffering, no async
//// dispatch, and no retry. If the sink raises, the FFI catches the
//// exception and logs it via `logger:warning/2` (event tag plus
//// stack trace, no payload, so sensitive `attempted_input` strings
//// do not leak into shared logs). The library-internal process that
//// emitted the event survives. This protects actors, the call
//// selector, and the `call_isolated` proxy from being taken down by
//// a buggy sink, while still surfacing the bug operationally.
////
//// Production sinks should still be written fail-fast and total:
//// the catch is a safety net for shipped bugs, not a license to
//// raise. Async fan-out (routing to a metrics aggregator) is best
//// done by `process.send`-ing the event to a background worker
//// from inside the sink itself.
////
//// **Visibility model**. `install/1` is last-wins, the most recent
//// caller replaces the previous sink. There is no subscription list,
//// no priority, no filter chain. A library that wants multiple
//// downstream consumers (e.g. logger + metrics + audit) installs
//// **one** sink that fans out internally to whatever it cares about.
////
//// **Storage**. Sink reference is held in an ETS table
//// (`distribute_telemetry_sink_table`, `public`, `read_concurrency`).
//// Reads on the hot path are lock-free; writes (`install`/`reset`)
//// trigger no global GC. Production code is expected to call
//// `install` exactly once at boot.

import gleam/erlang/process
import gleam/option

/// A structured observability event emitted by `distribute` at every
/// load-bearing boundary. Pattern-match in your sink to project the
/// fields you care about and drop the rest.
///
/// Variants are added in minor versions; downstream sinks should
/// always have a `_ -> Nil` catch-all to stay forward-compatible.
pub type Event {
  // Registry lifecycle ------------------------------------------------------
  /// A `:global` registration succeeded for `name`.
  ActorRegistered(name: String)
  /// A `:global` registration attempt failed. `reason` is the rendered
  /// `register_error_to_string`; the library kills the orphan actor
  /// automatically.
  ActorRegistrationFailed(name: String, reason: String)
  /// `:global` `unregister` returned a result. `removed` is `True` when
  /// the entry was successfully removed; `False` for `NotFound` /
  /// `NotOwned` outcomes.
  ActorUnregistered(name: String, removed: Bool)

  // Atom-creation guardrail -------------------------------------------------
  /// `cluster.connect`/`ping`/`start_node` rejected a fresh-atom
  /// reservation past `config.max_distribution_atoms`.
  AtomBudgetExhausted(attempted_input: String, where: AtomBudgetOrigin)

  // I/O boundary ------------------------------------------------------------
  /// A typed boundary refused a payload that exceeded
  /// `config.max_payload_size_bytes`. `cap_bytes` is the active limit
  /// at the moment of rejection; `where` distinguishes which boundary.
  PayloadRejected(size_bytes: Int, cap_bytes: Int, where: PayloadOrigin)
  /// A binary failed `decode_checked` or a top-level decoder.
  /// `error_message` is the rendered `decode_error_to_string`.
  DecodeFailed(error_message: String, where: DecodeOrigin)

  // Call lifecycle ----------------------------------------------------------
  /// A `call` returned `Error(Timeout)`. `elapsed_ms` is the effective
  /// timeout used by the receive path (input clamped to non-negative)
  /// Not the real elapsed time, which the BEAM does not expose
  /// without monotonic measurement at the call site. For
  /// `call_isolated`, emits are at-least-once: a timeout race can
  /// produce two `CallTimedOut` events for one user call.
  CallTimedOut(elapsed_ms: Int)
  /// A `call` returned `Error(TargetDown)`. The target was either dead
  /// at call time or died during the call.
  CallTargetDown
  /// `call_isolated`'s proxy process died before sending its result.
  /// The caller still sees `Error(Timeout)` for typed-error consistency,
  /// but this event distinguishes the cause: a crash inside `make_request`,
  /// the response codec, or another component evaluated by the proxy.
  /// Operationally significant: a steady stream of these events points
  /// at user-side bugs, not at network or peer-side latency.
  CallProxyCrashed

  // Actor lifecycle hardening -----------------------------------------------
  /// `terminate_orphan_gracefully` exhausted the shutdown grace window
  /// and escalated to `process.kill`. Operationally significant:
  /// orphan kill means a registered actor either trapped exits and
  /// failed to unregister, or registration races outpaced cleanup.
  OrphanKillEscalated(pid: process.Pid)

  // Split-brain conflict resolution -----------------------------------------
  /// `:global` invoked the conflict resolver because the same name was
  /// claimed by two PIDs (typically after a partition heal). `winner`
  /// is the PID the resolver kept; `Some(pid)` for `Keep(pid)`,
  /// `None` for `KillBoth`. Operationally critical: a steady stream
  /// of these events means the cluster is "flapping". partitions
  /// are healing repeatedly. One-off events after a known network
  /// incident are normal; high frequency points at infrastructure
  /// instability that the application code cannot fix on its own.
  ConflictResolved(name: String, winner: option.Option(process.Pid))
  /// The user-supplied conflict resolver crashed or ran past its
  /// timeout. `reason` carries the rendered exception class+reason
  /// or the literal `"resolver timed out"`. The library applied a
  /// deterministic fallback (lowest term-ordered PID wins) so the
  /// global_name_server worker never blocks; the fallback's pick is
  /// emitted as a separate `ConflictResolved` event right after.
  ConflictResolverFailed(name: String, reason: String)
}

/// Where in the system an atom-budget reservation was refused.
pub type AtomBudgetOrigin {
  AtomBudgetOnConnect
  AtomBudgetOnPing
  AtomBudgetOnStartNode
}

/// Where in the system a payload was rejected for size.
pub type PayloadOrigin {
  PayloadOnSend
  PayloadOnCallRequest
  PayloadOnCallResponse
  PayloadOnReply
  PayloadOnReceive
  PayloadOnActorInbound
}

/// Where in the system a decode failure happened.
pub type DecodeOrigin {
  DecodeOnReceive
  DecodeOnCallReply
  DecodeOnActorInbound
}

/// A telemetry sink. Pure side-effect from the library's perspective:
/// the return value is discarded. The sink runs inline in the emit
/// point's process, so a slow sink slows the emit point. Write
/// async fan-out into the sink itself if you need it.
pub type EventSink =
  fn(Event) -> Nil

// ---------------------------------------------------------------------------
// FFI bindings (private)
// ---------------------------------------------------------------------------

@external(erlang, "telemetry_ffi", "install")
fn install_ffi(sink: EventSink) -> Nil

@external(erlang, "telemetry_ffi", "reset")
fn reset_ffi() -> Nil

@external(erlang, "telemetry_ffi", "emit")
fn emit_ffi(event: Event) -> Nil

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Install (or replace) the telemetry sink. Last-wins, a second
/// `install` call replaces the previous sink without warning. This is
/// intentional: tests that swap sinks between cases stay terse, and
/// production code should call `install` exactly once at boot.
pub fn install(sink: EventSink) -> Nil {
  install_ffi(sink)
}

/// Remove the active sink. After `reset`, every emit is a no-op. Used
/// by tests for isolation; not intended for production code.
@internal
pub fn reset() -> Nil {
  reset_ffi()
}

/// Emit an event through the active sink. No-op when no sink is
/// installed. `@internal` so downstream packages know they are
/// reaching for an unstable surface. Stick to the `Event` variants
/// in this module.
@internal
pub fn emit(event: Event) -> Nil {
  emit_ffi(event)
}
