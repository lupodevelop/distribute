/// Global name registration via Erlang's `:global` module.
///
/// The key idea is `TypedName(msg)`: a name string bundled with its
/// encoder/decoder. Define it once, pass it to both registration and
/// lookup, and the compiler makes sure the message type matches.
///
/// ```gleam
/// let counter = registry.named("counter", codec.int())
///
/// let assert Ok(gs) = actor.start(counter, 0, handler, 5000)
/// let assert Ok(Nil) = registry.register_global(counter, gs)
/// let assert Ok(found) = registry.lookup(counter)
/// ```
import distribute/codec
import distribute/conflict
import distribute/global
import distribute/telemetry
import gleam/erlang/process
import gleam/int

// ---------------------------------------------------------------------------
// TypedName: compile-time protocol safety
// ---------------------------------------------------------------------------

/// A name bound to an encoder/decoder pair.
///
/// The `msg` type links registration and lookup: register with
/// `TypedName(Int)`, look up a `GlobalSubject(Int)`.
pub opaque type TypedName(msg) {
  TypedName(
    name: String,
    encoder: codec.Encoder(msg),
    decoder: codec.Decoder(msg),
  )
}

/// Create a typed name from explicit encoder/decoder.
pub fn typed_name(
  name: String,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> TypedName(msg) {
  TypedName(name:, encoder:, decoder:)
}

/// Create a typed name from a bundled `Codec`.
///
/// ```gleam
/// let counter = registry.named("counter", codec.int())
/// ```
///
/// ## Caution: split-brain conflict resolution is brutal
///
/// Names registered via `register_global` are stored in Erlang's
/// `:global` table. After a network partition heals, `:global` may
/// discover the same name registered to different PIDs on each
/// side. Its default conflict resolver (`random_notify_name/3`)
/// picks a winner and sends an **uncatchable**
/// `exit(loser_pid, kill)` to the other PID. The library cannot
/// trap this and there is no user hook to plug in: it is a
/// property of `:global`, not of `distribute`.
///
/// Operational consequences:
///
/// - critical singletons can vanish without warning during a
///   partition-heal event;
/// - any work in flight on the loser PID is lost;
/// - Subjects obtained from a pre-split `lookup` point at the
///   loser and silently drop sends after the kill.
///
/// Mitigation: subscribe to `cluster_monitor` and treat each
/// `NodeUp` after a known partition as a "re-validate critical
/// state" signal; re-`lookup` registered actors before each
/// critical operation rather than caching Subjects across long
/// timescales. For workloads where this is unacceptable, use `pg`
/// (process groups, eventually-consistent, no kill-on-merge) or a
/// consensus layer instead. See `docs/safety_and_limits.md` under
/// "BEAM and OS-level risks" for the full discussion.
pub fn named(name: String, c: codec.Codec(msg)) -> TypedName(msg) {
  TypedName(name:, encoder: c.encoder, decoder: c.decoder)
}

/// Get the name string.
pub fn typed_name_to_string(tn: TypedName(msg)) -> String {
  tn.name
}

/// Get the encoder.
pub fn typed_name_encoder(tn: TypedName(msg)) -> codec.Encoder(msg) {
  tn.encoder
}

/// Get the decoder.
pub fn typed_name_decoder(tn: TypedName(msg)) -> codec.Decoder(msg) {
  tn.decoder
}

/// Derive a `TypedName` for a pool worker.
///
/// `pool_member(base, 2)` returns a `TypedName` with name `"<base>_2"` and
/// the same codecs.
pub fn pool_member(base: TypedName(msg), index: Int) -> TypedName(msg) {
  TypedName(..base, name: base.name <> "_" <> int.to_string(index))
}

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

/// Errors from registry operations.
pub type RegisterError {
  /// Name is already registered by another process.
  AlreadyExists
  /// Process is not alive or invalid.
  InvalidProcess
  /// Name is empty, too long, or contains invalid characters.
  InvalidArgument(String)
  /// Network partition or connectivity issue.
  NetworkError(String)
  /// Generic registration failure.
  RegistrationFailed(String)
}

/// Outcome of a failed `unregister/1`. The success case
/// (`Ok(Nil)`) means *we removed the entry*; an error tells the
/// caller why we did not, so debugging surfaces the real cause
/// instead of a silent no-op.
pub type UnregisterError {
  /// Name was not registered at the moment of the call. Idempotent
  /// callers can ignore this; observability paths surface it.
  NotFound
  /// Name resolved to a PID on another node. The local-ownership ACL
  /// refused to forward the unregister, the `:global` table is
  /// untouched.
  NotOwned
}

pub fn unregister_error_to_string(err: UnregisterError) -> String {
  case err {
    NotFound -> "Name not registered"
    NotOwned -> "Name owned by a remote node"
  }
}

pub fn register_error_to_string(err: RegisterError) -> String {
  case err {
    AlreadyExists -> "Name is already registered"
    InvalidProcess -> "Invalid or dead process"
    InvalidArgument(reason) -> "Invalid argument: " <> reason
    NetworkError(reason) -> "Network error: " <> reason
    RegistrationFailed(reason) -> "Registration failed: " <> reason
  }
}

// ---------------------------------------------------------------------------
// FFI bindings (private)
// ---------------------------------------------------------------------------

/// Returns {ok, nil} | {error, already_exists} | {error, invalid_process},
/// mapping directly to Ok(Nil) | Error(AlreadyExists) | Error(InvalidProcess).
@external(erlang, "registry_ffi", "register")
fn register_ffi(name: String, pid: process.Pid) -> Result(Nil, RegisterError)

@external(erlang, "conflict_ffi", "register_with_resolver")
fn register_with_resolver_ffi(
  name: String,
  pid: process.Pid,
  resolver: conflict.Resolver,
) -> Result(Nil, RegisterError)

@external(erlang, "registry_ffi", "unregister")
fn unregister_ffi(name: String) -> Result(Nil, UnregisterError)

/// Returns {ok, Pid} | {error, nil}.
@external(erlang, "registry_ffi", "whereis")
fn whereis_ffi(name: String) -> Result(process.Pid, Nil)

/// Monotonic clock for deadline arithmetic. Wall-clock skew (NTP
/// adjustment, manual clock change) must NOT be able to push a
/// running poll-loop past or before its budget; only monotonic time
/// gives the strictly non-decreasing reference.
@external(erlang, "distribute_ffi_utils", "monotonic_ms")
fn monotonic_ms() -> Int

@external(erlang, "distribute_ffi_utils", "subject_tag_matches_name")
fn subject_tag_matches_name(
  subject: process.Subject(BitArray),
  name: String,
) -> Bool

/// Charset and length validation for registry names. Charset is
/// `[a-zA-Z0-9._-]+`, byte size 1..255. Mirrors cluster cookie/node
/// validation so registry input cannot be wider than what the cluster
/// layer already accepts.
@external(erlang, "distribute_ffi_utils", "is_valid_registry_name")
fn is_valid_registry_name_ffi(name: String) -> Bool

// ---------------------------------------------------------------------------
// Core API
// ---------------------------------------------------------------------------

/// Register a PID under a global name.
///
/// Low-level escape hatch. Prefer `register_global/2` when you have a
/// `GlobalSubject`, or `register_typed/2` when you already hold the exact
/// distributed `Subject(BitArray)` that remote lookups will reconstruct.
///
/// A raw PID carries no protocol/tag invariant by itself: if you register the
/// owner of a subject whose mailbox tag does not equal `name`, remote lookups
/// will still reconstruct `unsafe_from_name(name, pid)` and can silently send into a
/// mailbox slot the actor never receives on.
///
/// ## Caution: split-brain conflict resolution is brutal
///
/// Backed by `:global`. After a network partition heals, conflict
/// resolution sends an uncatchable `exit(loser_pid, kill)` to one
/// side of any duplicate registration. See `named/2` and
/// `docs/safety_and_limits.md` for the full operational picture.
pub fn register(name: String, pid: process.Pid) -> Result(Nil, RegisterError) {
  let outcome = case validate_name(name) {
    Error(e) -> Error(e)
    Ok(_) -> register_ffi(name, pid)
  }
  emit_register_outcome(name, outcome)
  outcome
}

fn emit_register_outcome(
  name: String,
  outcome: Result(Nil, RegisterError),
) -> Nil {
  case outcome {
    Ok(Nil) -> telemetry.emit(telemetry.ActorRegistered(name))
    Error(e) ->
      telemetry.emit(telemetry.ActorRegistrationFailed(
        name,
        register_error_to_string(e),
      ))
  }
}

/// Register a name-tagged distributed `Subject(BitArray)` under a global name.
///
/// Low-level escape hatch: this is the raw-subject sibling of
/// `register_global/2`. The supplied subject must already be the exact
/// name-tagged distributed subject that remote lookups will reconstruct
/// (`global.unsafe_from_name(name, pid, ...)` or the subject returned by
/// `actor.start_*`).
///
/// Subjects built with `process.new_subject()` (random Ref tag) or
/// `global.from_pid()` (Nil tag) are rejected: a remote `lookup` would
/// reconstruct `unsafe_from_name(name, pid)` and silently send into a mailbox slot
/// that never matches the original selector.
pub fn register_typed(
  name: String,
  subject: process.Subject(BitArray),
) -> Result(Nil, RegisterError) {
  let outcome = case validate_name(name) {
    Error(e) -> Error(e)
    Ok(_) ->
      case subject_tag_matches_name(subject, name) {
        False ->
          Error(InvalidArgument(
            "Subject tag does not match registry name \""
            <> name
            <> "\". Build it with global.unsafe_from_name/4 or actor.start_*.",
          ))
        True ->
          case process.subject_owner(subject) {
            Ok(pid) -> register_ffi(name, pid)
            Error(Nil) -> Error(InvalidProcess)
          }
      }
  }
  emit_register_outcome(name, outcome)
  outcome
}

/// Register a `GlobalSubject` under a typed name.
///
/// The `msg` type parameter on `TypedName(msg)` and `GlobalSubject(msg)`
/// must match. The compiler enforces this.
///
/// **Runtime invariant**: the supplied `gs` must have been built with
/// `global.unsafe_from_name(name, ...)` (or via `actor.start_*`, which does the
/// same internally) so that its tag equals the registry name. A remote
/// `lookup` reconstructs the Subject as `unsafe_from_name(name, pid)` and any
/// other tag would silently accumulate messages in the actor's mailbox
/// without ever matching its selector.
///
/// We enforce the invariant up front. Registering a Subject built with
/// `global.new()` (random Ref tag) or `global.from_pid()` (Nil tag)
/// returns `Error(InvalidArgument(...))` instead of silently breaking.
///
/// ## Propagation is not atomic across the cluster
///
/// `Ok(Nil)` means `:global` accepted the registration **on this
/// node**. Cross-cluster visibility follows the standard `:global`
/// gossip path and is not instantaneous: a `lookup` from a remote
/// node a microsecond after this call returns can still see the
/// previous state (or no state) until propagation completes. This
/// is a property of `:global` itself, not a defect in
/// `distribute`. Callers who need to register-then-immediately-
/// lookup on another node have three options:
///
/// - Use `lookup_with_timeout` with a small budget (200-500 ms is
///   usually plenty); the polling loop bridges the propagation
///   window without busy-waiting.
/// - Send a synchronous `call` from the registering node back to
///   the actor before notifying remote callers; the round trip
///   forces propagation to complete.
/// - For test setups, call `:global.sync()` directly via FFI.
///   Not recommended for production hot paths because it blocks
///   on the global_name_server queue.
pub fn register_global(
  tn: TypedName(msg),
  gs: global.GlobalSubject(msg),
) -> Result(Nil, RegisterError) {
  case subject_tag_matches_name(global.subject(gs), tn.name) {
    False -> {
      let err =
        InvalidArgument(
          "GlobalSubject tag does not match TypedName \""
          <> tn.name
          <> "\". Build it with global.unsafe_from_name/4 or actor.start_*.",
        )
      emit_register_outcome(tn.name, Error(err))
      Error(err)
    }
    True ->
      case global.owner(gs) {
        // `register/2` already emits on its own outcome.
        Ok(pid) -> register(tn.name, pid)
        Error(Nil) -> {
          emit_register_outcome(tn.name, Error(InvalidProcess))
          Error(InvalidProcess)
        }
      }
  }
}

/// Like `register_global/2`, but installs a custom split-brain
/// conflict resolver alongside the registration.
///
/// `:global` invokes the resolver when the same name is claimed by
/// two different PIDs (typically after a network partition heals).
/// The default resolver behind `register_global/2`
/// (`:global.random_notify_name/3`) picks a winner at random and
/// kills the loser. For workloads where one side should always win,
/// a leader pinned to a specific node, a router whose state
/// must survive on a primary, a singleton whose oldest instance is
/// authoritative. Pass an explicit resolver from
/// `distribute/conflict`.
///
/// ```gleam
/// import distribute/conflict
/// import distribute/registry
///
/// let assert Ok(Nil) =
///   registry.register_global_with_resolver(
///     counter_name,
///     gs,
///     conflict.node_priority(["primary@host", "secondary@host"]),
///   )
/// ```
///
/// ## Operational guard rails
///
/// The resolver runs *inside* `:global`'s singleton worker
/// process; while it executes, every `:global` operation cluster-
/// wide is serialised behind it. To bound the worst-case stall,
/// the FFI shim spawns the resolver in a short-lived worker with
/// a hard deadline read from
/// `config.get().conflict_resolver_timeout_ms` (default
/// **1 000 ms**). On timeout or crash, the fallback
/// (lowest term-ordered PID wins) fires and
/// `telemetry.ConflictResolverFailed` is emitted so operators can
/// see that the user fn misbehaved. Every resolution emits
/// `telemetry.ConflictResolved` with the surviving PID (or `None`
/// for `KillBoth`).
///
/// ## Data-loss honesty box
///
/// The fallback is **deterministic** but not **state-aware**: if
/// your resolver was supposed to keep the side with the most
/// recent state and it fails (timeout, panic, malformed return),
/// the fallback picks lowest-PID and the cluster may lose
/// whatever the loser was holding. For stateful actors where this
/// would corrupt the system, use `conflict.kill_both()` as the
/// resolver and watch `telemetry.ConflictResolved(_, None)` to
/// trigger an application-level recovery path (re-elect, re-
/// bootstrap from durable storage). See `distribute/conflict` for
/// the full discussion.
///
/// ## Tag invariant
///
/// Same as `register_global/2`: the supplied `GlobalSubject` must
/// carry the `TypedName` as its tag. A mismatch returns
/// `Error(InvalidArgument(...))` before any `:global` call. Build
/// the subject via `actor.start_*` or via `lookup`.
pub fn register_global_with_resolver(
  tn: TypedName(msg),
  gs: global.GlobalSubject(msg),
  resolver: conflict.Resolver,
) -> Result(Nil, RegisterError) {
  case subject_tag_matches_name(global.subject(gs), tn.name) {
    False -> {
      let err =
        InvalidArgument(
          "GlobalSubject tag does not match TypedName \""
          <> tn.name
          <> "\". Build it with global.unsafe_from_name/4 or actor.start_*.",
        )
      emit_register_outcome(tn.name, Error(err))
      Error(err)
    }
    True ->
      case global.owner(gs) {
        Ok(pid) -> {
          let outcome = register_with_resolver_ffi(tn.name, pid, resolver)
          emit_register_outcome(tn.name, outcome)
          outcome
        }
        Error(Nil) -> {
          emit_register_outcome(tn.name, Error(InvalidProcess))
          Error(InvalidProcess)
        }
      }
  }
}

/// Unregister a global name.
///
/// See also: `register/2`, `register_global/2`, `lookup/1`.
///
/// Returns `Ok(Nil)` when *this* VM owned the name and the entry has
/// been removed from `:global`. Returns `Error(NotOwned)` when the
/// owning PID runs on another node (the local-ownership ACL refuses
/// the unregister), and `Error(NotFound)` when the name was not
/// registered at all. Idempotent cleanup paths can `let _ =
/// unregister(name)` to discard the outcome; observability paths can
/// pattern-match for diagnostics.
///
/// **Local-ownership ACL**: stateless `node(Pid) =:= node()` check via
/// `:global.whereis_name/1`. No local mirror can go stale, no
/// auto-cleanup is needed when processes die. This blocks the "registry
/// wipe" vector where unvalidated input flows into `unregister` and
/// tears down arbitrary cluster routing. Standard auto-cleanup
/// (`:global` removes a name when the owning process dies) is
/// unaffected.
pub fn unregister(name: String) -> Result(Nil, UnregisterError) {
  let outcome = unregister_ffi(name)
  emit_unregister_outcome(name, outcome)
  outcome
}

/// Unregister using a `TypedName` directly. Type-safe sibling of
/// `unregister/1` that reuses the protocol the caller already holds,
/// avoiding a stringly-typed extraction at the call site.
pub fn unregister_typed(tn: TypedName(msg)) -> Result(Nil, UnregisterError) {
  unregister(tn.name)
}

fn emit_unregister_outcome(
  name: String,
  outcome: Result(Nil, UnregisterError),
) -> Nil {
  let removed = case outcome {
    Ok(Nil) -> True
    Error(_) -> False
  }
  telemetry.emit(telemetry.ActorUnregistered(name, removed))
}

/// Look up a globally registered PID by name.
pub fn whereis(name: String) -> Result(process.Pid, Nil) {
  whereis_ffi(name)
}

/// Look up a `GlobalSubject` by `TypedName`.
///
/// Reconstructs the subject using the name-based tag, so messages
/// sent via the returned subject reach the actor mailbox.
///
/// ## Snapshot semantics
///
/// `lookup` returns the Subject that resolves to the PID found at
/// the exact moment the lookup ran. The PID may crash, deregister,
/// or leave the cluster a microsecond later. Subsequent
/// `global.send` calls on a stale Subject silently drop messages
/// because that is the BEAM's contract for `erlang:send/2` to a
/// dead PID. For long-lived references, either re-`lookup` before
/// each critical send, or use `global.call` (which monitors the
/// target and surfaces `Error(TargetDown)` immediately when the PID
/// has died).
///
/// ## Propagation is not atomic across the cluster
///
/// A `lookup` from node B issued microseconds after a successful
/// `register_global` on node A may still return `Error(Nil)`
/// because `:global` propagates registrations via gossip rather
/// than synchronous broadcast. This is the standard `:global`
/// behaviour, not a defect in `distribute`. For the
/// register-then-immediately-lookup pattern across nodes, prefer
/// `lookup_with_timeout` with a 200-500 ms budget over a
/// single-shot `lookup`: the polling loop bridges the propagation
/// window without busy-waiting.
pub fn lookup(tn: TypedName(msg)) -> Result(global.GlobalSubject(msg), Nil) {
  case whereis_ffi(tn.name) {
    Ok(pid) -> Ok(global.unsafe_from_name(tn.name, pid, tn.encoder, tn.decoder))
    Error(Nil) -> Error(Nil)
  }
}

/// Check whether a name is currently registered.
pub fn is_registered(name: String) -> Bool {
  case whereis_ffi(name) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Errors from polling lookups.
pub type LookupError {
  /// Name not registered within the timeout window.
  LookupNotFound
  /// `timeout_ms` was zero or negative.
  LookupInvalidTimeout(Int)
  /// `poll_interval_ms` was zero or negative.
  LookupInvalidPollInterval(Int)
}

pub fn lookup_error_to_string(err: LookupError) -> String {
  case err {
    LookupNotFound -> "Name not found within timeout"
    LookupInvalidTimeout(v) ->
      "Invalid timeout: " <> int.to_string(v) <> " (must be > 0)"
    LookupInvalidPollInterval(v) ->
      "Invalid poll interval: " <> int.to_string(v) <> " (must be > 0)"
  }
}

/// Poll until a `TypedName` is registered or `timeout_ms` elapses.
///
/// Retries every `poll_interval_ms` milliseconds. Both `timeout_ms` and
/// `poll_interval_ms` must be strictly positive. Passing zero or a
/// negative value returns a typed `Error` instead of crashing the
/// scheduler (a non-positive `poll_interval_ms` would tight-loop or
/// raise `badarg` inside `process.sleep`).
///
/// **Warning**: blocks the calling process. Do not call inside an OTP actor
/// handler. Use `lookup_async` instead.
pub fn lookup_with_timeout(
  tn: TypedName(msg),
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Result(global.GlobalSubject(msg), LookupError) {
  case timeout_ms <= 0 {
    True -> Error(LookupInvalidTimeout(timeout_ms))
    False ->
      case poll_interval_ms <= 0 {
        True -> Error(LookupInvalidPollInterval(poll_interval_ms))
        False ->
          do_lookup_with_timeout(
            tn,
            monotonic_ms(),
            timeout_ms,
            poll_interval_ms,
          )
      }
  }
}

/// Non-blocking poll: spawns a polling worker that probes the registry
/// until the name resolves, the timeout elapses, or the caller dies.
///
/// The worker **monitors the caller**. If the caller process dies for
/// any reason (crash *or* normal `exit(normal)`), the worker stops
/// polling and terminates immediately. A bare `link` would only catch
/// crashes: a short-lived web handler that completes with reason
/// `normal` would leave the worker busy-polling for the full timeout
/// while the result has nowhere to go.
///
/// Validation errors (invalid timeout or poll interval) are forwarded to
/// `reply_to` synchronously. The worker is not spawned for invalid params.
///
/// ```gleam
/// let reply = process.new_subject()
/// registry.lookup_async(tn, reply, 5000, 100)
/// let assert Ok(Ok(gs)) = process.receive(reply, 5100)
/// ```
pub fn lookup_async(
  tn: TypedName(msg),
  reply_to: process.Subject(Result(global.GlobalSubject(msg), LookupError)),
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Nil {
  case timeout_ms <= 0 {
    True -> process.send(reply_to, Error(LookupInvalidTimeout(timeout_ms)))
    False ->
      case poll_interval_ms <= 0 {
        True ->
          process.send(
            reply_to,
            Error(LookupInvalidPollInterval(poll_interval_ms)),
          )
        False -> {
          let caller = process.self()
          let _ =
            process.spawn_unlinked(fn() {
              do_async_lookup(
                tn,
                reply_to,
                caller,
                monotonic_ms(),
                timeout_ms,
                poll_interval_ms,
              )
            })
          Nil
        }
      }
  }
}

/// Worker body for `lookup_async`. Monitor-driven liveness on the caller
/// catches both crashes and normal exits, unlike a link.
fn do_async_lookup(
  tn: TypedName(msg),
  reply_to: process.Subject(Result(global.GlobalSubject(msg), LookupError)),
  caller: process.Pid,
  start: Int,
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Nil {
  let mon = process.monitor(caller)
  let caller_dead =
    process.new_selector()
    |> process.select_specific_monitor(mon, fn(_) { Nil })
  do_async_loop(tn, reply_to, caller_dead, start, timeout_ms, poll_interval_ms)
}

fn do_async_loop(
  tn: TypedName(msg),
  reply_to: process.Subject(Result(global.GlobalSubject(msg), LookupError)),
  caller_dead: process.Selector(Nil),
  start: Int,
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Nil {
  case lookup(tn) {
    Ok(gs) -> {
      process.send(reply_to, Ok(gs))
      Nil
    }
    Error(_) -> {
      let elapsed = monotonic_ms() - start
      case elapsed >= timeout_ms {
        True -> {
          process.send(reply_to, Error(LookupNotFound))
          Nil
        }
        False -> {
          // Clamp wait to remaining budget to honour `timeout_ms`
          // exactly: a `lookup_async(_, _, 5_000, 4_000)` must never
          // run for 8 s just because the poll interval was rounded
          // up. Wait *while listening* for the caller's DOWN so we
          // exit immediately if the caller has lost interest.
          let remaining = timeout_ms - elapsed
          let wait_ms = case poll_interval_ms < remaining {
            True -> poll_interval_ms
            False -> remaining
          }
          case process.selector_receive(caller_dead, wait_ms) {
            Ok(Nil) -> Nil
            Error(Nil) ->
              do_async_loop(
                tn,
                reply_to,
                caller_dead,
                start,
                timeout_ms,
                poll_interval_ms,
              )
          }
        }
      }
    }
  }
}

fn do_lookup_with_timeout(
  tn: TypedName(msg),
  start: Int,
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Result(global.GlobalSubject(msg), LookupError) {
  case lookup(tn) {
    Ok(gs) -> Ok(gs)
    Error(_) -> {
      let elapsed = monotonic_ms() - start
      case elapsed >= timeout_ms {
        True -> Error(LookupNotFound)
        False -> {
          // Clamp the wait to whatever budget is left so we never
          // overshoot the user-supplied `timeout_ms`. Without the
          // clamp, `lookup_with_timeout(name, 5_000, 4_000)` would
          // poll at T=0 (sleep 4000), poll at T=4000 (sleep 4000
          // *again*) and finally return at T=8000. 60% over budget.
          let remaining = timeout_ms - elapsed
          let wait_ms = case poll_interval_ms < remaining {
            True -> poll_interval_ms
            False -> remaining
          }
          process.sleep(wait_ms)
          do_lookup_with_timeout(tn, start, timeout_ms, poll_interval_ms)
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

fn validate_name(name: String) -> Result(Nil, RegisterError) {
  case is_valid_registry_name_ffi(name) {
    True -> Ok(Nil)
    False ->
      Error(InvalidArgument(
        "Name must be 1..255 bytes from charset [a-zA-Z0-9._-]",
      ))
  }
}
