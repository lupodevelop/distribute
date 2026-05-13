# Changelog

<!-- markdownlint-disable MD024 MD049 MD060 -->

## v4.0.0 (2026-05-13)

First production-grade release. v4 hardens the v3 model: the wire
format gets explicit size and trust boundaries, the actor lifecycle
gets explicit failure semantics, and the public surface gets opinion.
Existing v3 nodes cannot speak to v4 nodes. The codec length prefix
changed from 16 to 32 bits.

### Changed

- **`global.from_name` renamed to `global.unsafe_from_name`,
  dropped from facade.**
  Gleam has no friend-module visibility, so a function the registry
  / receiver / actor modules need to call across boundaries cannot
  be made truly private. The `unsafe_` prefix is the idiomatic
  signal: anything in user code that types
  `global.unsafe_from_name(...)` reads as a code smell on review,
  while the registry's internal use stays compile-clean. The
  function additionally keeps `@internal` to drop it from
  generated docs and IDE autocomplete, and the facade re-export
  `distribute.from_name` is gone. The contract behind the rename:
  reconstructing a `GlobalSubject` from a free-form
  `(name, pid, encoder, decoder)` tuple bypasses the `TypedName`
  discipline that the rest of the API leans on, and a forged
  subject with a mismatched codec would silently encode garbage
  into a peer's mailbox. The receiver still rejects via decode,
  but the caller's mental model "I have a `GlobalSubject(MyMsg)`,
  therefore the actor at the other end receives `MyMsg`" is the
  lie that needs the loud name. External callers should use
  `registry.lookup` (or the facade `distribute.lookup`).
- **FFI catches narrowed everywhere.**
  Both `distribute_ffi_utils:budget_aware_atom_create/1` and
  `distribute_ffi_utils:decode_subject_safe/1` previously used
  `catch _:_`. Narrowed to `catch error:badarg` (the documented
  exception class for `binary_to_existing_atom/2` and
  `binary_to_term/2` on malformed input). Other classes
  (`error:system_limit` from a saturated atom table, `exit:` from
  NIF-induced kills, real bugs in this code path) now propagate
  upward instead of being disguised as `atom_budget_exceeded` or
  `Error(InvalidBinary)`. Closes the "FFI swallows VM crashes"
  audit finding and keeps Gleam's "no silent runtime exceptions"
  promise honest at the boundary.

### Added
- **`distribute.install_telemetry/1` e re-export eventi sulla facade.** 
  Esposta l'installazione del sink della telemetria (`telemetry.install/1`) e i tipi `TelemetryEvent` e `TelemetrySink` direttamente in `distribute.gleam`. Questo rende la telemetria una "first-class citizen" e semplifica enormemente la DX per i pacchetti downstream (nessun import aggiuntivo necessario).
- **Test del layout del `Subject` (CI).** 
  Aggiunto un test dedicato (`subject_layout_test_ffi.erl` e `subject_layout_test.gleam`) per verificare a ogni build che il runtime Gleam mantenga il layout nativo `{subject, Pid, Tag}`. Se `gleam_erlang` rompe questo contratto in futuro, il test farà fallire la build immediatamente.


- **Custom split-brain conflict resolvers (Tier 1).**
  New module `distribute/conflict` plus
  `registry.register_global_with_resolver/3`. Lets callers swap the
  default `:global` coin-flip resolver
  (`:global.random_notify_name/3`) for a deterministic policy:
  `conflict.lowest_pid_wins()`, `conflict.highest_pid_wins()`,
  `conflict.keep_local()`, `conflict.kill_both()`,
  `conflict.node_priority([...])`. Custom resolvers are accepted
  via the `Resolver` type alias `fn(String, Pid, Pid) ->
  ConflictOutcome` where `ConflictOutcome` is `Keep(Pid)` or
  `KillBoth`.

  The FFI shim (`conflict_ffi:safe_invoke/5`) protects
  `:global`'s singleton worker from slow or panicking user
  resolvers: every invocation runs in a short-lived spawned
  worker with a hard deadline read from
  `config.get().conflict_resolver_timeout_ms` (default
  **1 000 ms**, lowered from the v4-alpha 5 000 ms after the
  cynical-audit observation that `:global` serialises every
  cluster-wide operation behind the resolver). On timeout or
  crash the shim falls back to lowest-PID-wins, emits
  `ConflictResolverFailed` telemetry, and returns control to
  `:global` so the cluster never wedges.

  `conflict.kill_both()` is the built-in resolver to use when
  state divergence is worse than no winner. The default
  fallback is deterministic but not state-aware: for stateful
  actors (counters, caches, leaders carrying quorum state),
  picking lowest-PID after a resolver failure can lose
  authoritative state. `kill_both()` plus a
  `ConflictResolved(_, None)` telemetry handler is the safer
  pattern for those workloads.
- **Telemetry events for conflict resolution.**
  `telemetry.ConflictResolved(name, winner: Option(Pid))` fires
  on every resolver invocation (including the deterministic
  fallback that runs after a user-side failure).
  `telemetry.ConflictResolverFailed(name, reason)` fires when the
  user fn crashed or timed out, immediately before the fallback
  emits its own `ConflictResolved`. Operators watch the failure
  event to detect "our resolver is broken" and the resolved-event
  rate to detect cluster flapping.
- **Telemetry sink panic isolation.** `emit/1` now catches sink
  exceptions and logs them via `logger:warning/2` (event tag plus
  stack trace, no payload to avoid leaking sensitive
  `attempted_input` strings). Library-internal processes (actor
  handlers, call selectors, the `call_isolated` proxy) survive a
  buggy sink instead of dying with it. Production sinks should
  still be written total; the catch is a safety net for shipped
  bugs, not a license to raise.
- **`actor.start_resource_owner(open, close, lifetime)`** ships the
  "linked resource owner" recipe as a first-class helper. Spawns an
  unlinked observer that opens an external resource, monitors the
  lifetime PID, and runs `close(resource)` once the lifetime PID
  dies for any reason. Workaround for the missing `{terminate, Reason}`
  callback in `gleam/otp/actor` 1.x (upstream tracking issue:
  gleam-lang/otp#126). Contract is shaped so callers can swap the
  call for the upstream API once it ships.
- **`telemetry.CallProxyCrashed`** distinguishes a `call_isolated`
  proxy crash from a real RPC timeout. The caller still observes
  `Error(Timeout)` for typed-error consistency, but observability
  now sees the qualitatively different cause (a panic inside
  `make_request`, the response codec, or another component evaluated
  by the proxy).

### Fixed

- **`cluster_ffi:start_node/2` no longer catches every exception class.**
  The outer catch was narrowed from a catch-all to
  `catch error:Reason:Stack`, so `throw`/`exit` classes are no longer
  silently re-labeled as `start_failed`. This keeps the Erlang boundary
  aligned with the project's "no broad catches" hardening rule.
- **Conflict-resolver timeout teardown is now bounded and monitor-safe.**
  In `conflict_ffi:safe_invoke/5`, the timeout path now handles
  already-dead workers without waiting forever on monitor delivery:
  `await_worker_down/2` checks liveness, performs a short bounded wait
  for `DOWN`, and flushes the monitor fallback path deterministically.
- **`cluster.health()` ping-worker shutdown is now graceful-first.**
  `terminate_ping_workers/1` now sends `exit(Pid, shutdown)` first and
  escalates to `process.kill` only if the worker is still alive. This
  matches the library's two-phase termination style used elsewhere.
- **Removed no-op monitor cleanup in orphan termination fast path.**
  `actor.terminate_orphan_gracefully/1` no longer calls
  `demonitor_process` after an already-consumed `DOWN` in the success
  branch; behavior is unchanged, intent is clearer.
- **`cluster.health()` now returns deterministic partitions.**
  `reachable_nodes` and `unreachable_nodes` are canonicalised to the
  order of `connected_nodes`, so worker-reply timing and map-key
  iteration cannot leak into API ordering.
- **`CallTimedOut` telemetry now uses the effective timeout.**
  `global.call` and `global.call_isolated` emit the clamped
  non-negative timeout used by the receive path (e.g. negative input
  now emits `0`, never a negative duration).
- **Proxy-crash branch of `call_isolated` now emits dedicated
  telemetry.** Previously the branch returned `Error(Timeout)`
  *without* emitting anything, breaking the contract that every
  `Error(Timeout)` produces a structured event. The new
  `CallProxyCrashed` event closes the gap and lets observers tell
  user-side bugs from real RPC timeouts.
- **`AtomBudgetExhausted` telemetry on `cluster.start_node` now
  attributes the offending input correctly.** When the cookie atom
  was the one that ran the budget out, the previous code emitted the
  *node name* as `attempted_input`. The FFI now emits at the boundary
  with the actual offending binary, name or cookie.
- **Real-cluster Z2/Z3 tests are now mandatory everywhere (local + CI).**
  Distribution setup failures no longer short-circuit to a passing
  "skip" branch; they fail the test immediately. This removes the last
  false-green path where missing `epmd` or peer startup issues could be
  overlooked outside CI.

### Documentation

- **Audit follow-up docs synced with runtime behaviour.**
  Safety docs now spell out the bounded timeout teardown in the
  conflict-resolver FFI shim and the graceful-first shutdown policy for
  `cluster.health` ping workers. The actors/registry guide now calls out
  the known `:global` pool cascade risk with operational guidance.
- **Migration guide (`docs/migration_3_to_4.md`)** corrected on the
  `configure` step: it is optional, not required. The library reads
  built-in defaults when no configuration has been loaded.
- **Indicative release roadmap.** New [`ROADMAP.md`](./ROADMAP.md)
  at the project root with the planned sequencing of post-v4.0
  releases (v4.0.1 patch, v4.1 backend abstraction, v4.2 second
  backend opt-in). Marked explicitly as tentative, refined as
  user feedback comes in. Brief cross-link in
  `docs/safety_and_limits.md`.
- **Conflict resolver data-loss honesty box.**
  `distribute/conflict` and
  `registry.register_global_with_resolver/3` docstrings now
  spell out the "lowest-PID fallback is deterministic but not
  state-aware" trade-off and direct stateful-actor users at
  `kill_both()` plus a `ConflictResolved(_, None)` telemetry
  handler.
- **`:global` propagation race documented on the API surface.**
  `register_global/2` and `lookup/1` docstrings now call out
  that `:global` propagates registrations via gossip and a
  cross-node `lookup` issued microseconds after a successful
  `register_global` may still see the prior state. Recommends
  `lookup_with_timeout` with a 200-500 ms budget for the
  register-then-immediately-lookup pattern. No code change; this
  is a standard `:global` behaviour the library cannot abstract
  away, so it sits on the call site rather than buried in a side
  document.
- **`cluster.start_node` blocking risk documented.** The function
  delegates to `net_kernel:start/1`, which talks to `epmd` and the
  OS resolver. The library cannot interpose a Gleam-side timeout;
  the docstring now spells out the failure modes (epmd impaled,
  DNS down, misconfigured `/etc/hosts`) and the mitigations callers
  can apply. Mirrored in `docs/safety_and_limits.md` under a new
  "BEAM and OS-level risks" subsection.
- **`global.send` fire-and-forget semantics spelled out.** A
  successful `send` means the BEAM accepted the message for local
  dispatch, not that the target received it. The docstring now
  points at `call` for delivery confirmation and explains why the
  type system cannot lie about this.
- **`registry.lookup` snapshot semantics documented.** The returned
  Subject resolves to the PID found at the exact moment of lookup;
  the PID may die a microsecond later. Docstring directs callers
  at `call` (monitor-backed) when delivery matters.
- **Sub-binary memory retention disclosed.** The codec has called
  `binary:copy/1` on decoded `String` and `BitArray` values since
  v4 to break the BEAM's parent-binary share, but the trade-off
  (small allocation per decode in exchange for predictable memory
  in long-lived actor state) was not previously surfaced. Now
  documented in `docs/safety_and_limits.md`.
- **`:global` split-brain conflict resolution documented.** After a
  network partition heals, `:global` may discover the same name
  registered to different PIDs on each side. Its default resolver
  picks a winner and sends an uncatchable `kill` to the loser.
  The library cannot trap this; the doc now spells out the
  operational impact and points at `cluster_monitor` as the signal
  callers can subscribe to for re-validation. Mirrored as
  `## Caution` blocks on the docstrings of `registry.named/2` and
  `registry.register/2` so the warning sits on top of the API
  surface, not buried in a side document.
- **Backpressure and head-of-line blocking caveats documented.**
  `global.send` is asynchronous with no high-water mark; decoding
  runs in the receiver's reduction budget. Both can be exploited
  by a hostile or buggy sender. The doc now lists the application-
  level mitigations (synchronous `call`, mailbox watermarks, sized
  worker pools) for callers who run on the hot path.

### Performance

- **`codec.list_encoder` no longer appends binaries in a loop.**
  Element binaries are accumulated then concatenated once, removing the
  quadratic growth pattern on large lists.

### Tests

- Regression tests for deterministic cluster-health partition
  ordering and for clamped timeout telemetry in both `call` and
  `call_isolated`.
- New tests covering the `CallProxyCrashed` event, the cookie-input
  attribution on `start_node` atom budget telemetry, and the
  `start_resource_owner` lifetime contract (close runs on lifetime
  death, including when the lifetime PID was already dead at the
  time the helper was called).
- **`distribute.version()` vs `gleam.toml` regression test.** Reads
  the manifest at test time and asserts the hardcoded
  `distribute.version()` matches. Catches a release that bumps one
  side without the other.
- **Sink panic isolation test.** Installs a sink that raises,
  asserts subsequent `emit` calls return cleanly and a healthy
  sink installed afterwards still receives events.

### Architectural guarantees

- **OOM protection.** Every binary-to-typed path enforces
  `config.max_payload_size_bytes` *before* decoding or network I/O.
- **No zombie actors.** Registration failures surface as typed errors
  and the orphaned actor is killed (uncatchable, even when the actor
  traps exits).
- **Fast-fail calls.** `call` monitors the target; a dead peer returns
  `Error(TargetDown)` immediately instead of blocking until the
  timeout expires.
- **Mailbox safety.** `call_isolated` runs the inner request inside a
  short-lived unlinked proxy process so orphan late-replies die with
  the proxy and cannot pollute long-running callers.
- **Atom-table guardrail.** Cluster `connect`/`ping` count fresh-atom
  creation against `config.max_distribution_atoms` via a lock-free
  `atomics:add_get`. The VM atom table cannot be exhausted from this
  path.
- **Local-ownership ACL.** `unregister` refuses names owned by remote
  PIDs (`node(Pid) =:= node()`), blocking the "registry wipe" attack
  where unvalidated input flows into `unregister` to tear down
  cluster routing.
- **Centralised config.** No magic numbers anywhere. A single
  `configure(...)` at boot tunes the whole library; reads are O(1)
  via `persistent_term`.

### Breaking changes

#### v3 → v4 migration

| v3                                                | v4                                                                                                              |
| ------------------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| `distribute.start(name, cookie)`                  | `distribute.start_node(name, cookie)`                                                                           |
| `global.send -> Result(_, codec.EncodeError)`     | `global.send -> Result(_, global.SendError)`                                                                    |
| `CallError.CallTimeout`                           | `CallError.Timeout`                                                                                             |
| `receiver.Next(state)`                            | `receiver.HandlerStep(state)`                                                                                   |
| `receiver.ReceiveError.Timeout`                   | `receiver.ReceiveError.ReceiveTimeout`                                                                          |
| `cluster.ConnectError.NodeNotFound`               | (removed): folded into `ConnectFailed`                                                                         |
| `cluster.StartError.StartFailed("cookie...")`     | `cluster.StartError.InvalidCookieFormat(_)`                                                                     |
| legacy `registry.unregister` semantics            | `registry.unregister -> Result(Nil, UnregisterError)` with `NotFound` / `NotOwned` surfaced explicitly             |
| `registry.lookup_with_timeout -> Result(_, Nil)`  | `Result(_, LookupError)` with `LookupNotFound` / `LookupInvalidTimeout` / `LookupInvalidPollInterval`              |
| `actor.start_registered` errors                   | New `StartRegisteredError` (`ActorStartFailed` / `RegistrationFailed`)                                          |
| 16-bit length prefix (`String`, `BitArray`)       | 32-bit length prefix. Wire incompatible with v3                                                                |
| `int_encoder` truncates out-of-range values       | Returns `Error(ValueTooLarge)`                                                                                  |
| `tagged.encoder` wraps version on overflow        | Returns `Error(ValueTooLarge)`                                                                                  |
| `pool(size: 0)` silently produces empty supervisor | Returns `Error(actor.InitFailed("pool size must be >= 1"))`                                                    |
| `lookup_with_timeout(0, 0)` tight-loops / `badarg` | Returns `Error(LookupInvalidTimeout)` / `LookupInvalidPollInterval`                                            |
| Actor handler bypassed `max_payload_size_bytes`   | Size check enforced at every `binary -> typed` path                                                             |
| `actor.start_registered` orphan via `send_exit`   | `unlink` + `process.kill` (uncatchable even when the actor traps exits)                                        |

#### Facade (`distribute`) signature changes

The facade is now an opinionated 90% surface: the primary name is the
default-driven form, `_with_timeout` siblings are the explicit overrides.

| New                                  | Old                                  |
| ------------------------------------ | ------------------------------------ |
| `call/3`                             | `call_default/3`                     |
| `call_with_timeout/4`                | old `call/4`                         |
| `call_isolated/3`                    | (new default form)                   |
| `call_isolated_with_timeout/4`       | old `call_isolated/4`                |
| `receive/1`                          | `receive_default/1`                  |
| `receive_with_timeout/2`             | old `receive/2`                      |
| `start_actor/3`                      | `start_actor_default/3`              |
| `start_actor_with_timeout/4`         | old `start_actor/4`                  |
| `start_actor_observed/4`             | old `start_actor_observed/5`         |
| `start_registered/3`                 | `start_registered_default/3`         |
| `start_registered_with_timeout/4`    | old `start_registered/4`             |
| `start_supervised/3`                 | (new default form)                   |
| `start_supervised_with_timeout/4`    | old `start_supervised/4`             |
| `pool/4`                             | (new default form)                   |
| `pool_with_timeout/5`                | old `pool/5`                         |
| `child_spec/3`                       | (new default form)                   |
| `child_spec_with_timeout/4`          | old `child_spec/4`                   |
| `new_subject(c: Codec(msg))`         | `new_subject(encoder, decoder)`      |
| `has_peers()`                        | `is_healthy()` (renamed; misnomer for single-node) |

#### Removed from the facade (still in their modules)

`register_pid`, `register_typed`, `whereis`, `is_registered`,
`lookup_with_timeout`, `lookup_async` (use `distribute/registry`);
`from_pid`, `from_subject` (use `distribute/global`);
`receive_typed`, `selecting_typed` (use `distribute/receiver`);
`start_registered_observed` (use `distribute/actor`);
`connected_count`, `ping` (use `distribute/cluster` or `health()`);
`typed_name_codec` (functionally identical to `named`; duplicate killed).

### Security & hardening

- **Payload size enforcement** at every I/O boundary
  (`global.send`/`call`/`receive`/`reply`, `receiver.receive_typed`):
  oversized binaries return `PayloadTooLarge(size)` *before* decoding,
  so a malicious peer cannot drive the receiver out of memory.
- **32-bit length prefix** on `String` and `BitArray`. Lists use a
  32-bit element count and the encoder enforces the same
  `config.max_decoded_list_elements` cap (default 10,000 elements)
  as the decoder (`ListTooLong` guardrail) to keep encode/decode symmetry.
- **Strict trailing-byte rejection** in every codec: a binary with
  trailing bytes after the value is `Error(InvalidBinary)`, blocking
  the smuggle vector.
- **Atom-creation budget** for distribution: `cluster.connect` and
  `ping` reject fresh atoms past `config.max_distribution_atoms` with
  a typed error; the BEAM atom table cannot be exhausted through
  this path.
- **`binary_to_existing_atom`-first** lookup before any
  `binary_to_atom`, so a name we have already seen costs nothing
  against the budget.
- **Local-ownership ACL** on `unregister`: the operation is refused
  for names whose owning PID lives on another node. Standard
  `:global` auto-cleanup (PID monitor) is unaffected.
- **Cookie + node-name format validation byte-wise in FFI**
  (`is_valid_cookie/1`, `is_valid_node_name/1`,
  `is_valid_registry_name/1`): charset `[a-zA-Z0-9._-]+`,
  1..255 bytes. The Gleam-side codepoint check that disagreed with
  the FFI byte check is gone.
- **`call` is monitor-driven**: a dead target returns
  `Error(TargetDown)` immediately, never blocks.
- **`call_isolated` runs in an unlinked proxy + monitor**: orphan
  late-replies die with the proxy and a proxy crash surfaces as
  `Error(Timeout)` to the caller, never as crash propagation.
- **Caller-side timeout drains the result Subject** before returning,
  so a late reply cannot leak back through the canonical reply path.
- **Health-check uses absolute monotonic deadline**:
  `cluster.health()` fans out parallel pings, collects with a single
  8 s deadline, and preserves the invariant
  `reachable ∪ unreachable = connected` even on partial timeouts.
- **`start_registered` orphan cleanup is uncatchable**:
  `process.unlink` + `process.kill`; the actor cannot trap-exit
  the cleanup signal.
- **`is_distributed` uses `erlang:is_alive/0`**, the authoritative
  signal. Previously compared the node-name string against
  `"nonode@nohost"` and would lie if the runtime ever changed that
  placeholder.

### Added

- **`distribute/config`**; new module for global runtime
  configuration backed by `persistent_term`. Single-write
  (`AlreadyConfigured` on a second call) so the GC cost of repeated
  `persistent_term` writes is paid at most once.
- **`distribute/cluster_monitor`**; typed wrapper over
  `net_kernel:monitor_nodes/1`. Subscribers receive `NodeUp(name)` /
  `NodeDown(name)` events. The `Message` type is opaque and the
  monitor proactively prunes subscribers via `process.monitor` on
  the owner PID, so the subscriber list is bounded under churn
  regardless of cluster activity.
- **Facade error surface, no extra imports.** Every public error
  type and `*_to_string` formatter is re-exported on `distribute`:
  `NodeStartError`, `ConnectError`, `RegisterError`,
  `UnregisterError`, `LookupError`, `CallError`, `SendError`,
  `ReceiveError`, `EncodeError`, `DecodeError`, `ConfigError`,
  `ActorStartError`, `StartRegisteredError`, plus value types
  `Config`, `ClusterHealth`, `ClusterEvent`, `MonitorMessage`,
  `TypedName`, `GlobalSubject`, `HandlerStep`. `import distribute`
  is sufficient for pattern-matching and rendering errors.
- **`distribute.unregister_typed/1`**; type-safe sibling of
  `unregister/1` that pulls the name from the `TypedName` the caller
  already holds, removing the "hardcode the string at the cleanup
  site" hazard.
- **`call_isolated`**; mailbox-safe variant of `call`. Recommended
  for long-running callers issuing many RPCs under sustained
  timeouts. Both default-form and `_with_timeout` form available.
- **`global.SendError`** type with `SendEncodeFailed(EncodeError)`
  and `PayloadTooLarge(Int)` variants; `send_error_to_string/1`.
- **`global.CallError.TargetDown`** plus `CallPayloadTooLarge(Int)`.
- **`registry.lookup_async`**; non-blocking polling lookup safe to
  call from inside OTP actor handlers (spawns an unlinked worker
  process that monitors the caller).
- **Observed start variants**; `actor.start_observed`,
  `actor.start_registered_observed`, `receiver.start_receiver_observed`,
  `receiver.start_distributed_worker_observed` accept an
  `on_decode_error: fn(DecodeError) -> Nil` hook for observability.
- **`cluster_monitor.start_observed`**; diagnostic hook for
  unrecognised mailbox terms; silent drops in cluster discovery are
  a debugging nightmare, this surfaces them.
- **`codec/composite.result/2`**; bundled `Codec(Result(a, e))` to
  match the existing `option`, `tuple2`, `tuple3` combinators.
- **Default-driven facade.** `configure(...)` once at boot, then
  every long-running operation (`call`, `receive`, `start_actor`,
  `start_registered`, `start_supervised`, `pool`, `child_spec`,
  `call_isolated`) reads its timeout from `config.get()`. The
  `_with_timeout` siblings are the override path.

### Fixed

- **`actor.start_registered` no longer swallows the actor start
  error.** Both `ActorStartFailed` and `RegistrationFailed` are
  surfaced via `StartRegisteredError`.
- **`cluster.connect` returns `ConnectFailed`** on refused connection
  instead of an opaque error.
- **`pool(size: 0)`** returns `Error(actor.InitFailed(...))` instead
  of silently producing an empty supervisor with degenerate worker
  names like `name_0` / `name_-1`.
- **`lookup_with_timeout` / `lookup_async` reject non-positive
  parameters.** Previously a 0 or negative `poll_interval_ms`
  tight-looped or raised `badarg` inside `process.sleep`.
- **`lookup_with_timeout` clamps the wait to the remaining budget**
  on every iteration so the user-supplied `timeout_ms` is honoured
  exactly; previously a poll interval larger than the deadline
  could overrun by up to one full interval.
- **Health check preserves the partition invariant** on partial
  timeout. Stalled pings are filed as `unreachable` so
  `reachable ∪ unreachable = connected` holds; the previous design
  silently dropped pending nodes from the result.
- **Atom-budget initialisation is race-safe.** ETS-based bootstrap
  with `read_concurrency` replaces the `persistent_term` lazy-init
  pattern that caused a GC storm under contention.
- **Late-reply mailbox leak via `call`** is documented and
  `call_isolated` provides the OTP-pure mitigation. Slow workers in
  `cluster.health()` whose `net_adm:ping` returns past the deadline
  send to a now-dead proxy PID, which the BEAM drops at zero cost.
- **`registry.register_typed` enforces the tag invariant.** Previously
  `register_typed(name, subject)` only checked the owner PID; a
  caller could register a `Subject(BitArray)` with a random Ref or
  `Nil` tag, and a remote `lookup` reconstructing
  `from_name(name, pid)` would silently send into a mailbox slot the
  actor never received on. Now the FFI `subject_tag_matches_name/2`
  check is enforced up-front; mismatched tags surface as
  `InvalidArgument` exactly like `register_global`. Closes a silent
  cross-node mailbox-pollution vector.
- **`call_isolated` late-reply test now probes the caller mailbox
  directly.** The previous canary check was tag-orthogonal and would
  pass even if orphans leaked. New test runs the call inside a
  dedicated child and probes its mailbox via `select_other` so any
  leftover raw term is matched immediately.
- **Doc examples align with the slim facade.** Six broken
  copy-pasteable examples fixed: `start_supervised(.., 5000)` and
  `pool(.., 5000)` now use the explicit `_with_timeout` siblings;
  `receive(gs, ms)` becomes `receive_with_timeout`; the three
  `distribute.start_registered_observed` references rewrite to
  `import distribute/actor as dist_actor` (the registered observed
  variant is intentionally not on the facade).

### Changed

- **Wire format**: 32-bit length prefix on `String` / `BitArray`. v3
  nodes cannot communicate with v4 nodes.
- **`registry.unregister`** returns `Result(Nil, UnregisterError)`
  with `NotFound` / `NotOwned` variants. The previous `Nil` return
  silently swallowed two distinct failure modes operators want
  surfaced for debugging. Idempotent cleanup paths can
  `let _ = unregister(name)`.
- **`is_healthy` renamed to `has_peers`.** "Health" implies
  infrastructure failure; "no peers" is operationally fine on a
  single-node deployment. K8s probes and operator dashboards now
  get an accurate signal.
- **`receiver.Next` renamed to `HandlerStep`** so it no longer
  collides with `gleam/otp/actor.Next` when both modules are
  imported unqualified.
- **`receiver.ReceiveError.Timeout` renamed to `ReceiveTimeout`** to
  disambiguate from `global.CallError.Timeout`.
- **`distribute.configure`** returns `Result(Nil, ConfigError)`,
  rejects zero/negative limits, and enforces single-write with
  `AlreadyConfigured`.
- **`global.call_isolated` proxy is unlinked + monitored.** The
  earlier linked design propagated proxy crashes back to the caller;
  the unlinked + monitor selector composes the result Subject with a
  DOWN match so a proxy crash surfaces cleanly as `Error(Timeout)`.
- **`registry.validate_name`** enforces charset `[a-zA-Z0-9._-]+`,
  1..255 bytes, byte-wise in FFI. Mirrors cluster cookie / node
  validation so registry input cannot widen the attack surface.
- **`clamp_timeout`** lives in `distribute_ffi_utils`. Both `global`
  and `receiver` bind via FFI so the clamp policy cannot drift.
- **`from_pid` opens with the send-only warning.** First line of the
  docstring is now the one a hover preview shows: send-only, never
  `receive`/`call`, use `from_name` for bidirectional.
- **Actor shutdown caveat documented.** README and safety docs now
  explicitly call out that `gleam_otp/actor` 1.x lacks an OTP-style
  `terminate` callback; external resources require the linked
  resource-owner pattern.
- **`config.reset` carries a production-danger doc.** Stays
  `@internal` (excluded from generated docs / autocomplete) but the
  IDE-hover contract now spells out test-only intent.
- **`StartRegisteredError.RegistrationFailed` renamed to
  `GlobalRegisterFailed`.** Avoids the unqualified-import collision
  with `RegisterError.RegistrationFailed` (a generic registry
  failure). Imported qualified the call sites still read clearly:
  `actor.GlobalRegisterFailed(...)` vs
  `registry.RegistrationFailed(...)`.
- **`cluster.connect` and `cluster.start_node` no longer duplicate
  Gleam-side `string.contains("@")` validation.** Format/charset
  checking was already enforced byte-wise by the FFI
  (`is_valid_node_name/1`, `is_valid_cookie/1`) which gives stronger
  guarantees on multi-byte input. The Gleam-side codepoint check is
  removed.
- **`registry_ffi:is_owned_here/1` removed.** Dead export, the inline
  ACL in `unregister/1` is the single source of truth.

### Doc/code coherence + residual-risk disclosure

External audit flagged one doc/code inconsistency and one
documentation gap; no runtime defect.

- **`distribute/telemetry` module doc corrected on `ping`**. Earlier
  text stated "`ping` does **not** emit today (its FFI return is
  `Bool`, no error path)". After the telemetry hardening pass the FFI was wired to emit
  `AtomBudgetExhausted(_, AtomBudgetOnPing)` directly, but the doc
  was not updated; a misleading contract for downstream consumers.
  The doc now reflects reality: all three of
  `connect` / `start_node` / `ping` emit on budget refusal, with the
  `ping` emit wired at FFI level because the public Gleam return is
  still `Bool`.
- **Accepted residual risk documented in
  `global.call_isolated` and `cluster.parallel_partition_reachable`
  teardown.** A NIF-bound proxy that yields after the demonitor
  fallback, mid-`process.send`, can leave at most one orphan
  binary in the caller's mailbox. Microsecond-wide window, scheduler-
  dependent, requires misbehaving user code in `make_request` or a
  custom codec. The fundamental fix waits on `erlang:alias/0`-aware
  Subjects in upstream `gleam_erlang`; the residual risk is now
  spelled out in the source comment and pointed at
  `docs/safety_and_limits.md` so operators see it.

### Cluster health teardown + facade call caveat

External audit flagged a teardown asymmetry in `cluster.health` and
recommended a stronger ergonomic warning on the facade `call/3`:

- **`cluster.parallel_partition_reachable` timeout branch now matches
  the `call_isolated` teardown shape**: kill -> wait DOWN -> fallback
  `demonitor_process` if the wait expires -> drain. Previously the
  wait was `let _ = selector_receive(...)` which discarded the result;
  on the rare path where the proxy's DOWN does not arrive within the
  grace window (NIF-bound proxy, scheduler stall), the monitor stayed
  active and a late DOWN could land in the caller's mailbox after
  `health()` returned. The new explicit fallback closes that
  symmetric edge with `call_isolated`. The grace window is named
  `health_proxy_shutdown_grace_ms` (1000 ms).
- **Facade `call/3` docstring now spells out the late-reply caveat
  prominently** and points long-running callers at `call_isolated/3`.
  No API change; the contract was always there in the core
  `global.call/4` docstring, but the facade re-export was thin and
  did not surface the trade-off where a casual reader would see it.

### Telemetry sink coverage gaps closed

After the initial telemetry rollout, an external audit identified
four emission gaps where the public contract held but the sink saw
nothing. All four are fixed and pinned by tests.

- **`global.call_isolated` caller-side timeout now emits
  `CallTimedOut`.** The previous code emitted only when the inner
  `call/4` timed out cleanly; if the proxy was stuck past
  `timeout_ms + 100`ms (heavy contention, scheduler stall) we killed
  the proxy and returned `Error(Timeout)` *without* emit. The contract
  "every `Error(Timeout)` returned by `call_isolated` produces at
  least one `CallTimedOut` to observers" now holds; the rare
  double-emit when both inner and outer fire is documented.
- **`registry.register_global` early-return paths emit
  `ActorRegistrationFailed`.** Tag mismatch and dead-subject_owner
  paths returned typed errors but never told the sink. Audit/security
  consumers depending on the event for compliance now see them.
- **`registry.register_typed` all paths emit.** The success path
  bypassed the emitting `register/2` wrapper by calling `register_ffi`
  directly; the validation-fail and tag-mismatch paths returned
  early. Both fixed by wrapping the entire body in
  `emit_register_outcome`.
- **`cluster.ping` atom-budget exhaustion emits
  `AtomBudgetExhausted(_, AtomBudgetOnPing)`.** The function still
  returns `Bool` (no public-API change) but the FFI now emits before
  returning `false` on the budget-refused path. Closes the documented
  caveat from the initial telemetry rollout.

### Telemetry event sink

New module `distribute/telemetry` exposes a single opt-in observability
hook, designed as the integration point for downstream packages
(`distribute_telemetry`, `distribute_audit`, `distribute_metrics`,
`distribute_otel`).

- **`install(sink: fn(Event) -> Nil) -> Nil`**, last-wins. Default = no
  sink installed; every emit point is a single ETS lookup + branch
  (microseconds, lock-free, zero allocation on the no-sink path).
- **`Event` sum type** carrying registry lifecycle (`ActorRegistered`,
  `ActorRegistrationFailed`, `ActorUnregistered`), atom-budget
  exhaustion (`AtomBudgetExhausted` with origin enum), boundary
  rejections (`PayloadRejected` with 6 origins, `DecodeFailed` with 3
  origins), call lifecycle (`CallTimedOut`, `CallTargetDown`), and
  actor hardening (`OrphanKillEscalated`).
- **Stringified failure reasons** in `ActorRegistrationFailed` and
  `DecodeFailed` keep the module a leaf in the import graph; no
  cycle through `registry.RegisterError` or `codec.DecodeError`. The
  trade-off (loss of structured pattern-match) is documented in the
  module head: downstream sinks log/meter strings anyway.
- **Storage in ETS, not `persistent_term`**: an `install` call does
  not trigger a global GC pass, so tests can swap sinks freely between
  cases via `@internal reset/0`.
- **Sink runs inline in the emit point's process**. Slow sinks slow the
  emit point; downstream packages that need async fan-out implement it
  in the sink itself (e.g. `process.send` to a background worker).

The library's own emit points cover registry register/unregister
results, all four `PayloadRejected` paths in `global` plus the
`receiver.decode_checked` boundary, both `CallTimedOut` and the two
`CallTargetDown` paths, atom-budget refusals in `cluster.start_node`
and `cluster.connect`, and orphan kill escalation in
`actor.terminate_orphan_gracefully`, plus atom-budget refusal in
`cluster.ping` (FFI-level emit on the `Bool` API path).

### Adversarial test coverage (Z-suite)

Five new tests close the prior "grey" coverage gaps:

- **Z1 atom-budget concurrent contention** (`cluster_test`). 200
  concurrent `cluster.connect` calls with unique fresh names against
  a 50-atom budget. Asserts exactly 50 calls pass the guardrail and
  150 surface `ConnectAtomBudgetExceeded`. Proves `atomics:add_get/3`
  is correct under multi-scheduler contention; the counter cannot
  overshoot regardless of scheduler interleaving.
- **Z2 `:global.sync()` race window** (`multinode_real_test`).
  Spins a real BEAM peer via OTP `peer`, registers a name on the
  peer's `:global`, then asserts
  `registry.lookup_with_timeout(name, 5000, 100)` resolves on the
  origin within budget. Skips gracefully (`io.println` sentinel) when
  distribution cannot be started (e.g. no `epmd` on a CI runner).
- **Z3 cross-node `register_global` race** (`multinode_real_test`).
  Two peers, fully connected via `net_kernel:connect_node`, both race
  the same global name. After explicit `:global.sync()` on both,
  asserts both peers' `whereis_name` resolve to the *same* Pid;
  i.e. `:global`'s conflict-resolve callback elects exactly one
  winner across the cluster.
- **Z4 v3 → v4 wire-format incompatibility** (`codec_test`). Four
  synthetic v3-style binaries (16-bit prefix on `String`, `BitArray`,
  `List`, `Subject`) fed to v4 decoders. All four must surface a
  typed `Error`; never a silent partial decode. Pins the
  CHANGELOG-claim "v3 nodes cannot communicate with v4 nodes" to a
  test harness instead of folklore.
- **Z5 garbage-flood resilience** (`receiver_test`). A distributed
  worker is bombarded with 1 000 raw foreign terms via
  `erlang:send`; a legit binary message must still be processed
  within 1 s. Proves the `select_other → Garbage` drain keeps the
  selective-receive cost O(1) regardless of garbage queue depth.

### Test coverage disclosure

The 260-test suite verifies the wire-level behaviour of every
distributed claim, but it does so on a **single VM via simulated
peer sends**: raw `term_to_binary` injection into actor mailboxes,
not real BEAM distribution. Real peer-node coverage (epmd-backed,
two BEAM instances) lives in `dev/peer_support.gleam` +
`dev/peer_ffi.erl` and is exercised manually with `gleam dev`; it is
intentionally not part of `gleam test` to keep CI hermetic.

What this means in practice:

- **Strongly covered**: codec strictness, payload limits, mailbox
  drain, atom-budget concurrency, registry ACL, `call`/`TargetDown`,
  `call_isolated` mailbox safety, monitor pruning, health partition
  invariant, orphan kill on registration failure.
- **Manually verified, not automated**: `:global.sync()` race window,
  cross-node `lookup` timing, atom budget behaviour under sustained
  multi-scheduler contention, supervisor restart cycle on
  cross-node registration races.

If your deployment relies on the second class, run `gleam dev` in
your CI and add custom integration tests against your own peer
topology.

## v3.0.0 (2026-02-11)

Ground-up rewrite. Smaller API, proper OTP actors, compile-time type safety
via `TypedName(msg)`. Not compatible with v2.

### Removed

Everything outside the core: crypto, discovery, settings, groups, monitoring,
connection pool, retry. Also removed `whereis_global(name, encoder, decoder)`.

### Changed

- Actors are real `gen_statem` via `gleam/otp/actor` `observer`,
  `sys:get_status`, supervision trees all work.
- Registry uses binary names with `:global`. No atoms created, no atom
  table exhaustion.
- `start`, `start_registered`, `start_supervised`, `pool` all take
  `TypedName` instead of separate name + encoder + decoder.
- `register` and `lookup` take `TypedName`. The compiler enforces that
  both sides use the same `msg` type.
- Each `GlobalSubject` gets a unique or deterministic tag (was shared
  `Nil` in v2, caused message mixing).
- Orphaned actors are killed on registration failure in `start_supervised`.

### Added

- `TypedName(msg)` opaque type binding a name to an encoder/decoder pair.
- `Codec(a)` bundles encoder + decoder + sized decoder. Shorthand
  constructors: `int()`, `string()`, `float()`, `bool()`, `bitarray()`,
  `nil()`, `list(c)`.
- `codec.map(c, wrap, unwrap)` derive a codec for a custom type.
- `codec.subject()` serialize a `Subject(BitArray)` for cross-node
  request/response.
- `global.call(target, make_request, response_decoder, timeout)`.
  synchronous request/response across nodes.
- `global.reply(reply_to, response, encoder)` send a response back.
- `composite.option(c)`, `composite.result(ok, err)`,
  `composite.tuple2(a, b)`, `composite.tuple3(a, b, c)`.
- `registry.named(name, codec)` short form of `typed_name`.
