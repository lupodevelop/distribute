# Safety and Limits

`distribute` is designed for production stability. It provides explicit
mechanisms to prevent resource exhaustion and handle distributed
failure modes.

## Payload Size Limits

To prevent a single malformed or malicious message from crashing a
node with an `OutOfMemory` error, `distribute` enforces a global size
limit.

### Configuration

Set the limit via `distribute.configure`:

```gleam
distribute.configure(config.Config(
  ..config.default(),
  max_payload_size_bytes: 2 * 1024 * 1024, // 2 MiB
))
```

### Enforcement

The limit is checked at every I/O boundary:

- **`send`**: encoded binaries are checked before they leave the process.
- **`reply`**: responses are checked before they go back to the caller.
- **`receive`**: incoming binaries are checked against their raw size
  before any decoding attempt.

If a binary exceeds the limit, the function returns
`Error(PayloadTooLarge(size))`.

### Hardening knobs (advanced)

In addition to payload and atom budget, you can tune these limits from
`config.Config`:

- `max_decoded_list_elements` (default `10_000`): caps list frame size in
  decoders to mitigate CPU/heap amplification from tiny payloads with huge
  declared counts.
- `isolated_proxy_shutdown_grace_ms` (default `5_000`): wait window for
  proxy DOWN in `call_isolated` timeout teardown.
- `resource_owner_poll_ms` (default `5_000`): fallback liveness poll for
  `start_resource_owner` when DOWN delivery is delayed.
- `health_proxy_shutdown_grace_ms` (default `1_000`): wait window for
  proxy DOWN in `cluster.health` timeout teardown.

These are intentionally advanced knobs: raise them only with production
evidence and load tests. Lower values reduce tail latency in timeout paths;
higher values reduce late-send races under heavy NIF-bound workloads.

`cluster.health` also applies graceful-first teardown to in-flight ping
workers on deadline (`exit(..., shutdown)`), escalating to brutal kill only
for workers still alive after the graceful signal.

## Error Hierarchy

Instead of crashing or returning generic errors, `distribute` uses
specific error types for different failure modes.

### `SendError`

Occurs during asynchronous delivery:

- `SendEncodeFailed`: your codec failed to serialize the message.
- `PayloadTooLarge`: the message is too big for the current config.

### `CallError`

Occurs during synchronous request/response:

- `Timeout`: no reply arrived within the timeframe.
- `TargetDown`: the target process or node died during the call.
- `CallEncodeFailed`: request serialization failed.
- `CallPayloadTooLarge`: the request or response exceeded size limits.

## Atom Table Safety

In Erlang, atoms are not garbage collected. Converting dynamic strings
to atoms can lead to **Atom Table Exhaustion**, crashing the entire VM
(default cap: 1 048 576 atoms).

**How `distribute` stays safe**:

1. **Registry names stay binary.** `:global` accepts any term as a
   name, so `register_global` uses the raw `String` without ever
   touching the atom table. Add as many actors as you like.
2. **Config keys are static atoms.** The `persistent_term` key is a
   single compile-time atom (`distribute_config`). No growth.
3. **Subject decode runs in `[safe]` mode.** `binary_to_term/2` with
   `[safe]` refuses to allocate new atoms while deserialising remote
   subjects.

The only paths that *can* create an atom are bounded:

| Path | Charset | Length cap | Bounded by |
| --- | --- | --- | --- |
| `cluster.start_node` (name) | `[a-zA-Z0-9_-]+@[a-zA-Z0-9._-]+` | 255 bytes | Once per node start |
| `cluster.start_node` (cookie) | `[a-zA-Z0-9_-]+` | 255 bytes | Once per node start |
| `cluster.connect` / `ping` | same node-name format | 255 bytes | Once per remote peer name |

A real cluster sees on the order of N node atoms, where N is the count
of distinct peers ever contacted. Even pathological clusters of
100 000 nodes are 4 orders of magnitude below the atom cap.

### `max_distribution_atoms` guardrail

`config.max_distribution_atoms` (default `10_000`) is a hard, atomic,
lock-free cap on how many *fresh* atoms `distribute` will mint through
its FFI helpers (node names and cookies). Existing atoms cost nothing.

Once the counter reaches the cap, every further call to
`cluster.connect` / `cluster.ping` / `cluster.start_node` returns a
typed `*AtomBudgetExceeded` error **before** `binary_to_atom/2` is
invoked. The VM atom table is untouchable through these paths even if
the caller passes a million unique valid names.

```gleam
// Tighten the budget for a service that exposes node names from
// untrusted input. 100 fresh atoms is plenty for any real cluster.
distribute.configure(config.Config(
  ..config.default(),
  max_distribution_atoms: 100,
))
```

The counter is per-VM, persists across `configure` calls (changing the
budget only changes the cap, not the running count), and is implemented
with `atomics:add_get/3` so it is correct under concurrent access from
multiple schedulers.

## Threat Model

Where the typed boundary holds, and where it does not.

### In scope (defended)

- **Inbound oversized messages.** Every `binary → typed` path checks
  `max_payload_size_bytes` *before* invoking the decoder. This includes
  `global.receive`, `receiver.receive_typed`, the actor handlers
  (`start_receiver_observed`, `start_distributed_worker_observed`), and
  the selector path (`selecting_typed`). The handler is never called
  with oversized data.
- **Inbound malformed binaries.** Decode failure returns a typed
  `DecodeError` and (in `_observed` variants) fires
  `on_decode_error(err)`. The actor stays alive; the message is
  dropped.
- **Encode-side overflow.** `int_encoder` rejects values outside
  signed 64-bit. `tagged.encoder` rejects versions or lengths outside
  unsigned 32-bit. The list encoder rejects more than 10 000
  elements (same cap enforced by the list decoder). No
  self-incompatible payload crosses the wire.
- **Dead target during `call`.** Monitor-based `TargetDown` returns
  immediately. Late replies are drained from the reply subject, and
  late DOWN messages are flushed by `process.demonitor_process`
  (`gleam_erlang` already passes `[flush]`).
- **Registration races.** `start_registered` and `child_spec` `unlink`
  and brutally `kill` orphan workers when `:global.register_name`
  rejects the name. An orphan that traps exits cannot survive.
- **Atom-flood via FFI.** Node names and cookies pass charset and
  length validation before hitting `binary_to_atom`.

### Out of scope: actor `terminate/2` callback (upstream gap)

`gleam/otp/actor` 1.x **does not implement** OTP's
`{terminate, Reason}` system message (see `gleam_otp_external.erl`).
Two consequences for any actor that owns external resources (file
handles, ETS tables, ports, locks, sockets):

- **Handler-controlled exits run user code first.** Returning
  `receiver.Stop` or `receiver.StopAbnormal` from your handler gives
  you the chance to release every resource before the actor exits.
  This is the only path that the library can guarantee.
- **External terminations skip the actor.** A `process.kill` (the
  uncatchable cleanup path used when `:global` registration fails) or
  a supervisor-driven `exit(Pid, shutdown)` does **not** invoke any
  user callback. Resources owned only on the actor's heap are reaped
  by the BEAM (memory), but external handles leak.

The OTP-pure mitigation is shipped as
`actor.start_resource_owner(open, close, lifetime)`: it spawns an
unlinked observer process that opens the resource, monitors the
lifetime PID (typically the actor), and runs `close(resource)` when
the lifetime PID dies for any reason. A copy-pasteable example lives
in [recipes.md § 8](./recipes.md#8-resource-cleanup-with-start_resource_owner).
Upstream tracking issue: gleam-lang/otp#126.

When upstream `gleam/otp` ships terminate support, the contract of
the helper does not change. Callers can replace the helper call with
the native API; the helper stays as a deprecated thin wrapper for the
more general "tie cleanup to any PID" pattern.

### BEAM and OS-level risks the library cannot abstract away

- **`start_node` blocks on `epmd` and DNS.**
  `cluster.start_node` calls `net_kernel:start/1`, which talks to
  the Erlang Port Mapper Daemon and runs the host name through the
  OS resolver. There is no Gleam-side timeout the library can
  interpose: if `epmd` is impaled, if DNS is unreachable, or if
  `/etc/hosts` is misconfigured, the BEAM hangs on a libc resolver
  timeout that can be tens of seconds long. Mitigations: run
  `epmd -daemon` before the process boots, prefer IP literals
  (`myapp@127.0.0.1`) where the deployment allows, supervise the
  boot path itself if you need a hard deadline. See `start_node`'s
  own docstring for the full pattern.
- **`global.send` is fire-and-forget.** A successful `send` means
  the BEAM accepted the message for delivery, **not** that the
  target received it. The remote PID may have died between the
  `lookup` and the `send`, the connection may drop mid-route, or
  the receiving node may panic before processing the mailbox. If
  delivery confirmation matters, use `call` (synchronous, monitor-
  backed) or build an application-level ack on top of `send`. The
  type system cannot lie about this: it is the BEAM's contract.
- **`registry.lookup` is a snapshot.** It returns the Subject for
  the PID found at the exact moment the lookup ran. The PID can
  crash, deregister, or leave the cluster a microsecond later.
  Subsequent `send` calls on a stale Subject silently drop. For
  long-lived references, re-`lookup` before each critical send, or
  use `call` (which monitors the target and returns
  `Error(TargetDown)` when the PID has died).
- **`:global` resolves name conflicts after net-split with a brutal kill.**
  `distribute` builds the registry on top of Erlang's `:global`
  module. After a network partition heals and two halves rejoin,
  `:global` may discover that the same name was registered to
  different PIDs on each side. Its conflict-resolution function
  picks a winner and sends an uncatchable `exit(loser_pid, kill)`
  to the other PID. The kill is by design uncatchable; the
  resolver decision is what callers can shape.

  Default behaviour: `:global.random_notify_name/3` is a coin
  flip. For workloads where the choice matters (a leader pinned
  to a specific node, a router whose state lives on a primary, a
  singleton whose oldest instance is authoritative) coin-flip
  resolution turns split-brain healing into a non-deterministic
  outage.

  **Mitigation: `register_global_with_resolver`.** Pair the
  registration with a `distribute/conflict.Resolver` to swap the
  default for a deterministic policy. Built-ins:

  - `conflict.lowest_pid_wins()` / `highest_pid_wins()`; stable
    tiebreakers when no domain policy fits.
  - `conflict.keep_local()`; always prefer the PID running on
    the resolver's node.
  - `conflict.node_priority(["primary@host", "secondary@host"])`
    defines a static hierarchy: PIDs on listed nodes beat PIDs on
    unlisted nodes; ties within the list use lowest-PID.

  The resolver runs *inside* `:global`'s singleton worker
  process, so a slow or panicking user function would otherwise
  stall every `:global` operation cluster-wide. The library
  wraps every resolver in an FFI shim that:

  - spawns a short-lived worker to run the user fn;
  - imposes a hard deadline from `config.conflict_resolver_timeout_ms`
    (default **1 000 ms**);
  - on timeout, kills the worker and performs a bounded wait for `DOWN`
    with monitor-flush fallback, so resolver teardown cannot hang the
    `:global` server on stale monitor timing;
  - falls back to lowest-PID-wins on timeout or crash;
  - emits `telemetry.ConflictResolved` on every resolution and
    `telemetry.ConflictResolverFailed` whenever the user fn
    misbehaved and the fallback fired.

  Watch the failure event in production: a steady stream means
  the resolver itself is broken. A steady stream of resolutions
  (without failures) means the cluster is flapping, partitions
  are healing repeatedly, and the application can't fix it on
  its own.

  Note that conflict resolution at the registry level **cannot
  reconcile the state** of two diverged actors. The loser dies,
  the winner keeps its state, anything pending on the loser is
  gone. Reconciling state is the next layer up (snapshot the
  loser before the kill, merge into the winner) and intentionally
  not in this scope. The plan to ship that as a higher-level
  "merge_resolver" composes on top of the `Resolver` type without
  changing it.

  For workloads where even the resolver+telemetry path is
  unacceptable, use `pg` (process groups, eventually-consistent,
  no kill-on-merge) or a consensus layer instead of
  `:global`-backed registration.
- **No automatic backpressure on `send`.** `global.send` is
  asynchronous and the library does not enforce a high-water mark
  on the receiver's mailbox. A misbehaving sender (buggy loop,
  hostile peer) can flood a target actor with millions of small
  messages. `max_payload_size_bytes` only bounds *per-message*
  size, not aggregate queue depth. If the receiver is critical,
  apply backpressure at the application layer:
  - prefer `call` (synchronous) on the hot path so the sender
    waits for the receiver and the queue cannot grow unbounded;
  - watch `process.message_queue_data/1` from a supervisor and
    shed load (drop, redirect, kill) past a threshold;
  - rate-limit at the source rather than relying on the receiver
    to survive arbitrary fan-in.
- **Decoding runs in the receiver's process.** `receiver.receive_typed`
  and the actor handler decode the inbound binary on the actor's
  reduction budget. A complex codec (deeply nested composites,
  large lists near the 1 M cap, custom `dynamic` validation in user
  code) can spend tens of milliseconds parsing one message; while
  it parses, the actor cannot drain the rest of its mailbox. Under
  a fan-in burst this becomes head-of-line blocking and the queue
  grows. Mitigations:
  - keep codecs simple and bounded (`int`, `string`, `bool`,
    `tuple2`, fixed-shape records via `codec.map`); avoid building
    a parser inside `decode`;
  - move heavy decoding to a worker pool: the entry actor pulls
    the binary and forwards it to a sized `pool` (recipe 3), which
    deserialises in parallel and returns the typed result;
  - cap inbound list element counts via `max_decoded_list_elements`
    so a hostile sender cannot induce O(n) decode time on a 4 MiB
    list of trivial elements.
- **Sub-binary memory retention.** BEAM's binary heap shares storage
  between a parent binary and any sub-binary slices taken from it.
  A 10-byte string extracted from a 50 MiB payload would normally
  pin the full payload in RAM until the slice is collected. The
  codec mitigates this by calling `binary:copy/1` on every
  decoded `String` and `BitArray`, paying a small allocation per
  decode in exchange for predictable memory behaviour in long-
  lived actor state. Custom codecs that bypass `string_decoder` /
  `bitarray_decoder` need to do the same.

### Out of scope (developer responsibility)

- **Trusted vs untrusted node names.** `cluster.connect` and
  `cluster.start_node` *will* create atoms for any input that passes
  format validation. Treat node names and cookies as **administrative
  input**, never user input. If you must accept untrusted node names,
  layer a strict allowlist before `connect`.
- **Cross-codebase codec drift.** `TypedName(msg)` enforces type
  safety *within one codebase*. Two services using different codecs
  for the same name will reject each other's binaries at runtime; the
  compiler cannot bridge separate codebases.
- **Wire-level transport.** `distribute` rides BEAM distribution. It
  inherits the encryption story (none unless you front it with TLS via
  `inet_dist_listen_*`), the latency story, and the partitioning
  story of the underlying network. Plan accordingly.
- **Authentication beyond the cookie.** Erlang's distribution cookie
  is a shared secret, not an auth protocol. For untrusted networks
  use TLS distribution (`-proto_dist inet_tls`) and rotate cookies.
- **Retries and backoff.** Every error is surfaced fast and typed.
  Retry strategy is application-specific and intentionally not built
  in.
- **External-resource cleanup on actor shutdown.** `gleam_otp/actor`
  1.x does not yet implement OTP's `{terminate, Reason}` callback.
  Handler-driven exits can clean up in user code, but external
  shutdown paths (`process.kill`, supervisor-driven
  `exit(Pid, shutdown)`) require the linked resource-owner pattern
  documented in `docs/recipes.md`.

## Transport: plaintext by default

`distribute` sits on top of standard BEAM distribution, which is
**plaintext by default**. The cookie is a shared secret, not an
authentication protocol. It travels in the clear during the handshake
along with every subsequent message.

**Production deployments outside a trusted network must enable TLS
distribution.** This is an Erlang VM concern, not a `distribute` API
concern, but the library will not save you from skipping it.

Minimal setup (Erlang side, applied via `vm.args` or `-erl`):

```sh
-proto_dist inet_tls
-ssl_dist_optfile /etc/distribute/ssl_dist.conf
```

Where `ssl_dist.conf` looks like:

```erlang
[{server, [{certfile, "/etc/distribute/cert.pem"},
           {keyfile,  "/etc/distribute/key.pem"},
           {cacertfile, "/etc/distribute/ca.pem"},
           {verify, verify_peer},
           {fail_if_no_peer_cert, true}]},
 {client, [{certfile, "/etc/distribute/cert.pem"},
           {keyfile,  "/etc/distribute/key.pem"},
           {cacertfile, "/etc/distribute/ca.pem"},
           {verify, verify_peer}]}].
```

Operational checklist:

- Use mutual TLS (`verify_peer`) on both server and client, not just
  the listener.
- Rotate the distribution cookie regularly. Treat a leaked cookie as
  a full compromise of the cluster.
- Run `epmd` on a private interface only, or replace with `erl_epmd`
  if your OTP version supports it.
- Restrict the distribution port range with `inet_dist_listen_min/max`
  so firewall rules can be tight.

## Configuration is single-write by design

`distribute.configure` writes to `persistent_term`. Every write
triggers a global GC pass across every process in the VM. To prevent
both accidental and **hostile** widening of `max_payload_size_bytes`
at runtime, the FFI rejects the second call with
`Error(AlreadyConfigured)` and the active configuration stays at
whatever was set first.

This means:

- A library you depend on cannot raise the payload limit behind your
  back.
- An attacker with code-execution access cannot disable size
  enforcement by re-calling `configure` with a 1 GiB limit. (They
  could replace `persistent_term` directly via FFI, but at that level
  the entire VM is compromised anyway.)
- A bug that calls `configure` twice surfaces immediately, instead of
  silently flipping the active limits between callers.

If you genuinely need to change the active limit (test fixtures,
controlled rollouts), call `config.reset()` followed by `configure`.
`reset` is `@internal` and only intended for tests; production code
should not use it.

**Red line:** never wire `distribute.configure` to runtime traffic
(HTTP handlers, webhooks, admin endpoints). Even a single successful
write to `persistent_term` on a live, actor-dense node can cause a
VM-wide pause long enough to trigger heartbeat failures and cluster
instability.

## Late-reply mailbox accumulation in `call`

`global.call` (and `distribute.call`) installs a synchronous
`drain_reply` at timeout or `TargetDown`, but the BEAM has no built-in
mechanism to drop messages tagged with a `gleam_erlang` Subject *after*
the Subject goes out of scope. If the target replies with a latency
exceeding the call timeout, the binary still lands in the caller's
mailbox tagged with the now-orphan reply Subject, and no selector ever
matches it again.

The cost is paid by **selective receive**: every future
`process.receive` in the caller scans past the orphan first. A handful
is harmless; tens of thousands quietly degrade the caller's
throughput.

How callers should defend themselves:

| Caller shape | Risk | Recommendation |
| --- | --- | --- |
| Short-lived CLI / script | low. Orphan dies with the process | use `call` directly |
| Long-running process (plain or `gleam/otp/actor`) issuing many RPCs | **high**. Orphans accumulate; every future `selector_receive` pays the scan cost | use the *Isolated call* recipe in `docs/recipes.md` (proxies each call through a short-lived process whose mailbox is reaped on exit) |

> **Common misconception.** `gleam/otp/actor` is **not** immune to
> this leak. Its loop is built on `process.selector_receive`. Messages
> that do not match the configured selector are not silently dropped:
> they stay in the mailbox and slow every subsequent receive. Treat
> long-running actors that issue `call`s as the high-risk shape, not
> the safe one. The only design that bounds the mailbox under
> sustained timeouts is the proxy-per-call pattern in the
> *Isolated call* recipe.

A future revision may swap `gleam_erlang` Subjects for
`erlang:alias/0`-based identifiers, which the BEAM drops at the
scheduler level after `unalias/1`. That requires bypassing
`gleam_erlang`'s Subject layout and is out of scope for v4.0.0.

## Registry backend: today `:global`, tomorrow pluggable

`distribute` currently routes every name registration through
Erlang's `:global` module. That choice gives strong consistency
("one PID per name across the cluster") at the cost of a known
ceiling: `:global` does not scale beyond ~50-100 nodes due to its
gossip-based coordination overhead, and its singleton
`global_name_server` serialises every operation cluster-wide.

The library's FFI is factored so the `:global` dependency lives
in two files only: `registry_ffi.erl` (register / unregister /
whereis) and `conflict_ffi.erl` (resolver-backed register). The
Gleam side is engine-agnostic. Once the v4 line is stable, the
plan is to introduce a `RegistryBackend` opaque type with
`global_backend()` (default), `syn_backend()` (third-party,
eventual consistency, scales to millions of names), and
`khepri_backend()` / `ra_backend()` (Raft-based strong
consistency for fault-tolerant deployments) as options. Selection
goes through `Config`; user code stays unchanged.

A few honest caveats for that plan:

- `pg` (Erlang's process_groups) has different *semantics*;
  multiple PIDs per group, no kill-on-merge, so it cannot
  be a drop-in registry backend. It would arrive as a separate
  `distribute/group` module if the demand justifies it, not as a
  swap.
- Backends differ in consistency guarantees. `syn` is eventually
  consistent and may briefly resolve different `lookup` results
  on different nodes during membership change. Raft-backed
  registries pay a write-latency cost for strong consistency.
  The right choice is workload-specific; the library will not
  pick one for you.
- v4.0.0 ships with `:global` only. Migrating to a future backend
  is opt-in and will be a configure-time change, not a code-
  rewrite. No wire-level compatibility is implied between
  backends.

For deployments that already run into the `:global` ceiling, the
operational answer today is to shard the namespace across
multiple `:global` clusters or to run a custom registry on top of
`distribute` primitives. A pluggable backend abstraction is on
the medium-term roadmap; see [`ROADMAP.md`](../ROADMAP.md) at
the project root for the indicative sequencing.

## Recommended Payload Limits

`max_payload_size_bytes` is the single knob that bounds memory
pressure per message. Pick a value tied to your workload, not a
default.

| Workload | Recommended limit | Rationale |
| --- | --- | --- |
| Cluster control plane, RPC, heartbeats | **64 KiB** | Tight cap; oversized means bug or attack. |
| Typical request/response APIs | **256 KiB** | Plenty of room for nested records, lists. |
| User-payload ingest (event bus) | **1 MiB** | Default for a permissive event-driven system. |
| Bulk transfer over `distribute` | **4 MiB** (default) | Upper bound for one logical message; chunk above this. |
| Anything > 4 MiB | **don't** | Move bulk data through a side channel (S3, NFS, BitTorrent). Keep `distribute` for control. |

Operational guidance:

- **Set the limit at startup, never at runtime.**
  `persistent_term:put/2` triggers a global GC pass. Fine once,
  expensive on every change.
- **Monitor `on_decode_error`.** Wire
  `start_distributed_worker_observed` to your metrics pipeline. A
  spike in `PayloadTooLarge` is either a bug in a peer or an attack in
  progress.
- **Match limits across the cluster.** Heterogeneous limits are legal
  but produce confusing one-way drops. Ship config as part of your
  release.

---

## Architecture and Design Decisions

To use `distribute` effectively, it helps to understand the reasoning
behind its technical constraints.

### 1. Why no auto-derive?

Gleam currently doesn't have a macro system or reflection. While
other languages might auto-generate codecs, `distribute` requires
manual codec composition. This ensures that you have total control
over the binary format, which is critical for long-term wire
compatibility.

### 2. Subject Construction

Gleam's `Subject` is opaque. To build a subject from a remote PID and
a name-based tag, we use a single [Erlang FFI function](https://github.com/lupodevelop/distribute/blob/main/src/distribute_ffi_utils.erl).
This keeps the "unsafe" part of the library isolated to one small,
audited boundary.

### 3. Responsibility of the Developer

- **Retries.** `distribute` surfaces errors instantly. It does not
  implement retry logic because retrying is a business decision: do
  you retry indefinitely, fail over to a cache, or surface the error?
- **Node Boundaries.** Type safety is verified by the codecs you
  provide. If Node A and Node B use different codecs for the same
  name, they will correctly return `DecodeError`. The compiler cannot
  check Node A's code against Node B's code.

### 4. BEAM Distribution Limits

`distribute` sits on top of standard BEAM distribution. It inherits
the strengths (efficiency, location transparency) and the weaknesses
(unreliable UDP/TCP transport under high latency) of the underlying
Erlang networking stack.
