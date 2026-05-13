<!-- markdownlint-disable MD024 MD049 MD060 -->

# Roadmap

Indicative release sequencing for the v4.x line. This is a
**tentative** outline, not a commitment: priorities and dates may
shift as user feedback arrives and audit cycles flag new work.
What stays fixed is the SemVer contract: patch releases carry
bug fixes only, minor releases are strictly additive, with no
breaking changes inside v4.

## v4.0.0 shipped

First production-grade release. Hardened wire format, single-
write configuration, atom-budget guardrail, fast-fail call path,
mailbox-safe `call_isolated`, conflict-resolver hook on the
registry, structured telemetry on every load-bearing boundary.
See [`CHANGELOG.md`](./CHANGELOG.md) for the full breakdown.

## v4.0.1 patch (target: within ~2 weeks of v4.0.0)

Bug fix only, no new features.

- **`registry.lookup_async` zombie under net-split.** When the
  peer that holds a registered name leaves the cluster mid-poll,
  the polling worker today spins until its deadline expires.
  Under fan-in (many concurrent lookups toward a vanishing
  peer) this wastes scheduler time at scale. The fix is to
  subscribe `cluster_monitor` from the poller and abort early on
  `NodeDown` of the relevant peer. Multi-node Z-suite test
  asserts that 100+ pending lookups toward a deliberately-killed
  peer all return inside a small grace window rather than
  running to deadline.

## v4.1.0 minor (target: weeks-to-months)

Strictly additive. No breaking changes for users staying on
`:global`.

- **Backend abstraction (Phase 1).** Introduce an opaque
  registry-backend type with one constructor today
  (`global_backend()`) so that future minor releases can add
  more constructors without changing user code. Internal
  refactor only; behaviour identical to v4.0 for any caller
  that does not opt in to a non-default backend.
- **Ergonomic retouch.** Candidate items: a `LookupOptions`
  record to replace the positional `(timeout_ms,
  poll_interval_ms)` pair on the polling lookups, plus
  whatever small adjustments accumulate from real-world use.

## v4.2.0 minor (Target: Absolute Priority)

Opt-in second backend: `syn`. Attualmente `:global` strozza i cluster superiori a 50 nodi, rendendo `distribute` limitato. Il passaggio a `syn` è il vero step enterprise.

- **`syn`-backed registry as a sibling package.** Likely shipped
  as `distribute_syn` so users staying on `:global` do not pull
  the `syn` Erlang application into their dependency closure.
  Multi-node integration tests parity with the `:global`
  backend; migration notes covering cold-flip semantics; no
  live `:global` → `syn` migration tooling in this release.

## Beyond v4.2

Open. Theorical. Possible directions: Raft-backed registry (`khepri` /
`ra`), performance work on the codec hot path. Each of these
warrants its own design pass before it lands on a release. None
of them are blockers for the current API.
