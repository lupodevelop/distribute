<!-- markdownlint-disable MD024 MD049 MD060 -->

# Migrating from v3.x to v4.0.0

v4 is a hardening release. The wire format, error surface, and facade
shape all changed. This guide walks through the rewrites a v3 codebase
needs, in the order you will hit them.

If you only have a few minutes, read these three things:

1. v4 nodes cannot speak to v3 nodes. The codec length prefix went
   from 16 to 32 bits. Plan a full-cluster restart, not a rolling
   upgrade across the major.
2. There is now an optional boot step:
   `distribute.configure(...)`. Skipping it is fine. The library reads
   built-in defaults (5 s call timeout, 5 s init timeout, 4 MiB payload
   cap, 10 000 fresh atoms) when no configuration is loaded. Run
   `configure` only when you want to override one of those values.
3. The facade module (`distribute`) is now the recommended entry
   point. Most v3 code that imported `distribute/global`,
   `distribute/registry`, and `distribute/cluster` directly can drop
   those imports.

## 1. Boot: configure when you need overrides

v3 had no central configuration. Magic numbers lived inside each
module. v4 introduces `distribute.configure(...)` as the single tuning
point, but calling it is optional. If you skip it, the library reads
built-in defaults: 5 s call timeout, 5 s init timeout, 4 MiB payload
cap, 10 000 fresh atoms.

Call `configure` only when you want different values:

```gleam
import distribute
import distribute/config

pub fn boot() {
  let assert Ok(Nil) =
    distribute.configure(config.Config(
      ..config.default(),
      default_call_timeout_ms: 10_000,
      max_payload_size_bytes: 16 * 1024 * 1024,
    ))
  Nil
}
```

Notes:

- `configure` is single-write. Calling it twice returns
  `Error(AlreadyConfigured)`. Run it from your top-level supervisor.
- All `_with_timeout` siblings still exist for cases where you need
  to override per call site.

## 2. Wire format: 32-bit length prefix

The codec length prefix on `String` and `BitArray` widened from 16 to
32 bits. Lists now use a 32-bit element count and the encoder enforces
the same 10 000-element cap as the decoder.

What this means in practice:

- v3 nodes cannot send messages a v4 node will accept, and vice versa.
- The previously silent truncation of large `Int` values now returns
  `Error(ValueTooLarge)`.
- Tagged-message version overflow now returns `Error(ValueTooLarge)`
  instead of wrapping.
- A binary with trailing bytes after the value is rejected as
  `Error(InvalidBinary)` in every codec.

If you persisted v3-encoded blobs to disk or sent them through a
non-distributed transport, those blobs are no longer decodable by a
v4 node. Either re-encode them on a v3 node and migrate, or run a
one-shot translation pass.

## 3. Node lifecycle: `start_node` not `start`

```gleam
// v3
distribute.start("myapp@127.0.0.1", "shared-cookie")

// v4
distribute.start_node("myapp@127.0.0.1", "shared-cookie")
```

The v3 name was ambiguous. `start_node` makes it obvious.

`is_distributed` now reads from `erlang:is_alive/0` instead of
comparing the node name string to `"nonode@nohost"`. If you ever set
a custom node name in tests, the v3 check would silently lie. v4 is
authoritative.

`is_healthy` was renamed to `has_peers`. "Health" implied
infrastructure failure on a single-node deployment, which was wrong.
Update K8s probes and operator dashboards.

## 4. The slim facade

v4 reorganises the facade around a "default form" plus an
`_with_timeout` override for every long-running call. The default
form reads the relevant timeout from `config.get()`.

| v3 / explicit                                       | v4 default form                              |
| --------------------------------------------------- | -------------------------------------------- |
| `distribute.call(gs, make_req, dec, 5000)`          | `distribute.call(gs, make_req, dec)`         |
| `distribute.call_default(gs, make_req, dec)`        | `distribute.call(gs, make_req, dec)` (renamed) |
| `distribute.receive(gs, 5000)`                      | `distribute.receive(gs)`                     |
| `distribute.start_actor(tn, init, h, 5000)`         | `distribute.start_actor(tn, init, h)`        |
| `distribute.start_registered(tn, init, h, 5000)`    | `distribute.start_registered(tn, init, h)`   |
| `distribute.start_supervised(tn, init, h, 5000)`    | `distribute.start_supervised(tn, init, h)`   |
| `distribute.pool(tn, n, init, h, 5000)`             | `distribute.pool(tn, n, init, h)`            |
| `distribute.child_spec(tn, init, h, 5000)`          | `distribute.child_spec(tn, init, h)`         |
| `distribute.start_actor_observed(tn, init, h, 5000, hook)` | `distribute.start_actor_observed(tn, init, h, hook)` |
| `distribute.new_subject(encoder, decoder)`          | `distribute.new_subject(codec)`              |

`make_req` above is the constructor function `fn(reply_subject) ->
req` that `call` injects so the handler can answer. A typical
message type looks like `Get(reply: process.Subject(BitArray))`,
and the call site passes the `Get` constructor directly.

If you actually need a per-call timeout, use the explicit sibling:

```gleam
let _ = distribute.call_with_timeout(target, Get, codec.int_decoder(), 30_000)
let _ = distribute.start_actor_with_timeout(tn, init, handler, 60_000)
```

`call_isolated` is new. It runs the request inside an unlinked proxy
process so late replies cannot accumulate in a long-running caller's
mailbox. Use it for managers, polling loops, and anything that issues
many RPCs under sustained timeouts:

```gleam
distribute.call_isolated(target, Get, codec.int_decoder())
distribute.call_isolated_with_timeout(target, Get, codec.int_decoder(), 30_000)
```

## 5. Error type changes

The single biggest shape change. Pattern matches must be updated.

### Cluster

| v3                                            | v4                                     |
| --------------------------------------------- | -------------------------------------- |
| `cluster.ConnectError.NodeNotFound`           | folded into `ConnectFailed`           |
| `cluster.StartError.StartFailed("cookie...")` | `cluster.StartError.InvalidCookieFormat(_)` |

`StartError` now distinguishes invalid cookies from invalid node
names from a real start failure. If you matched the old StartFailed
string to detect cookie format errors, switch to the explicit
variant.

### Calls

`CallError.CallTimeout` was renamed to `CallError.Timeout`.
`CallError` gained two new variants:

- `TargetDown`. The remote actor's PID is dead. v4 monitors the
  target and surfaces this immediately instead of blocking until the
  timeout fires.
- `CallPayloadTooLarge(Int)`. The encoded request exceeded
  `max_payload_size_bytes`.

```gleam
case distribute.call(target, Get, codec.int_decoder()) {
  Ok(value) -> ...
  Error(distribute.Timeout) -> ...
  Error(distribute.TargetDown) -> ...
  Error(distribute.CallDecodeFailed(_)) -> ...
  Error(distribute.CallEncodeFailed(_)) -> ...
  Error(distribute.CallPayloadTooLarge(_)) -> ...
}
```

The second argument is a constructor, not a value. `call` injects
the reply Subject so the handler can answer. A typical message type
looks like `Get(reply: process.Subject(BitArray))`.

### Sends

`global.send` previously returned `Result(_, codec.EncodeError)`.
v4 introduces a dedicated `SendError`:

```gleam
case distribute.send(gs, msg) {
  Ok(Nil) -> ...
  Error(distribute.SendEncodeFailed(_)) -> ...
  Error(distribute.PayloadTooLarge(size)) -> ...
}
```

### Registry

`registry.unregister` used to return `Nil` and silently swallow
"name not found" and "name owned by another node". Both are now
explicit:

```gleam
case distribute.unregister("counter") {
  Ok(Nil) -> ...
  Error(distribute.NotFound) -> ...
  Error(distribute.NotOwned) -> ...
}

// If you really want the v3 fire-and-forget shape, opt in:
let _ = distribute.unregister("counter")
```

The new `unregister_typed/1` removes the "hardcode the string at the
cleanup site" hazard. Pass the same `TypedName` you used to register:

```gleam
let counter = distribute.named("counter", codec.int())
let _ = distribute.unregister_typed(counter)
```

### Lookup

`registry.lookup_with_timeout` returned `Result(_, Nil)` in v3. v4
returns `Result(_, LookupError)` with three variants:

- `LookupNotFound`
- `LookupInvalidTimeout`
- `LookupInvalidPollInterval`

The previous tight-loop and `badarg` behaviour on zero or negative
poll intervals is gone. Both arguments are validated up front.

`lookup_with_timeout` also clamps each wait to the remaining budget,
so a poll interval larger than the deadline can no longer overrun by
up to one full interval.

### Actor start

`actor.start_registered` used to swallow the underlying actor start
error if registration failed afterwards. v4 introduces
`StartRegisteredError`:

- `ActorStartFailed(_)`. The actor's `init` returned an error.
- `GlobalRegisterFailed(_)`. The actor started but `:global` rejected
  the registration. The orphaned actor is killed before the error
  returns.

The orphan cleanup path is `process.unlink` plus `process.kill`. The
actor cannot trap-exit it.

### Receiver

`receiver.Next(state)` was renamed to `HandlerStep(state)` to avoid a
collision with `gleam/otp/actor.Next` when both are imported
unqualified.

`receiver.ReceiveError.Timeout` was renamed to `ReceiveTimeout` to
disambiguate from `global.CallError.Timeout`.

### Pool

`pool(size: 0)` used to silently succeed and produce a degenerate
supervisor with no workers. v4 returns
`Error(actor.InitFailed("pool size must be >= 1"))`.

## 6. Removed from the facade

The following were re-exported in v3 and are no longer on the facade.
They still exist in their original modules. If you used them, switch
the import:

| Symbol                         | New import                     |
| ------------------------------ | ------------------------------ |
| `register_pid`, `register_typed`, `whereis`, `is_registered`, `lookup_with_timeout`, `lookup_async` | `distribute/registry`         |
| `from_pid`, `from_subject`     | `distribute/global`            |
| `receive_typed`, `selecting_typed` | `distribute/receiver`     |
| `start_registered_observed`    | `distribute/actor`             |
| `connected_count`, `ping`      | `distribute/cluster` (or `distribute.health()`) |
| `typed_name_codec`             | use `distribute.named` (functionally identical, the duplicate was removed) |

The facade was trimmed to the 90% surface. Low-level escape hatches
stayed in their modules.

## 7. Codec API: `Codec(msg)` over encoder / decoder pairs

`new_subject` and registry helpers now accept a `Codec(msg)` value,
which bundles encoder, decoder, and the sized-decoder used for
composite framing. v3 took separate encoder and decoder functions.

```gleam
// v3
let gs = distribute.new_subject(my_encoder, my_decoder)

// v4
let gs = distribute.new_subject(codec.string())
let counter = distribute.named("counter", codec.int())
```

Built-in codecs live on `distribute/codec`:

- `codec.int()`, `codec.string()`, `codec.float()`, `codec.bool()`,
  `codec.bitarray()`, `codec.nil()`, `codec.subject()`
- `codec.list(inner)`
- `codec.map(inner, wrap, unwrap)` for records over tuples

Composite combinators live on `distribute/codec/composite`:

- `composite.option(inner)`, `composite.result(ok, err)`
- `composite.tuple2(a, b)`, `composite.tuple3(a, b, c)`

`composite.result/2` is new in v4 and rounds out the combinator set.

## 8. Cluster events: dedicated module

v3 callers monitored `:net_kernel` directly through ad-hoc helpers.
v4 ships `distribute/cluster_monitor`, a typed wrapper that pushes
`NodeUp(name)` and `NodeDown(name)` to subscribers.

```gleam
import distribute
import distribute/cluster_monitor

pub fn watch() {
  let assert Ok(mon) = distribute.start_monitor()
  let events = process.new_subject()
  distribute.subscribe(mon, events)
  // events now receives ClusterEvent values
}
```

The monitor proactively prunes dead subscribers via
`process.monitor`, so long-running deployments cannot leak the
subscription list.

## 9. Telemetry: opt-in event sink

New in v4. If you want to wire decode errors, atom budget refusals,
payload rejections, timeouts, and registration outcomes into your
metrics pipeline, install a sink at boot:

```gleam
import distribute/telemetry

pub fn install_metrics() {
  telemetry.install(fn(event) {
    case event {
      telemetry.PayloadRejected(size, cap, _origin) -> ...
      telemetry.DecodeFailed(_msg, _origin) -> ...
      telemetry.CallTimedOut(_elapsed) -> ...
      telemetry.AtomBudgetExhausted(_input, _origin) -> ...
      _ -> Nil
    }
  })
}
```

The sink is single-global and runs inline on the emitting process.
A slow sink slows the emit. Keep the handler cheap (counter
increment, queue push) and offload heavy work to a worker.

The sink is optional. v3 had no equivalent, so there is nothing to
migrate, but it is the path forward for any custom logging you
patched in.

## 10. Validation tightening

These are not new APIs but they will catch input v3 used to accept:

- Cookie, node name, and registry name are validated byte-wise in
  the FFI: charset `[a-zA-Z0-9._-]+`, 1 to 255 bytes. v3 had a
  Gleam-side codepoint check that disagreed with the FFI byte check
  on multi-byte input. The Gleam-side check is gone.
- `unregister` refuses names whose owning PID lives on another node.
  Standard `:global` auto-cleanup (PID monitor) is unaffected.

## 11. Quick rewrite checklist

For each module touching `distribute`, walk this list:

1. Add `distribute.configure(...)` to your boot path.
2. Replace `distribute.start(name, cookie)` with
   `distribute.start_node(name, cookie)`.
3. Replace `is_healthy()` with `has_peers()`.
4. Drop trailing timeout arguments where you used the v3 4-arg form
   and want the configured default. Switch to `_with_timeout` if you
   actually need an override.
5. Pattern matches on `CallError`: rename `CallTimeout` to `Timeout`,
   handle `TargetDown` and `CallPayloadTooLarge`.
6. Pattern matches on `ReceiveError`: rename `Timeout` to
   `ReceiveTimeout`.
7. Pattern matches on `unregister`: handle `NotFound` and `NotOwned`,
   or `let _ =` if you want fire-and-forget.
8. Pattern matches on `lookup_with_timeout`: switch from
   `Result(_, Nil)` to `Result(_, LookupError)`.
9. Replace `actor.start_registered` error handling with
   `StartRegisteredError`.
10. Replace `receiver.Next` with `HandlerStep`.
11. Replace `new_subject(encoder, decoder)` with
    `new_subject(codec)`. Define a `Codec` value once and reuse it.
12. If you used `register_pid`, `whereis`, `from_pid`, etc. via the
    facade, switch to `distribute/registry` or `distribute/global`.

If a v3 call still compiles after this list, it kept the same
contract. If it does not compile, the message points at the new
shape.

## 12. Test cluster: full restart

There is no compatibility shim. v4 nodes will not interoperate with
v3 nodes on the wire. Plan a full restart, not a rolling upgrade,
when you cross the major.

If you cannot afford a full restart, run a v3 process and a v4
process side by side, drain the v3 mailbox, then cut over. The
library does not provide tooling for this. It is a deployment
decision.

---

- Read [README](./README.md) for the full doc index.
- See [recipes.md](./recipes.md) for copy-pasteable v4 patterns.
- See [safety_and_limits.md](./safety_and_limits.md) for the threat
  model and operational caveats.
