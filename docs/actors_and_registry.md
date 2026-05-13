# Actors and Global Registry

The core of `distribute` is the ability to link a global name string to a type-safe subject.

## The `TypedName`

A `TypedName(msg)` bundles a name string with the encoder and decoder for its messages. You define it once and share it across your nodes.

```gleam
import distribute
import distribute/codec

// Define a protocol for a counter
pub fn counter_name() {
  distribute.named("counter_service", codec.int())
}
```

## Starting and Registering Actors

`distribute` provides several ways to start an actor and register it in Erlang's `:global` registry.

### 1. `start_registered` (The Easy Way)

Best for singleton actors that you manually start on a specific node.

```gleam
let assert Ok(gs) = distribute.start_registered(
  counter_name(),
  0, // initial state
  handler,
)
```

For cross-node production traffic, prefer the observed variant when you
care about visibility into malformed or oversized payloads. The
observed variant lives in `distribute/actor` (it is intentionally not
re-exported on the facade because it is a low-volume escape hatch):

```gleam
import distribute/actor as dist_actor

let assert Ok(gs) = dist_actor.start_registered_observed(
  counter_name(),
  0,
  handler,
  5000,
  fn(err) {
    // log / telemetry hook
    Nil
  },
)
```

### 2. `start_supervised` (The Reliable Way)

Best for actors that should automatically restart if they crash. If
the process dies, the supervisor restarts it and the library
automatically re-registers the name. Use the default form to pick up
`config.default_init_timeout_ms`, or
`start_supervised_with_timeout` for an explicit override:

```gleam
// Default init timeout, taken from config.
let assert Ok(pid) = distribute.start_supervised(
  counter_name(),
  0,
  handler,
)

// Or with an explicit timeout:
let assert Ok(pid) = distribute.start_supervised_with_timeout(
  counter_name(),
  0,
  handler,
  5000, // init timeout
)
```

### 3. `pool` (The Scalable Way)

Starts `N` workers, registering them as `name_1`, `name_2`, etc.
Default form picks up `config.default_init_timeout_ms`;
`pool_with_timeout` takes the timeout explicitly.

```gleam
let assert Ok(sup_pid) = distribute.pool(
  counter_name(),
  4, // size
  0,
  handler,
)

// Or with an explicit init timeout:
let assert Ok(sup_pid) = distribute.pool_with_timeout(
  counter_name(),
  4,
  0,
  handler,
  5000,
)
// Look up "counter_service_2"
```

> [!WARNING]
> `pool` + `:global` has a known cascade risk: if one worker repeatedly
> fails registration after a split-brain heal (for example `name_4`
> already owned on another partition), supervisor restart intensity can
> bring down the whole pool. Track
> `telemetry.ConflictResolved`/`telemetry.ConflictResolverFailed` and
> treat repeated conflicts on the same pool name as an operational alarm.

## The `GlobalSubject`

When you look up an actor, you get a `GlobalSubject(msg)`. This is a wrapper around a raw BEAM `Subject` that knows how to encode messages for that specific service.

```gleam
import distribute

let tn = counter_name()
case distribute.lookup(tn) {
  Ok(gs) -> {
    // We found it!
    let _ = distribute.send(gs, 42)
  }
  Error(Nil) -> {
    // Name not registered on any connected node
  }
}
```

## Monitoring and Observations

If you need to log or meter malformed messages (e.g., during a rolling
deploy where codecs might mismatch), use the `_observed` variants. The
facade exposes `start_actor_observed` (uses the default init timeout);
the registered variant lives in `distribute/actor`:

```gleam
import distribute
import distribute/actor as dist_actor

// Local actor (not registered globally), default init timeout:
distribute.start_actor_observed(
  tn,
  0,
  handler,
  fn(err) {
    io.println("Malformed message received!")
  },
)

// Registered actor, with explicit init timeout:
dist_actor.start_registered_observed(
  tn,
  0,
  handler,
  5000,
  fn(err) {
    io.println("Malformed message received!")
  },
)
```

## Low-level Registry API

If you already have a `GlobalSubject` and want to register it manually later:

- `distribute.register(name, subject)`
- `distribute.unregister(name)`

---
[Master Messaging and the Call pattern](./messaging.md)
