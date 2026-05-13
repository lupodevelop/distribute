# Recipes

Short, copy-pasteable patterns. Each one is a single-file Gleam module that
runs against the v4.0.0 facade.

## 1. Hello-actor

The smallest useful program: start one named actor, send it a message.

```gleam
import distribute
import distribute/codec
import distribute/receiver
import gleam/io

pub fn main() {
  let assert Ok(Nil) = distribute.configure(distribute.get_config())

  let greeter = distribute.named("greeter", codec.string())

  let assert Ok(_gs) =
    distribute.start_registered(greeter, Nil, fn(msg, _state) {
      io.println("hello, " <> msg)
      receiver.Continue(Nil)
    })

  let assert Ok(found) = distribute.lookup(greeter)
  let assert Ok(Nil) = distribute.send(found, "world")
}
```

## 2. Counter with `call`

Stateful actor. The client uses `call` for synchronous read.

```gleam
import distribute
import distribute/codec
import distribute/global
import distribute/receiver
import gleam/erlang/process
import gleam/result

pub type CounterMsg {
  Inc(Int)
  Get(reply: process.Subject(BitArray))
}

fn counter_encoder(msg: CounterMsg) -> Result(BitArray, codec.EncodeError) {
  case msg {
    Inc(n) -> {
      use body <- result.try(codec.int_encoder()(n))
      Ok(<<0, body:bits>>)
    }
    Get(reply) -> {
      use body <- result.try(codec.subject_encoder()(reply))
      Ok(<<1, body:bits>>)
    }
  }
}

fn counter_decoder(data: BitArray) -> Result(CounterMsg, codec.DecodeError) {
  case data {
    <<0, rest:bytes>> -> {
      use n <- result.try(codec.int_decoder()(rest))
      Ok(Inc(n))
    }
    <<1, rest:bytes>> -> {
      use reply <- result.try(codec.subject_decoder()(rest))
      Ok(Get(reply))
    }
    _ -> Error(codec.InvalidBinary("unknown counter msg tag"))
  }
}

fn counter_codec() -> codec.Codec(CounterMsg) {
  codec.Codec(
    encoder: counter_encoder,
    decoder: counter_decoder,
    sized_decoder: fn(_) {
      Error(codec.InvalidBinary(
        "counter codec is top-level only; not designed for composite framing",
      ))
    },
  )
}

pub fn handle(msg: CounterMsg, state: Int) -> receiver.HandlerStep(Int) {
  case msg {
    Inc(n) -> receiver.Continue(state + n)
    Get(reply) -> {
      let _ = global.reply(reply, state, codec.int_encoder())
      receiver.Continue(state)
    }
  }
}

pub fn main() {
  let counter = distribute.named("counter", counter_codec())
  let assert Ok(_) =
    distribute.start_registered(counter, 0, handle)

  let assert Ok(target) = distribute.lookup(counter)
  let assert Ok(Nil) = distribute.send(target, Inc(5))
  let assert Ok(Nil) = distribute.send(target, Inc(3))
  let assert Ok(8) =
    distribute.call(target, Get, codec.int_decoder())
}
```

## 3. Worker pool

Spawn N supervised workers with deterministic names.

```gleam
import distribute
import distribute/codec
import distribute/receiver

pub fn main() {
  let workers = distribute.named("worker", codec.int())

  let assert Ok(_sup) =
    distribute.pool_with_timeout(workers, 4, 0, fn(msg, state) {
      receiver.Continue(state + msg)
    }, 5000)

  // Address worker #3 directly:
  let assert Ok(w3) =
    distribute.lookup(distribute.named("worker_3", codec.int()))
  let assert Ok(Nil) = distribute.send(w3, 42)
}
```

## 4. Versioned protocol

Use `tagged.codec` to refuse messages from peers running a different
schema version. Useful during rolling deploys.

```gleam
import distribute
import distribute/codec
import distribute/codec/tagged
import distribute/receiver

pub fn main() {
  // Only accept messages tagged "auth" v2.
  let auth_codec = tagged.codec("auth", 2, codec.string())
  let auth = distribute.named("auth", auth_codec)

  let assert Ok(_gs) =
    distribute.start_registered(auth, Nil, fn(_msg, _state) {
      // _msg is a TaggedMessage. Extract via tagged.payload.
      receiver.Continue(Nil)
    })
}
```

A peer running v1 sends a binary that decodes to
`Error(VersionMismatch(expected: 2, got: 1))`. The actor stays alive,
the message is dropped.

## 5. Cluster events

Watch nodes joining and leaving.

```gleam
import distribute
import distribute/cluster_monitor
import gleam/erlang/process
import gleam/io

pub fn main() {
  let assert Ok(mon) = distribute.start_monitor()
  let events = process.new_subject()
  distribute.subscribe(mon, events)

  // Block on the next event.
  let assert Ok(event) = process.receive(events, 60_000)
  case event {
    cluster_monitor.NodeUp(name) -> io.println("up: " <> name)
    cluster_monitor.NodeDown(name) -> io.println("down: " <> name)
  }
}
```

## 6. Observability hook

Wire decode errors into your metrics pipeline. Useful when peers may run
different codec versions during a rolling deploy.

The facade exposes `start_actor_observed/4` (local actor, default init
timeout). For the *registered* observed variant, drop down to
  `distribute/actor` directly; it is a low-volume escape hatch and not
```gleam
import distribute
import distribute/actor as dist_actor
import distribute/codec
import distribute/receiver

fn report_decode_error(err: codec.DecodeError) -> Nil {
  // metrics.increment("distribute.decode_error", labels: [
  //   #("kind", codec.decode_error_to_string(err)),
  // ])
  Nil
}

pub fn main() {
  let svc = distribute.named("svc", codec.int())

  let assert Ok(_gs) =
    dist_actor.start_registered_observed(
      svc,
      0,
      fn(_msg, state) { receiver.Continue(state) },
      5000,
      report_decode_error,
    )
}
```

## 7. Isolated call (mailbox-safe under heavy timeouts)

`distribute.call` is fine for short-lived callers, but a late reply that
arrives *after* the function has already returned can still orphan in the
mailbox of a long-running caller. For actors, managers and polling loops that
issue many RPCs under sustained timeouts, use the built-in isolated variant.

```gleam
import distribute
import distribute/codec

let result = distribute.call_isolated(
  target,
  make_req,
  codec.int_decoder(),
)
```

If you need an explicit timeout, use `distribute.call_isolated_with_timeout`.
The implementation already handles the hard part: on caller-side timeout it
kills the proxy, waits for the proxy `DOWN`, and only then returns, so the
timeout path does not reopen the mailbox-race it is supposed to solve.

## 8. Resource cleanup with `start_resource_owner`

`gleam/otp/actor` 1.x does not yet support OTP's
`{terminate, Reason}` callback (tracking issue: gleam-lang/otp#126).
If your actor opens a file, holds an ETS table, owns a TCP socket, or
grabs a distributed lock, you cannot rely on a `terminate/1` callback
firing on external kill. The library ships
`actor.start_resource_owner/3` for this case: it spawns a tiny
observer process that opens the resource, monitors a lifetime PID
(typically the actor), and runs `close(resource)` when the lifetime
PID dies for any reason. The observer is unlinked from your caller,
so a `close` that raises does not propagate out.

```gleam
import distribute
import distribute/actor as dist_actor
import distribute/codec
import distribute/global
import distribute/receiver

pub type DbPool {
  DbPool(handle: Int)
}

pub fn open_pool() -> DbPool {
  // Open your real connection here. The function runs inside the
  // owner process, so the resource is BEAM-process-owned by it.
  DbPool(handle: 1)
}

pub fn close_pool(_p: DbPool) -> Nil {
  // Release the connection. Runs once, when the lifetime PID dies.
  Nil
}

pub fn start_with_resource() {
  let tn = distribute.named("svc", codec.string())
  let assert Ok(gs) =
    distribute.start_registered(tn, Nil, fn(_msg, state) {
      receiver.Continue(state)
    })

  let assert Ok(actor_pid) = global.owner(gs)
  let _owner =
    dist_actor.start_resource_owner(open_pool, close_pool, actor_pid)

  gs
}
```

This pattern works regardless of how the actor exits: handler-returned
`Stop`, `StopAbnormal`, supervisor shutdown, or brutal `process.kill`.
The monitor on a dead PID fires `DOWN` immediately, so calling
`start_resource_owner` after the actor has already crashed is still
safe.

When upstream `gleam/otp` ships `terminate` support, the contract of
this helper does not change: callers can replace the
`start_resource_owner(open, close, pid)` line with the upstream API
and the helper becomes a deprecated thin wrapper.

## 9. Custom record codec

`codec.map` over a tuple is the standard pattern for records. No macros,
but the combinator keeps it short.

```gleam
import distribute/codec
import distribute/codec/composite

pub type User {
  User(id: Int, name: String, active: Bool)
}

pub fn user_codec() -> codec.Codec(User) {
  let triple = composite.tuple3(codec.int(), codec.string(), codec.bool())
  codec.map(
    triple,
    fn(t) {
      let #(id, name, active) = t
      User(id, name, active)
    },
    fn(u) { #(u.id, u.name, u.active) },
  )
}
```
