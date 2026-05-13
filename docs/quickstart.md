# Quickstart

Five minutes from `gleam new` to typed cross-node messages.

## 1. Install

```sh
gleam add distribute
```

Requires Gleam 1.16 or newer. Runs on the Erlang target only.

## 2. Configure once

At application boot:

```gleam
import distribute
import distribute/config

pub fn boot() {
  let assert Ok(Nil) =
    distribute.configure(config.Config(
      ..config.default(),
      default_call_timeout_ms: 5_000,
      default_init_timeout_ms: 5_000,
      max_payload_size_bytes: 1 * 1024 * 1024,
    ))
  Nil
}
```

`configure` is single-write. The second call returns
`Error(AlreadyConfigured)`. Call it from your top-level supervisor.

## 3. Define a protocol

A `TypedName(msg)` binds a name string to a codec. Share the value
across both ends.

```gleam
import distribute
import distribute/codec

pub fn greeter() {
  distribute.named("greeter", codec.string())
}
```

## 4. Start a registered actor

```gleam
import distribute
import distribute/receiver
import gleam/io

pub fn start() {
  let assert Ok(_gs) =
    distribute.start_registered(greeter(), Nil, fn(msg, _state) {
      io.println("got: " <> msg)
      receiver.Continue(Nil)
    })
  Nil
}
```

## 5. Send from any node

```gleam
import distribute

pub fn send(message: String) {
  let assert Ok(target) = distribute.lookup(greeter())
  let assert Ok(Nil) = distribute.send(target, message)
}
```

That is the entire happy path. To go distributed:

```gleam
import distribute

pub fn join_cluster() {
  let assert Ok(Nil) =
    distribute.start_node("myapp@127.0.0.1", "shared-secret-cookie")
  let _ = distribute.connect("peer@127.0.0.1")
  Nil
}
```

## Next steps

- [Recipes](./recipes.md). Copy-pasteable patterns: call, pool,
  versioned protocol, cluster events, observability.
- [Actors and registry](./actors_and_registry.md). Start variants,
  supervision, pools.
- [Messaging](./messaging.md). `send` / `receive` / `call`, fast-fail,
  late-reply draining.
- [Codecs and types](./codecs_and_types.md). Built-ins, `map`, tagged
  messages, wire format.
- [Safety and limits](./safety_and_limits.md). Payload limits, atom
  table safety, threat model, recommended sizing.
