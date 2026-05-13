<!-- markdownlint-disable MD033 MD060 -->

# distribute

<p align="center">
  <img src="https://raw.githubusercontent.com/lupodevelop/distribute/d29c993/assets/img/distribute.png" alt="distribute logo" width="200" />
</p>

Typed distributed messaging for Gleam on the BEAM.

[![Package Version](https://img.shields.io/hexpm/v/distribute)](https://hex.pm/packages/distribute)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/distribute/)

`distribute` is a thin typed safety layer over Erlang's distribution
primitives. It puts binary codecs in front of `:global` and `Subject` so
the compiler catches protocol mismatches before messages leave the
process. Payload size and dead-target detection are enforced at every
I/O boundary, so a misbehaving peer cannot OOM your node or hang it.

## Install

```sh
gleam add distribute
```

Requires Gleam 1.16 or newer, Erlang targetÉ only.

## 30-second taste

```gleam
import distribute
import distribute/codec
import distribute/receiver

let greeter = distribute.named("greeter", codec.string())

let assert Ok(_gs) =
  distribute.start_registered(greeter, Nil, fn(msg, _state) {
    io.println("got: " <> msg)
    receiver.Continue(Nil)
  })

let assert Ok(target) = distribute.lookup(greeter)
let assert Ok(Nil) = distribute.send(target, "hello")
```

For the full walkthrough see [docs/quickstart.md](./docs/quickstart.md).

## Documentation

All long-form docs live in [`docs/`](./docs/README.md):

- [Quickstart](./docs/quickstart.md). Boot, configure, send.
- [Recipes](./docs/recipes.md). Counter, pool, versioned protocol,
  cluster events, custom records.
- [Actors and registry](./docs/actors_and_registry.md).
- [Messaging](./docs/messaging.md). `send` / `receive` / `call`.
- [Codecs and types](./docs/codecs_and_types.md). Wire format, custom
  records, tagged messages.
- [Safety and limits](./docs/safety_and_limits.md). Payload limits,
  threat model, recommended sizing.

## What you get

- **Typed boundary.** `TypedName(msg)` binds a name to a codec.
  Registration and lookup share the same `msg` type, and the compiler
  rejects mismatches.
- **Hard payload caps.** `max_payload_size_bytes` is enforced before
  encode and before decode on every path: `send`, `receive`, `call`,
  `reply`, actor handlers, selectors.
- **Fast-fail calls.** `global.call` monitors the target. A dead target
  returns `Error(TargetDown)` immediately. Late replies and DOWN
  messages are drained from the caller's mailbox.
- **OTP-native.** Real OTP gen_server-flavored actors via
  `gleam_otp/actor`, real supervisors, real child specs. `observer`,
  `sys:get_status`, restart strategies all work. No magic.
- **Single-source codec.** Encoding and decoding happen through one
  `Codec(a)` value. Combinators (`map`, `list`, `option`, `tuple`,
  `tagged`) cover the common cases without macros.

### Custom Type Codecs

Seamlessly encode and decode your Algebraic Data Types (enums) with a fluent builder.

```gleam
pub type MyMessage {
  Text(String)
  Ping
}

import distribute/codec
import distribute/codec/variant

let my_codec = 
  variant.new()
  |> variant.add(0, "Text", codec.string(), Text, fn(m) {
    case m { Text(s) -> Ok(s); _ -> Error(Nil) }
  })
  |> variant.unit(1, "Ping", Ping, fn(m) { m == Ping })
  |> variant.build()
```

### Cluster Monitoring

Subscribe to cluster events (`NodeUp`, `NodeDown`) to react to node topology changes.

```gleam
import distribute
import distribute/cluster/monitor

let subj = process.new_subject()
let assert Ok(m) = distribute.subscribe(subj)

// In your actor/process
case process.receive(subj, 5000) {
  Ok(monitor.NodeUp(node)) -> io.println("Node joined: " <> node)
  Ok(monitor.NodeDown(node)) -> io.println("Node left: " <> node)
  _ -> Nil
}

// Later
distribute.unsubscribe(m)
```

## Caveats

- **Two codebases, one wire.** `TypedName` enforces type safety inside
  one codebase. Mismatched codecs across separate codebases produce
  runtime decode errors, not compile errors.
- **No auto-derive.** Gleam has no macros, so complex codecs are
  manual. The combinators keep them short.
- **`terminate` caveat in `gleam_otp/actor` 1.x.** External shutdown
  paths do not currently invoke an OTP-style `terminate` callback. If
  an actor owns files, sockets, ETS tables, ports, or other external
  resources, use the linked resource-owner pattern documented in
  [docs/recipes.md](./docs/recipes.md) and
  [docs/safety_and_limits.md](./docs/safety_and_limits.md).
- **One internal coupling.** We construct `Subject` from a remote PID
  via [one Erlang FFI function](src/distribute_ffi_utils.erl). That is
  the single point that depends on `gleam_erlang`'s subject layout.

## Development

```sh
gleam test          # full suite (includes mandatory real-cluster Z2/Z3)
epmd -daemon        # start Erlang distribution daemon if not already running
gleam dev           # multi-node playground
gleam docs build    # local API docs
```

## License

MIT. See [LICENSE](./LICENSE).
