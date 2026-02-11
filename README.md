# distribute

<p align="center">
  <img src="assets/img/distribute.png" alt="distribute logo" width="200" />
</p>

Typed distributed messaging for Gleam on the BEAM.

[![Package Version](https://img.shields.io/hexpm/v/distribute)](https://hex.pm/packages/distribute)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/distribute/)

## What this is

Erlang already gives you distribution, but from Gleam you lose type info at
the node boundary — everything crosses the wire as raw terms. `distribute`
puts binary codecs in front of `:global` and `Subject` so the compiler can
catch mismatches before messages leave the process.

"Typed" here means checked at encode/decode boundaries. There is no shared
type system across nodes — the BEAM doesn't work that way.

## Install

```sh
gleam add distribute
```

## Usage

### Fire-and-forget

Define a `TypedName(msg)` that pairs a name with a codec, then use it on
both sides. The compiler won't let you register a `String` actor and look
it up as `Int`.

```gleam
import distribute
import distribute/codec
import distribute/registry
import distribute/receiver

// one TypedName, shared across the codebase
let greeter = registry.named("greeter", codec.string())

// start + register
let assert Ok(gs) = distribute.start_actor(greeter, Nil, fn(msg, _state) {
  io.println("Got: " <> msg)
  receiver.Continue(Nil)
})
let assert Ok(Nil) = distribute.register(greeter, gs)

// from any node
let assert Ok(remote) = distribute.lookup(greeter)
let assert Ok(Nil) = distribute.send(remote, "hello")
```

### Request / response

Include a reply `Subject(BitArray)` in your message type and use
`codec.subject()` to serialize it. The `Subject` carries node info, so
replies route back across nodes automatically.

```gleam
import distribute/codec
import distribute/global
import distribute/receiver
import gleam/erlang/process

type CounterMsg {
  Inc(Int)
  Get(reply: process.Subject(BitArray))
}

// handler side
fn handle(msg, state) {
  case msg {
    Inc(n) -> receiver.Continue(state + n)
    Get(reply) -> {
      let _ = global.reply(reply, state, codec.int_encoder())
      receiver.Continue(state)
    }
  }
}

// caller side
let assert Ok(count) = global.call(counter, Get, codec.int_decoder(), 5000)
```

`global.call` creates a temporary subject, sends the request, waits for
the response, decodes it. Same idea as `gen_server:call`.

### Codecs

Primitives: `codec.int()`, `codec.string()`, `codec.float()`, `codec.bool()`,
`codec.bitarray()`, `codec.nil()`.

Composites: `codec.list(c)`, `codec.subject()`, `codec.map(c, wrap, unwrap)`,
`composite.option(c)`, `composite.result(ok, err)`,
`composite.tuple2(a, b)`, `composite.tuple3(a, b, c)`.

For your own types, use `codec.map`:

```gleam
type UserId { UserId(Int) }

let user_id_codec = codec.map(codec.int(), UserId, fn(uid) {
  let UserId(n) = uid
  n
})
```

Gleam has no derive macros or reflection, so codecs for complex types
are manual. The combinators handle the serialization — you just wire
the fields together.

## Modules

| Module | Does |
|--------|------|
| `distribute` | Facade — start node, connect, send, lookup |
| `distribute/actor` | Named actors, supervision, pools |
| `distribute/cluster` | `net_kernel` start/connect/ping |
| `distribute/codec` | Binary codecs for primitives + `subject()` |
| `distribute/codec/composite` | Option, Result, Tuple codecs |
| `distribute/codec/tagged` | Tagged messages with version field |
| `distribute/global` | `GlobalSubject(msg)`, `call`, `reply` |
| `distribute/registry` | `TypedName(msg)`, `:global` registration |
| `distribute/receiver` | Typed receive, OTP actor wrappers |

## Caveats

**What the types catch** — within one codebase, `TypedName` and
`GlobalSubject` prevent mixing up message types at compile time.

**What they don't** — two separate codebases using different codecs for
the same name. The codec will reject the binary at runtime, not at compile
time. Same for Erlang code sending raw terms to a `distribute` actor.

**Subject construction** — Gleam's `Subject` is opaque. To build one from
a remote PID and a deterministic tag (how registry lookup works), we
construct the `{subject, Pid, Tag}` tuple in
[one Erlang function](src/distribute_ffi_utils.erl). If `gleam_erlang`
changes the internal representation, that single function needs updating.

**No auto-derive** — Gleam doesn't have macros. Complex message codecs
are manual. The combinators (`map`, `list`, `option`, `tuple2`, etc.)
keep it manageable, but it's not zero-boilerplate.

## Development

```sh
gleam test
gleam docs build
```
