# Migration Guide: v1.x → v2.0 (Type-safe APIs)

This guide explains how to migrate from the untyped APIs (v1.x) to the type-safe APIs introduced in v2.0. It is written to match the current implementation and helper functions in the repository.

## Summary of what changed

- Type-safe messaging APIs (`send_typed`, `call_typed`, `broadcast_typed`) using binary codecs and `process.Subject(BitArray)` for transported messages.
- Receiver helpers to simplify decoding (`receive_typed`, `selecting_typed`, `start_typed_receiver`, `start_global_receiver`).
- A schema and migration system for envelope tag + version and helper functions for migrating payloads between versions.
- Legacy untyped APIs remain available but are marked deprecated; prefer typed APIs.

## Breaking changes (short)

- Use `gleam/erlang/process.Subject(BitArray)`.
- Registry and group APIs now expect `Subject(BitArray)` for typed subjects (global subjects use a `Nil` tag for compatibility).
- Untyped messaging and RPC functions are deprecated; use the `_typed` variants that accept codecs and return explicit `Result` values.

## Step-by-step migration

### 1) Add or update dependencies

In `gleam.toml`:
```toml
[dependencies]
gleam_stdlib = ">= 0.43.0"
gleam_erlang = ">= 0.5.0"
gleam_otp = ">= 0.1.0"
distribute = "~> 2.0"
```

### 2) Replace custom imports with standard ones

- Add `import gleam/erlang/process`, `import distribute/codec`, `import distribute/receiver`

### 3) Replace untyped sends with typed sends

Old (deprecated):
```gleam
messaging.send(pid, value)
messaging.send_global("name", value)
```

New:
```gleam
let encoder = my_type_encoder()
case messaging.send_typed(subject, value, encoder) {
  Ok(_) -> // success
  Error(err) -> // handle SendError
}

case messaging.send_global_typed("name", value, encoder) {
  Ok(_) -> // success
  Error(err) -> // handle SendError
}
```

Notes:
- Typed functions encode to `BitArray` using your `Encoder(a)` implementation.
- `send_typed` takes a `process.Subject(BitArray)` value; create one via `process.new_subject()` or get from registry.

### 4) Replace manual receive + decode with receiver helpers

Old (manual):
```gleam
let binary = process.receive(subject, timeout)
// manual decode logic
```

New:
```gleam
let decoder = my_type_decoder()
case receiver.receive_typed(subject, decoder, timeout) {
  Ok(value) -> // decoded value
  Error(receiver.Timeout) -> // handle
  Error(receiver.DecodeError(err)) -> // handle
}
```

Convenience helpers:
- `selecting_typed` to add typed handlers to an existing `Selector`.
- `start_typed_receiver` starts an actor that decodes messages and runs a typed handler.
- `start_global_receiver` starts a receiver compatible with `registry.register_typed` (uses a `Nil` tag).

### 5) Registry and groups

- Use `registry.register_typed(name, subject)` and `registry.whereis_typed(name)` to publish and lookup typed global subjects.
- For groups, use `groups.join_typed(group, subject)` and `groups.broadcast_typed(group, message, encoder)`.

Important: global subjects use a `Nil` tag internally; regular actor subjects (random tag) will not receive global messages unless started with `start_global_receiver` or given a `Nil` tag.

### 6) RPC (remote calls)

Replace untyped `remote_call.call` with `remote_call.call_typed` or `call_typed_with_timeout`, passing encoders/decoders for arguments and results.

### 7) Schema and migrations

- If you need schema evolution, use `Schema(a)` and the provided migration helpers (`build_migration_chain`, `build_migration_graph`) to encode/decode across versions.
- Use `schema_encode_for_node` when sending to a node with a different negotiated version.

## Creating codecs

- Use the built-in primitive codecs and combinators in `distribute/codec`.
- For custom types, implement `Encoder` and `Decoder` for your type and prefer the `SizedDecoder` helpers for variable-length fields.

## Testing and verification

- Run `gleam check` to ensure your code compiles.
- Run `gleam test` to exercise unit tests.
- Run the examples in `examples/two_nodes/` to validate multi-node behavior.
- Check for uses of deprecated APIs (`@deprecated` annotations show intent); migrate usages until no warnings remain.

## Common pitfalls and notes

- Do not mix untyped and typed messaging patterns in a code path; they use different formats.
- Always handle decode/encode errors returned as `Result` instead of assuming success.
- When registering global subjects, prefer `start_global_receiver` so the subject uses the `Nil` tag and works with `registry.register_typed`.

## Actor API deprecations

To prepare for the v3.0.0 release we are deprecating a small set of low-level
actor APIs that were historically used to obtain raw `Subject(BitArray)` values
and to start untyped actors. These APIs will be removed in v3.0.0.

Deprecated APIs:
- `actor.start(initial_state, decoder, handler)` — use `actor.start_typed_actor`
  and obtain the raw subject via `global.subject` or `global.from_subject` if
  needed.
- `actor.start_global(initial_state, decoder, handler)` — use
  `actor.start_typed_actor` + `global.from_subject` or register the typed
  subject with `registry.register_typed`.

Migration examples:

1) Replace `start` with `start_typed_actor` and extract subject:

```gleam
// Old (deprecated)
let assert Ok(subject) = actor.start(0, my_decoder(), my_handler)
// New (typed)
let gs = actor.start_typed_actor(0, my_encoder(), my_decoder(), my_handler)
let subject = global.subject(gs)
```

2) Replace `start_global` with `start_typed_actor` and register:

```gleam
// Old (deprecated)
let subject = actor.start_global(0, my_decoder(), my_handler)
// New (typed)
let gs = actor.start_typed_actor(0, my_encoder(), my_decoder(), my_handler)
registry.register_typed("my_name", gs)
```

3) Supervisors: `child_spec_*` helpers now return `ChildSpecification(Subject(BitArray))`.
   If you prefer a typed wrapper after the child started, call `global.from_subject`
   with the returned subject and codecs in the parent process when appropriate.

## Where to get help

- Examples: `examples/typed_messaging/` and `examples/two_nodes/`.
- API docs: run `gleam docs build`.
- Report issues on GitHub or ask on Gleam community channels.

---

*This document has been updated to reflect the current implementation and helper functions in the codebase.*
