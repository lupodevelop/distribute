# Changelog

## v3.0.0 — 2026-02-11

Ground-up rewrite. Smaller API, proper OTP actors, compile-time type safety
via `TypedName(msg)`. Not compatible with v2.

### Removed

Everything outside the core: crypto, discovery, settings, groups, monitoring,
connection pool, retry. Also removed `whereis_global(name, encoder, decoder)`.

### Changed

- Actors are real `gen_statem` via `gleam/otp/actor` — `observer`,
  `sys:get_status`, supervision trees all work.
- Registry uses binary names with `:global`. No atoms created, no atom
  table exhaustion.
- `start`, `start_registered`, `start_supervised`, `pool` all take
  `TypedName` instead of separate name + encoder + decoder.
- `register` and `lookup` take `TypedName`. The compiler enforces that
  both sides use the same `msg` type.
- Each `GlobalSubject` gets a unique or deterministic tag (was shared
  `Nil` in v2 — caused message mixing).
- Orphaned actors are killed on registration failure in `start_supervised`.

### Added

- `TypedName(msg)` — opaque type binding a name to an encoder/decoder pair.
- `Codec(a)` — bundles encoder + decoder + sized decoder. Shorthand
  constructors: `int()`, `string()`, `float()`, `bool()`, `bitarray()`,
  `nil()`, `list(c)`.
- `codec.map(c, wrap, unwrap)` — derive a codec for a custom type.
- `codec.subject()` — serialize a `Subject(BitArray)` for cross-node
  request/response.
- `global.call(target, make_request, response_decoder, timeout)` —
  synchronous request/response across nodes.
- `global.reply(reply_to, response, encoder)` — send a response back.
- `composite.option(c)`, `composite.result(ok, err)`,
  `composite.tuple2(a, b)`, `composite.tuple3(a, b, c)`.
- `registry.named(name, codec)` — short form of `typed_name`.