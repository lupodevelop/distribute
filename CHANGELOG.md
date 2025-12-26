# CHANGELOG

All notable changes for major releases of the project.

---

## v2.0.0 — 2025-12-27

**Author:** lupodevelop — Scaratti Daniele

### Overview
This release is a significant rewrite and consolidation aimed at making the `distribute` library the reference implementation for *type-safe* messaging and distributed communication in Gleam. It improves protocol safety, message versioning, and introduces primitives to support future evolution (migrations, schema versioning, discovery).

### Key highlights
- **Type-safe messaging**: `send_typed`, `call_typed`, `broadcast_typed` APIs using `Encoder(a)` / `Decoder(a)` with envelope tag+version.
- **SizedDecoder (byte-tracking decoders)**: enables correct composition of decoders for variable-length types (e.g., strings, lists).
- **Complete `list_decoder` implementation**: built on top of `SizedDecoder`.
- **Schema system**: `Schema(a)` with `tag`, `version`, `schema_encode`, `schema_decode`, `peek_tag` and `versioned_decoder` to support compatible protocol evolution.
- **Migration helpers**:
  - `build_migration_chain` for sequential v→v+1 steps
  - `build_migration_graph` with pathfinding (DFS) for arbitrary version jumps
- **Explicit error variants**: new `DecodeError` variants (TagMismatch, VersionMismatch, MigrationMissing, MigrationFailed) with clearer messages.
- **Receiver / Actor helpers**: helpers for receiving typed messages (integrated with `gleam/otp` `Selector`) and common actor patterns (server/pool).
- **gleam_otp integration**: `process.Subject(msg)` is used as the base type for typed subjects.
- **Clear deprecations**: legacy untyped functions are marked deprecated with clear warnings and documentation (e.g., `send`, `broadcast` without codecs).
- **Examples & tests**: updated end-to-end examples and extended test coverage.

### Breaking changes
- **Removed custom `Subject(a)`**: replaced with `gleam/otp/process.Subject(msg)`. Users must update types accordingly.
- **`registry` API changes**: `whereis_typed` / `register_typed` now operate with `Subject(msg)` (not `Pid`).
- **Untyped APIs deprecated**: legacy functions remain available but are deprecated; they will be removed in a future major release (v3.0.0).
- **Major version bump**: version bumped from `1.x` to `2.0.0` to reflect breaking changes.

### Migration notes (short)
1. Replace custom `Subject` usages with `process.Subject(msg)` and update any module signatures that register or look up global subjects.
2. Use `send_typed` / `call_typed` / `broadcast_typed` and provide `Encoder` / `Decoder` for exchanged types.
3. Ensure messages are wrapped with correct `tag` and `version` (use `schema_encode_for_node` when communicating with nodes advertising different capabilities).

### Acknowledgement
This release addresses issue #3 reported by Louis Pilfold aka @lpil (https://github.com/lpil). See https://github.com/lupodevelop/distribute/issues/3 for details.

---