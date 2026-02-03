# CHANGELOG

All notable changes for major releases of the project.

---

## v2.1.0 — 2026-02-01

**Author:** lupodevelop — Scaratti Daniele

### Overview

This release introduces **capability negotiation**, **protocol versioning**, **production-ready crypto**, and **intelligent retry logic** as foundational features for distributed type-safe communication. It enables nodes to exchange capabilities during handshake, negotiate protocol versions, and use the appropriate encoder/decoder based on negotiated versions. This is a critical step toward supporting rolling upgrades and heterogeneous clusters.

### Key Features

#### 🔐 Crypto Adapter (otp_crypto_adapter)
- **Production-ready OTP crypto adapter** using Erlang's `:crypto` module (OTP 22+)
- **Algorithms**:
  - **Key Exchange:** X25519 (Curve25519 ECDH)
  - **AEAD Encryption:** ChaCha20-Poly1305 (RFC 8439)
  - **Key Derivation:** HKDF-SHA256
  - **Random:** OTP `crypto:strong_rand_bytes`
- **Technical details**: 12-byte nonces (RFC 8439), 16-byte auth tags, no external dependencies
- ChaCha20-Poly1305 chosen over AES-GCM (equivalent security, no AES-NI dependency)
- **Stub**: `sodium_adapter` scaffold reserved for future libsodium implementation (NIF/port)

#### 🔄 Retry Module (distribute/retry)
- **New module `distribute/retry`**: Complete retry policy implementation with exponential backoff
- **Jitter strategies**: `NoJitter`, `FullJitter` (recommended), `EqualJitter`, `DecorrelatedJitter`
- **Fluent API**: Chain builders like `with_max_attempts()`, `with_base_delay_ms()`, `with_jitter()`
- **Preset policies**: `default()`, `default_with_jitter()`, `aggressive()`, `conservative()`, `no_retry()`
- **Observability**: `DelayResult` type includes metadata (attempt, base_delay, is_final_attempt)
- Based on AWS/Google Cloud best practices for distributed systems

#### 🤝 Capability Negotiation & Protocol Versioning
- Nodes exchange `NodeCapabilities` during handshake and negotiate compatible protocol versions
- **Protocol versioning APIs**:
  - `protocol_negotiate(local_caps, remote_caps, protocol) -> Option(Int)`
  - `schema_encode_for_node(schema, value, node) -> Result(BitArray, EncodeError)`
  - `schema_decode_from_node(schema, binary, node) -> Result(a, DecodeError)`
- **Registry integration**: Handshake actors automatically store negotiated metadata
- **Handshake state machine**: Hello → Capabilities → Accept/Reject → Established
- **Validation helpers**: `validate_capabilities(caps)` ensures well-formed definitions

#### 🎭 Actor Module Improvements
- Maintained v2.0.0 compatibility for legacy functions `actor.start` and `actor.start_global`
- **Type-safe helpers**: `actor.start_typed_actor`, `actor.start_server` returning `GlobalSubject(msg)`
- **Supervision helpers**: `actor.child_spec_typed_actor`, `actor.child_spec_server`
- **Convenience helpers**:
  - `actor.start_typed_actor_registered(name, ...)` — actor start + global registration
  - `actor.start_typed_actor_started(...)` — returns `actor.Started(GlobalSubject(msg))`
  - `actor.child_spec_typed_actor_typed(...)` — fully typed child spec
- **High-level supervision**:
  - `actor.start_typed_actor_supervised(...)` — starts actor under supervisor
  - `actor.pool_supervisor(pool_size, ...)` — creates N worker actors for load balancing
- Advanced examples: SWIM and Raft integration patterns (see `examples/advanced_patterns/`)

#### 📦 Registry Improvements
- `register_global(global_subject, name)` — register GlobalSubject directly
- `register_with_strategy(subject, name, policy)` — registration with retry policy
- `lookup_global(name, encoder, decoder)` — convenience alias for `whereis_global`
- `lookup_with_timeout(name, encoder, decoder, timeout, poll_interval)` — blocking lookup
- `is_registered(name)` — efficient existence check
- `unregister_and_remove(name)` — cleanup global registry and persistent_term

### Bugfixes
- **Receiver EXIT handling**: Global receiver now properly handles linked process EXIT messages using `process.trap_exits(True)` and `select_trapped_exits`
- **FFI atom safety**: All FFI modules use safe atom conversion via `distribute_ffi_utils:to_atom_safe/1`

### Migration from 2.0.0

```gleam
// Retry with strategy (new recommended API)
import distribute/retry
let policy = retry.default_with_jitter()
registry.register_with_strategy(subject, "my-service", policy)

// Capability negotiation
let caps = [Capability("my_proto", 1, 3)]
schema_encode_for_node(schema, value, target_node)
```

### Breaking Changes
- **No breaking changes**: Fully backward compatible with v2.0.0

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