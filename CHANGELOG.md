# CHANGELOG

All notable changes for major releases of the project.

---

## v2.1.0 — 2025-12-28

**Author:** lupodevelop — Scaratti Daniele

### Overview
This release introduces **capability negotiation** and **protocol versioning** as foundational features for distributed type-safe communication. It enables nodes to exchange capabilities during handshake, negotiate protocol versions, and use the appropriate encoder/decoder based on negotiated versions. This is a critical step toward supporting rolling upgrades and heterogeneous clusters.

### Key highlights
- **Capability negotiation**: Nodes exchange `NodeCapabilities` during handshake and negotiate compatible protocol versions.
- **Protocol versioning APIs**:
  - `protocol_negotiate(local_caps, remote_caps, protocol) -> Option(Int)` — finds the highest compatible version for a protocol
  - `schema_encode_for_node(schema, value, node) -> Result(BitArray, EncodeError)` — encodes using the negotiated version for a specific node
  - `schema_decode_from_node(schema, binary, node) -> Result(a, DecodeError)` — decodes using the negotiated version
- **Registry integration**: Handshake actors automatically store negotiated metadata in the registry for lookup during message encoding/decoding.
- **Handshake state machine**: Complete flow with Hello → Capabilities → Accept/Reject → Established, including registry-based validation for responder.
- **Crypto provider behaviour**: Defined pluggable `crypto.Provider` trait with states (Plain, KeyExchangeInProgress, SecureEstablished, Rekeying, Failed) and stub implementation for development.
- **Validation helpers**: `validate_capabilities(caps)` ensures capability definitions are well-formed (min ≤ max, non-empty protocol names).
- **Comprehensive test coverage**: Unit tests for negotiation logic (compatible/incompatible ranges, missing protocols), integration tests for handshake ↔ registry ↔ negotiation flow.

### Breaking changes
- **No breaking changes**: This release is fully backward compatible with v2.0.0. New APIs are additive.

### Migration notes (short)
1. Define `NodeCapabilities` for your protocols (e.g., `[Capability("my_proto", 1, 3)]`).
2. Use `schema_encode_for_node` instead of `schema_encode` when communicating with specific nodes to ensure version compatibility.
3. Start handshake actors with registry integration: `start_initiator_handshake(..., registry: Some(registry_subject))`.
4. Optionally implement a custom crypto provider by adhering to the `crypto.Provider` behaviour (see documentation).

### Acknowledgement
This release builds on the type-safe foundation established in v2.0.0 and addresses the need for versioned protocols in distributed systems.

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