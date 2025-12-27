/// Registry behaviour definition and types.
///
/// This module defines the strongly-typed contract used by registry
/// adapters. Adapters (for example `registry_actor`) should implement the
/// semantics described here. Keep this module lightweight: it provides types
/// and documentation so callers and adapters can remain decoupled
/// and fully type-safe.
///
/// The registry stores a small piece of node metadata for each known node.
/// The metadata is intentionally generic so it can evolve without breaking
/// adapters that implement the behaviour.
import distribute/handshake/capability

/// Node identifier used across the system. We keep it as `String` to match
/// existing APIs (node names like "node@host").
pub type NodeId =
  String

/// Minimal metadata the registry stores per-node.
pub type Metadata {
  Metadata(
    node_id: NodeId,
    capabilities: List(capability.Capability),
    // Free-form JSON or string blob for adapter-specific extensions.
    extra: String,
  )
}

/// Errors returned by registry operations.
pub type RegistryError {
  NotFound
  AlreadyExists
  InvalidArgument(String)
  AdapterFailure(String)
}

// NOTE: We intentionally do not enforce a language-level behaviour trait
// because Gleam's extensibility and runtime adapters are commonly expressed
// as modules exposing the documented functions. This module standardises the
// types used across adapters and documents the contract clearly.

/// Behaviour contract (documented):
///
/// - `start/0` or `start_link/0` should create a new registry adapter
///   instance and return a `process.Subject`/`Pid` or an error.
/// - `register(node_id, metadata)` stores/updates metadata for `node_id`.
/// - `unregister(node_id)` removes `node_id` from the registry.
/// - `lookup(node_id)` returns `Ok(metadata)` when present otherwise `Error(NotFound)`.
/// - `list()` returns the list of known `NodeId` values.
///
/// Adapters are free to expose richer APIs but must at minimum provide the
/// semantics above. The handshake layer will depend only on `lookup` and
/// `register` behaviour to persist negotiated protocol metadata.
/// Example (adapter should provide these functions):
///
/// Registry behaviour: contract for pluggable membership/metadata registries.
///
/// This module declares the strongly-typed API the core expects from a
/// registry implementation. Adapters (actor-based, ETS-based, remote, etc.)
/// should implement these functions. The design keeps the core decoupled
/// (handshake and negotiation call the behaviour) so adapters can be
/// swapped without changing higher-level logic.
import gleam/option.{type Option, None}

/// Opaque state for registry implementations. Adapters may represent this
/// however they like (actor state, ETS reference, etc.).
pub type RegistryState {
  RegistryState
}

/// Initialize a fresh registry state. Implementations may accept config
/// via environment or separate constructors; the core can call `init()` when
/// creating an adapter instance.
pub fn init() -> RegistryState {
  RegistryState
}

/// Register a node with associated metadata.
/// Returns the updated state and `Ok(Nil)` on success or `Error(AdapterFailure(...))`.
pub fn register(
  state: RegistryState,
  _node_id: NodeId,
  _metadata: Metadata,
) -> #(RegistryState, Result(Nil, RegistryError)) {
  #(state, Error(AdapterFailure("not implemented")))
}

/// Unregister a node.
pub fn unregister(
  state: RegistryState,
  _node_id: NodeId,
) -> #(RegistryState, Result(Nil, RegistryError)) {
  #(state, Error(AdapterFailure("not implemented")))
}

/// Lookup node metadata. Returns `Some(metadata)` if present or `None`.
pub fn lookup(
  state: RegistryState,
  _node_id: NodeId,
) -> #(RegistryState, Option(Metadata)) {
  #(state, None)
}

/// Return the list of known node ids.
pub fn list_nodes(state: RegistryState) -> #(RegistryState, List(String)) {
  #(state, [])
}
