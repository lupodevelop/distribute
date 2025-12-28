/// The `distribute` library provides **type-safe** distributed computing primitives for Gleam.
///
/// ## Type-Safety Enforcement (v2.0+)
///
/// All messaging APIs now enforce compile-time type safety using:
/// - **GlobalSubject(msg)**: Opaque wrapper with encoder/decoder for type-safe messaging
/// - **Binary codecs**: Encoder(a)/Decoder(a) for serialization
/// - **gleam_otp integration**: Compatible with standard `Subject(BitArray)`
///
/// ## Migration from v1.x
///
/// Legacy untyped functions are deprecated:
/// - Use `send_global_typed` instead of `send_global`
/// - Use `whereis_global` instead of `whereis_typed` for GlobalSubject
/// - Use `whereis_with_tag` for custom actors with known tags
///
/// ## Core Modules
///
/// - `distribute/global`: GlobalSubject type for type-safe distributed messaging
/// - `distribute/codec`: Binary serialization with composable encoders/decoders
/// - `distribute/cluster`: Cluster formation and management
/// - `distribute/connection_pool`: Efficient connection pooling
/// - `distribute/groups`: Process groups (pg) with type-safe broadcast
/// - `distribute/messaging`: Inter-node type-safe messaging
/// - `distribute/monitor`: Node and process monitoring
/// - `distribute/node_builder`: Fluent API for node configuration
/// - `distribute/registry`: Global process registry with type-safe lookup
/// - `distribute/remote_call`: Type-safe RPC mechanisms
/// - `distribute/receiver`: Type-safe message receiving with gleam_otp selectors
/// - `distribute/settings`: Configuration settings
///
pub fn version() {
  "2.0.0"
}
