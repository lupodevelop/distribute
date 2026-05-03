//// `distribute` -- Typed messaging for the BEAM.
////
//// This library provides a safety layer over Erlang's native distribution.
//// It focuses on hardening the **Node Boundary**: the physical edge where
//// typed Gleam values become raw binary terms and vice versa.
////
//// ## The Typed Boundary Concept
////
//// In standard Erlang distribution, messages cross the wire as raw terms
//// without type information. `distribute` forces you to define a protocol
//// using `TypedName` and `Codec` before any data is sent.
////
//// 1. **At the Sender**: messages are encoded, checked against payload
////    limits, and transmitted via BEAM distribution.
//// 2. **At the Receiver**: binaries have their size verified *before*
////    decoding. If they exceed limits, they are rejected. If they are
////    valid, they are decoded into typed Gleam values.
////
//// This architecture ensures that an actor's mailbox is never flooded
//// with unparseable or oversized data, and the compiler can prevent
//// protocol mismatches at the call site.
////
//// ## Core Philosophy
////
//// - **Fail-Fast over Retry**: every failure (Timeout, TargetDown,
////   DecodeError) is surfaced immediately. Managing retries and circuit
////   breaking is left to the application layer.
//// - **Memory First**: payload limits are enforced at the I/O boundary
////   to protect nodes from OOM attacks or bugs.
//// - **OTP Native**: every part of the library (supervisors, monitors,
////   subjects) follows standard BEAM OTP patterns.
//// - **Zero Cost Read**: configuration is stored in Erlang's
////   `persistent_term` for O(1) reads with zero heap allocation.
////
//// ## Typical Usage
////
//// ```gleam
//// // 1. Configure once at application startup
//// let assert Ok(Nil) = distribute.configure(config.default())
////
//// // 2. Define a protocol (Name + Codec)
//// let counter = distribute.named("counter", codec.int())
////
//// // 3. Start a globally registered, supervised singleton
//// let assert Ok(pid) = distribute.start_supervised(counter, 0, handler)
////
//// // 4. Call from any node -- monitor-based TargetDown detection,
////    default timeout from config
//// let assert Ok(val) = distribute.call(gs, Get, codec.int_decoder())
//// ```
////
//// ## Default vs explicit timeout
////
//// Every long-running operation has two shapes:
////
//// - The **default form** reads from `config.get()` -- this is the path
////   you want 90% of the time. A single `configure(...)` at boot tunes
////   the whole surface (`call`, `receive`, `start_actor`,
////   `start_registered`, `start_supervised`, `pool`, `child_spec`,
////   `call_isolated`).
//// - The **`_with_timeout` form** is the explicit override -- pick it
////   only when the request has a hard deadline that diverges from
////   `config.default_*`.
////
//// ## Error handling without extra imports
////
//// Every public error type (and its `*_to_string` formatter) is
//// re-exported here. Pattern-matching on `CallError`, `RegisterError`,
//// `LookupError`, etc., requires only `import distribute`.
////
//// ## Reply-Subject helpers live in `distribute/receiver`
////
//// Handlers that answer a `call` typically need `receiver.receive_typed`
//// or `receiver.selecting_typed` to operate on the raw reply-Subject.
//// These are intra-handler primitives: import the module directly.
////
//// ```gleam
//// import distribute/receiver
//// case receiver.receive_typed(reply_to, codec.int_decoder(), 1000) {
////   Ok(value) -> ...
////   Error(receiver.ReceiveTimeout) -> ...
////   Error(receiver.DecodeError(_)) -> ...
//// }
//// ```
////
//// ## API surface, by concern
////
//// - **Boot & config**: `configure`, `get_config`, `version`, `Config`,
////   `ConfigError`, `config_error_to_string`.
//// - **Node lifecycle**: `start_node`, `connect`, `nodes`, `self_node`,
////   `is_distributed`, `has_peers`, `health`, `ClusterHealth`,
////   `NodeStartError`, `ConnectError`,
////   `node_start_error_to_string`, `connect_error_to_string`.
//// - **Cluster events**: `start_monitor`, `subscribe`, `unsubscribe`,
////   `ClusterEvent`, `MonitorMessage`.
//// - **Protocol**: `named`, `new_subject`, `register`, `lookup`,
////   `unregister`, `unregister_typed`, `TypedName`, `GlobalSubject`,
////   `RegisterError`, `UnregisterError`, `LookupError`,
////   `register_error_to_string`, `unregister_error_to_string`,
////   `lookup_error_to_string`.
//// - **Actor lifecycle**: `start_actor` / `start_actor_with_timeout` /
////   `start_actor_observed`, `start_registered` /
////   `start_registered_with_timeout`, `start_supervised` /
////   `start_supervised_with_timeout`, `pool` / `pool_with_timeout`,
////   `child_spec` / `child_spec_with_timeout`, `HandlerStep`,
////   `ActorStartError`, `StartRegisteredError`,
////   `actor_start_error_to_string`, `start_registered_error_to_string`.
//// - **Messaging**: `send`, `reply`, `receive` / `receive_with_timeout`,
////   `call` / `call_with_timeout`, `call_isolated` /
////   `call_isolated_with_timeout`, `SendError`, `CallError`,
////   `ReceiveError`, `EncodeError`, `DecodeError`,
////   `send_error_to_string`, `call_error_to_string`,
////   `receive_error_to_string`, `encode_error_to_string`,
////   `decode_error_to_string`.
////
//// Most users only need `import distribute`. Low-level modules
//// (`distribute/actor`, `distribute/global`, `distribute/registry`,
//// `distribute/codec`, `distribute/receiver`, `distribute/cluster`,
//// `distribute/cluster_monitor`, `distribute/codec/composite`,
//// `distribute/codec/tagged`, `distribute/config`) remain available for
//// advanced cases not covered by the facade -- e.g. `whereis`,
//// `register_pid`, `register_typed`, `from_pid`, `from_subject`,
//// `lookup_with_timeout`, `lookup_async`, `start_registered_observed`,
//// `receive_typed`, `selecting_typed`.

import distribute/actor as dist_actor
import distribute/cluster
import distribute/cluster_monitor
import distribute/codec
import distribute/config
import distribute/global
import distribute/receiver
import distribute/registry
import distribute/telemetry
import gleam/erlang/process
import gleam/otp/actor
import gleam/otp/supervision

// -- Boot & config -----------------------------------------------------------

/// Library version, hardcoded. Must stay in sync with `gleam.toml`
/// at release time -- Gleam has no compile-time API to read the
/// manifest, so this is a release-time discipline, not a runtime
/// invariant.
pub fn version() -> String {
  "4.0.0"
}

pub type Config =
  config.Config

pub type ConfigError =
  config.ConfigError

/// Set global runtime configuration. Call once at application startup.
pub fn configure(cfg: Config) -> Result(Nil, ConfigError) {
  config.configure(cfg)
}

/// Read the current global configuration (or defaults if never configured).
pub fn get_config() -> Config {
  config.get()
}

pub fn config_error_to_string(err: ConfigError) -> String {
  config.config_error_to_string(err)
}

// -- Cluster -----------------------------------------------------------------

pub type NodeStartError =
  cluster.StartError

pub type ConnectError =
  cluster.ConnectError

pub type ClusterHealth =
  cluster.ClusterHealth

/// Start a distributed BEAM node.
///
/// `name` must contain `@` (e.g. `"myapp@127.0.0.1"`).
/// `cookie` must be `[a-zA-Z0-9_-]+` and 1..255 bytes (validated by FFI).
pub fn start_node(name: String, cookie: String) -> Result(Nil, NodeStartError) {
  cluster.start_node(name, cookie)
}

pub fn connect(node: String) -> Result(Nil, ConnectError) {
  cluster.connect(node)
}

pub fn nodes() -> List(String) {
  cluster.nodes()
}

pub fn self_node() -> String {
  cluster.self_node()
}

/// Whether this node is running BEAM distribution (via `erlang:is_alive/0`).
pub fn is_distributed() -> Bool {
  cluster.is_distributed()
}

/// Whether this node has at least one connected peer. *Not* a health
/// check -- a single-node deployment is operationally fine and will
/// return `False` here. Use this only to gate cluster-wide operations.
pub fn has_peers() -> Bool {
  cluster.has_peers()
}

/// Full cluster health snapshot with parallel pings.
pub fn health() -> ClusterHealth {
  cluster.health()
}

pub fn node_start_error_to_string(err: NodeStartError) -> String {
  cluster.start_error_to_string(err)
}

pub fn connect_error_to_string(err: ConnectError) -> String {
  cluster.connect_error_to_string(err)
}

// -- Cluster events ----------------------------------------------------------

pub type ClusterEvent =
  cluster_monitor.ClusterEvent

pub type MonitorMessage =
  cluster_monitor.Message

/// Start the cluster monitor actor. It listens for Erlang node events
/// and broadcasts them to all Gleam subscribers.
pub fn start_monitor() -> Result(
  process.Subject(MonitorMessage),
  actor.StartError,
) {
  cluster.start_monitor()
}

pub fn subscribe(
  monitor: process.Subject(MonitorMessage),
  listener: process.Subject(ClusterEvent),
) {
  cluster.subscribe(monitor, listener)
}

pub fn unsubscribe(
  monitor: process.Subject(MonitorMessage),
  listener: process.Subject(ClusterEvent),
) {
  cluster.unsubscribe(monitor, listener)
}

// -- Telemetry ---------------------------------------------------------------

pub type TelemetryEvent =
  telemetry.Event

pub type TelemetrySink =
  telemetry.EventSink

/// Install (or replace) the global telemetry sink for observability.
/// This single opt-in sink receives all load-bearing events from the library
/// (registry, atom budget, payload limits, codec failures, timeouts).
/// See `distribute/telemetry` for the full semantics and event structure.
pub fn install_telemetry(sink: TelemetrySink) -> Nil {
  telemetry.install(sink)
}

// -- Handler signatures ------------------------------------------------------

pub type HandlerStep(state) =
  receiver.HandlerStep(state)

// -- Protocol: named codecs and subjects -------------------------------------

pub type TypedName(msg) =
  registry.TypedName(msg)

pub type GlobalSubject(msg) =
  global.GlobalSubject(msg)

/// Create a `TypedName` from a bundled `Codec`.
///
/// ```gleam
/// let counter = distribute.named("counter", codec.int())
/// ```
pub fn named(name: String, c: codec.Codec(msg)) -> TypedName(msg) {
  registry.named(name, c)
}

/// Create a new `GlobalSubject` owned by the current process, from a
/// bundled `Codec`. For separate encoder/decoder, drop down to
/// `global.new` directly.
pub fn new_subject(c: codec.Codec(msg)) -> GlobalSubject(msg) {
  global.new(c.encoder, c.decoder)
}

// `unsafe_from_name` is intentionally not re-exported on the facade:
// reconstructing a `GlobalSubject` from a free-form (name, pid,
// codec) tuple bypasses the `TypedName` discipline that the rest of
// the API leans on. External callers should use `lookup` to obtain
// a Subject for a registered actor; that path threads the codec
// through the opaque `TypedName` and stays within the safety
// invariants. The function is still available on `distribute/global`
// as `@internal unsafe_from_name` for the registry, receiver, and
// actor modules.

// -- Protocol: registry ------------------------------------------------------

pub type RegisterError =
  registry.RegisterError

pub type UnregisterError =
  registry.UnregisterError

pub type LookupError =
  registry.LookupError

/// Register a `GlobalSubject` under its `TypedName`. The typed pair is
/// the recommended path; for raw-PID registration import
/// `distribute/registry` and call `registry.register/2` directly.
pub fn register(
  typed_name: TypedName(msg),
  subject: GlobalSubject(msg),
) -> Result(Nil, RegisterError) {
  registry.register_global(typed_name, subject)
}

/// Look up a `GlobalSubject` by its `TypedName`. For polling variants
/// (blocking and async) see `registry.lookup_with_timeout` /
/// `registry.lookup_async`.
pub fn lookup(typed_name: TypedName(msg)) -> Result(GlobalSubject(msg), Nil) {
  registry.lookup(typed_name)
}

/// Unregister a globally registered name. Idempotent cleanup paths can
/// `let _ = unregister(name)`.
pub fn unregister(name: String) -> Result(Nil, UnregisterError) {
  registry.unregister(name)
}

/// Type-safe sibling of `unregister/1`: pulls the name string from the
/// `TypedName` the caller already holds. Recommended for graceful
/// shutdown paths so the cleanup site never has to hardcode the name.
pub fn unregister_typed(
  typed_name: TypedName(msg),
) -> Result(Nil, UnregisterError) {
  registry.unregister_typed(typed_name)
}

pub fn register_error_to_string(err: RegisterError) -> String {
  registry.register_error_to_string(err)
}

pub fn unregister_error_to_string(err: UnregisterError) -> String {
  registry.unregister_error_to_string(err)
}

pub fn lookup_error_to_string(err: LookupError) -> String {
  registry.lookup_error_to_string(err)
}

// -- Actors ------------------------------------------------------------------

pub type ActorStartError =
  actor.StartError

pub type StartRegisteredError =
  dist_actor.StartRegisteredError

/// Render a `gleam_otp/actor.StartError` as a human-readable string.
pub fn actor_start_error_to_string(err: ActorStartError) -> String {
  case err {
    actor.InitTimeout -> "Actor init timed out"
    actor.InitFailed(reason) -> "Actor init failed: " <> reason
    actor.InitExited(_) -> "Actor init process exited"
  }
}

pub fn start_registered_error_to_string(err: StartRegisteredError) -> String {
  dist_actor.start_registered_error_to_string(err)
}

/// Start a named actor using `config.get().default_init_timeout_ms`.
/// Use `start_actor_with_timeout/4` for an explicit timeout.
pub fn start_actor(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
) -> Result(GlobalSubject(msg), ActorStartError) {
  dist_actor.start_default(typed_name, initial_state, handler)
}

/// Start a named actor with an explicit init timeout.
pub fn start_actor_with_timeout(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(GlobalSubject(msg), ActorStartError) {
  dist_actor.start(typed_name, initial_state, handler, init_timeout_ms)
}

/// Start a named actor with a decode-error callback. Useful for
/// logging or metering malformed messages across nodes (e.g. during
/// rolling deploys with mismatched codec versions). Uses
/// `config.get().default_init_timeout_ms` -- if you need a custom
/// init timeout, drop down to `actor.start_observed` directly.
pub fn start_actor_observed(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
  on_decode_error: fn(codec.DecodeError) -> Nil,
) -> Result(GlobalSubject(msg), ActorStartError) {
  dist_actor.start_observed(
    typed_name,
    initial_state,
    handler,
    config.get().default_init_timeout_ms,
    on_decode_error,
  )
}

/// Start an actor and register it globally in one step. Uses
/// `config.get().default_init_timeout_ms`. Use
/// `start_registered_with_timeout/4` for an explicit timeout, or
/// `actor.start_registered_observed` for the decode-error hook variant.
pub fn start_registered(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
) -> Result(GlobalSubject(msg), StartRegisteredError) {
  dist_actor.start_registered_default(typed_name, initial_state, handler)
}

/// Like `start_registered`, with an explicit init timeout.
pub fn start_registered_with_timeout(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(GlobalSubject(msg), StartRegisteredError) {
  dist_actor.start_registered(
    typed_name,
    initial_state,
    handler,
    init_timeout_ms,
  )
}

/// Start a supervised actor using `config.get().default_init_timeout_ms`.
pub fn start_supervised(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
) -> Result(process.Pid, ActorStartError) {
  dist_actor.start_supervised_default(typed_name, initial_state, handler)
}

/// Like `start_supervised`, with an explicit init timeout.
pub fn start_supervised_with_timeout(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(process.Pid, ActorStartError) {
  dist_actor.start_supervised(
    typed_name,
    initial_state,
    handler,
    init_timeout_ms,
  )
}

/// Start N supervised actors using `config.get().default_init_timeout_ms`.
pub fn pool(
  typed_name: TypedName(msg),
  size: Int,
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
) -> Result(process.Pid, ActorStartError) {
  dist_actor.pool_default(typed_name, size, initial_state, handler)
}

/// Like `pool`, with an explicit init timeout.
pub fn pool_with_timeout(
  typed_name: TypedName(msg),
  size: Int,
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(process.Pid, ActorStartError) {
  dist_actor.pool(typed_name, size, initial_state, handler, init_timeout_ms)
}

/// OTP child spec using `config.get().default_init_timeout_ms`.
pub fn child_spec(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
) -> supervision.ChildSpecification(GlobalSubject(msg)) {
  dist_actor.child_spec_default(typed_name, initial_state, handler)
}

/// Like `child_spec`, with an explicit init timeout.
pub fn child_spec_with_timeout(
  typed_name: TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> HandlerStep(state),
  init_timeout_ms: Int,
) -> supervision.ChildSpecification(GlobalSubject(msg)) {
  dist_actor.child_spec(typed_name, initial_state, handler, init_timeout_ms)
}

// -- Messaging ---------------------------------------------------------------

pub type SendError =
  global.SendError

pub type CallError =
  global.CallError

pub type ReceiveError =
  receiver.ReceiveError

pub type EncodeError =
  codec.EncodeError

pub type DecodeError =
  codec.DecodeError

pub fn send(
  subject: GlobalSubject(msg),
  message: msg,
) -> Result(Nil, SendError) {
  global.send(subject, message)
}

/// Send a response through a reply subject. Used by handlers to answer a `call`.
pub fn reply(
  reply_to: process.Subject(BitArray),
  response: resp,
  encoder: codec.Encoder(resp),
) -> Result(Nil, SendError) {
  global.reply(reply_to, response, encoder)
}

/// Receive a typed message using `config.get().default_call_timeout_ms`.
/// Use `receive_with_timeout/2` for an explicit timeout.
pub fn receive(subject: GlobalSubject(msg)) -> Result(msg, DecodeError) {
  global.receive_default(subject)
}

/// Like `receive`, with an explicit timeout.
pub fn receive_with_timeout(
  subject: GlobalSubject(msg),
  timeout_ms: Int,
) -> Result(msg, DecodeError) {
  global.receive(subject, timeout_ms)
}

/// Synchronous request/response with monitor-based `TargetDown` detection.
/// Uses `config.get().default_call_timeout_ms`. Use `call_with_timeout/4`
/// for an explicit timeout.
///
/// ## Late-reply caveat -- choose `call_isolated` for long-running callers
///
/// `call` is the cheap default. It is **safe** for short-lived callers
/// (CLI tools, request handlers, scripts) whose process exits shortly
/// after the call returns: orphan late-replies die with the process.
///
/// It is **not** the right choice for long-lived processes (OTP
/// actors, supervisors, manager loops) that issue many `call`s under
/// sustained timeouts. A reply that arrives *after* `call` returns
/// `Error(Timeout)` cannot be evicted from the caller's mailbox by
/// the BEAM (no `erlang:alias/0`-aware Subject layout in the current
/// `gleam_erlang`); selective receive scans every orphan on every
/// subsequent `process.receive`, and tens of thousands of orphans
/// quietly degrade the caller's throughput.
///
/// For that shape, prefer `call_isolated/3`: it runs each call inside
/// a short-lived unlinked proxy process whose mailbox is reaped on
/// exit. See `global.call/4` and `docs/safety_and_limits.md` for the
/// full design rationale.
pub fn call(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
) -> Result(resp, CallError) {
  global.call_default(target, make_request, response_decoder)
}

/// Like `call`, with an explicit timeout. Inherits the same
/// late-reply caveat -- see `call/3`.
pub fn call_with_timeout(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
  timeout_ms: Int,
) -> Result(resp, CallError) {
  global.call(target, make_request, response_decoder, timeout_ms)
}

/// Mailbox-safe variant of `call` using
/// `config.get().default_call_timeout_ms`. Each invocation runs inside
/// a short-lived unlinked proxy process so orphan late-replies die
/// with the proxy instead of polluting the caller's mailbox.
/// Recommended for long-running callers issuing many RPCs under
/// sustained timeouts. See `global.call_isolated` for the full design
/// rationale.
pub fn call_isolated(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
) -> Result(resp, CallError) {
  global.call_isolated_default(target, make_request, response_decoder)
}

/// Like `call_isolated`, with an explicit timeout.
pub fn call_isolated_with_timeout(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
  timeout_ms: Int,
) -> Result(resp, CallError) {
  global.call_isolated(target, make_request, response_decoder, timeout_ms)
}

pub fn send_error_to_string(err: SendError) -> String {
  global.send_error_to_string(err)
}

pub fn call_error_to_string(err: CallError) -> String {
  global.call_error_to_string(err)
}

pub fn receive_error_to_string(err: ReceiveError) -> String {
  receiver.receive_error_to_string(err)
}

pub fn encode_error_to_string(err: EncodeError) -> String {
  codec.encode_error_to_string(err)
}

pub fn decode_error_to_string(err: DecodeError) -> String {
  codec.decode_error_to_string(err)
}
