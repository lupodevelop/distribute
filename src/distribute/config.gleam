/// Global runtime configuration for distribute.
///
/// Set once at application startup with `configure/1`. All library
/// functions read via `get/0`, which falls back to `default/0` when
/// not yet configured.
///
/// ## persistent_term
///
/// Configuration is stored in `persistent_term` for zero-cost reads.
/// Avoid calling `configure/1` more than once: every write invalidates
/// the value in every process heap, which can cause GC spikes under load.
/// Treat `configure/1` as a boot-only operation in your root supervisor.
/// Never wire it to runtime input (HTTP handlers, webhooks, admin RPC),
/// or you risk cluster-wide VM pauses, heartbeat timeouts, and net split.
///
/// ## Example
///
/// ```gleam
/// // at application startup, before starting any actors
/// config.configure(config.Config(
///   ..config.default(),
///   default_call_timeout_ms: 10_000,
///   max_payload_size_bytes: 1 * 1024 * 1024,
/// ))
/// ```
import gleam/int
import gleam/result

@external(erlang, "config_ffi", "put_config")
fn put_term(value: Config) -> Result(Nil, ConfigError)

@external(erlang, "config_ffi", "get_config")
fn get_term(default: Config) -> Config

@external(erlang, "config_ffi", "put_atom_budget")
fn put_atom_budget(n: Int) -> Nil

@external(erlang, "config_ffi", "put_conflict_resolver_timeout")
fn put_conflict_resolver_timeout(n: Int) -> Nil

/// **Production danger.** Reset all `persistent_term` state for the
/// distribute config and zero the atom budget counter. Intended only
/// for test isolation: a live system relying on the previous config
/// (codec defaults, payload caps, registered atoms) will see actors
/// cross the boundary with mismatched expectations. The function is
/// exported with `@internal` so it stays out of generated docs and
/// autocomplete in downstream projects, but it remains callable via
/// FFI for cross-module test harnesses.
@external(erlang, "config_ffi", "reset_config")
@internal
pub fn reset() -> Nil

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

pub type Config {
  Config(
    /// Timeout for `global.call` requests, in milliseconds.
    default_call_timeout_ms: Int,
    /// Timeout for OTP actor initialisation, in milliseconds.
    default_init_timeout_ms: Int,
    /// Maximum allowed payload size in bytes.
    /// `global.send`, `global.call` and `global.receive` return
    /// `PayloadTooLarge` when a message exceeds this limit.
    max_payload_size_bytes: Int,
    /// Maximum number of *fresh* atoms `distribute` will create through
    /// its FFI helpers (node names + cookies). Existing atoms cost
    /// nothing. Once the budget is exhausted, `cluster.connect`,
    /// `cluster.ping`, and `cluster.start_node` return
    /// `AtomBudgetExceeded` instead of letting the caller exhaust the
    /// VM atom table (default cap 1 048 576, no GC).
    ///
    /// Default 10 000. Ten times a generous cluster size, four
    /// orders of magnitude below the VM cap.
    max_distribution_atoms: Int,
    /// Decoder cap for list element count. Protects against CPU and
    /// heap amplification from tiny payloads with huge declared counts
    /// (for example `<<1_000_000:32>>` with `list(nil())`).
    ///
    /// Default 10 000. Raise only if your workload really needs larger
    /// in-memory frames and you have tested scheduler impact.
    max_decoded_list_elements: Int,
    /// Grace window while waiting for proxy DOWN in `call_isolated`
    /// timeout teardown.
    ///
    /// Default 5 000 ms.
    isolated_proxy_shutdown_grace_ms: Int,
    /// Poll interval used by `actor.start_resource_owner` fallback
    /// liveness checks when DOWN delivery is delayed.
    ///
    /// Default 5 000 ms.
    resource_owner_poll_ms: Int,
    /// Grace window while waiting for proxy DOWN in `cluster.health`
    /// timeout teardown.
    ///
    /// Default 1 000 ms.
    health_proxy_shutdown_grace_ms: Int,
    /// Hard deadline (ms) for a custom split-brain conflict resolver
    /// installed via `registry.register_global_with_resolver/3`.
    ///
    /// The resolver runs inside the global_name_server worker; while
    /// it executes, every `:global` operation cluster-wide is
    /// serialised behind it. Past this deadline the FFI shim kills
    /// the worker and applies a deterministic fallback (lowest
    /// term-ordered PID wins). Lower is safer for cluster
    /// throughput; higher gives the user fn room to do RPC work.
    ///
    /// Default 1 000 ms. Pure resolvers (`lowest_pid_wins`,
    /// `keep_local`, `node_priority`) take microseconds and never
    /// approach this. RPC-based resolvers should fit comfortably
    /// inside 1 second on a healthy cluster; raise only when
    /// profiling shows a legitimate need and the cluster can
    /// tolerate the longer per-conflict stall.
    conflict_resolver_timeout_ms: Int,
  )
}

pub type ConfigError {
  /// The configuration was already loaded and cannot be mutated.
  AlreadyConfigured
  /// A timeout field was zero or negative.
  InvalidTimeout(field: String, value: Int)
  /// The payload size limit was zero or negative.
  InvalidPayloadSize(Int)
  /// The atom-creation budget was zero or negative.
  InvalidAtomBudget(Int)
  /// The list decode element cap was zero or negative.
  InvalidListElementCap(Int)
}

pub fn config_error_to_string(err: ConfigError) -> String {
  case err {
    AlreadyConfigured ->
      "Configuration already loaded (persistent_term is immutable)"
    InvalidTimeout(f, v) ->
      "Invalid timeout for " <> f <> ": " <> int.to_string(v)
    InvalidPayloadSize(v) -> "Invalid payload size: " <> int.to_string(v)
    InvalidAtomBudget(v) ->
      "Invalid atom budget: " <> int.to_string(v) <> " (must be > 0)"
    InvalidListElementCap(v) ->
      "Invalid list element cap: " <> int.to_string(v) <> " (must be > 0)"
  }
}

// ---------------------------------------------------------------------------
// API
// ---------------------------------------------------------------------------

/// Returns the built-in defaults:
/// - `default_call_timeout_ms`: 5 000 ms
/// - `default_init_timeout_ms`: 5 000 ms
/// - `max_payload_size_bytes`: 4 MiB (4 * 1 024 * 1 024)
/// - `max_distribution_atoms`: 10 000
/// - `max_decoded_list_elements`: 10 000
/// - `isolated_proxy_shutdown_grace_ms`: 5 000 ms
/// - `resource_owner_poll_ms`: 5 000 ms
/// - `health_proxy_shutdown_grace_ms`: 1 000 ms
/// - `conflict_resolver_timeout_ms`: 1 000 ms
pub fn default() -> Config {
  Config(
    default_call_timeout_ms: 5000,
    default_init_timeout_ms: 5000,
    max_payload_size_bytes: 4 * 1024 * 1024,
    max_distribution_atoms: 10_000,
    max_decoded_list_elements: 10_000,
    isolated_proxy_shutdown_grace_ms: 5000,
    resource_owner_poll_ms: 5000,
    health_proxy_shutdown_grace_ms: 1000,
    conflict_resolver_timeout_ms: 1000,
  )
}

/// Store a custom configuration. Call once at startup.
///
/// Returns `Error(ConfigError)` if any value is invalid (zero or negative)
/// or if the configuration has already been loaded via a previous call.
///
/// **Danger**: call this only during application boot. `persistent_term`
/// writes trigger a global VM-wide heap invalidation pass; exposing this
/// path to runtime traffic is an operational footgun and can cause
/// heartbeat failures between nodes under load.
///
/// Side-effect: also writes `max_distribution_atoms` to a separate
/// `persistent_term` slot read by the FFI atom helpers, so the budget
/// is observable from Erlang without needing to decode the Gleam
/// Config tuple.
pub fn configure(cfg: Config) -> Result(Nil, ConfigError) {
  use _ <- result.try(check_positive_timeout(
    "default_call_timeout_ms",
    cfg.default_call_timeout_ms,
  ))
  use _ <- result.try(check_positive_timeout(
    "default_init_timeout_ms",
    cfg.default_init_timeout_ms,
  ))
  use _ <- result.try(check_positive_payload_size(cfg.max_payload_size_bytes))
  use _ <- result.try(check_positive_atom_budget(cfg.max_distribution_atoms))
  use _ <- result.try(check_positive_list_cap(cfg.max_decoded_list_elements))
  use _ <- result.try(check_positive_timeout(
    "isolated_proxy_shutdown_grace_ms",
    cfg.isolated_proxy_shutdown_grace_ms,
  ))
  use _ <- result.try(check_positive_timeout(
    "resource_owner_poll_ms",
    cfg.resource_owner_poll_ms,
  ))
  use _ <- result.try(check_positive_timeout(
    "health_proxy_shutdown_grace_ms",
    cfg.health_proxy_shutdown_grace_ms,
  ))
  use _ <- result.try(check_positive_timeout(
    "conflict_resolver_timeout_ms",
    cfg.conflict_resolver_timeout_ms,
  ))
  use _ <- result.try(put_term(cfg))
  put_atom_budget(cfg.max_distribution_atoms)
  put_conflict_resolver_timeout(cfg.conflict_resolver_timeout_ms)
  Ok(Nil)
}

fn check_positive_timeout(
  field: String,
  value: Int,
) -> Result(Nil, ConfigError) {
  case value > 0 {
    True -> Ok(Nil)
    False -> Error(InvalidTimeout(field, value))
  }
}

fn check_positive_payload_size(value: Int) -> Result(Nil, ConfigError) {
  case value > 0 {
    True -> Ok(Nil)
    False -> Error(InvalidPayloadSize(value))
  }
}

fn check_positive_atom_budget(value: Int) -> Result(Nil, ConfigError) {
  case value > 0 {
    True -> Ok(Nil)
    False -> Error(InvalidAtomBudget(value))
  }
}

fn check_positive_list_cap(value: Int) -> Result(Nil, ConfigError) {
  case value > 0 {
    True -> Ok(Nil)
    False -> Error(InvalidListElementCap(value))
  }
}

/// Read the active configuration.
///
/// Never fails: returns `default()` when `configure/1` has not been called.
pub fn get() -> Config {
  get_term(default())
}
