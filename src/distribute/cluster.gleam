/// Cluster management: start a distributed node, connect to others,
/// check health.
import distribute/cluster_monitor
import distribute/config
import distribute/telemetry
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

pub type StartError {
  /// Node name failed format validation: must be `<name>@<host>` with
  /// charset `[a-zA-Z0-9_-]+@[a-zA-Z0-9._-]+` and 1..255 bytes.
  InvalidNodeName(String)
  /// Cookie failed format validation: charset `[a-zA-Z0-9_-]+`, 1..255 bytes.
  InvalidCookieFormat(String)
  /// `net_kernel:start/1` reported the node was already running.
  AlreadyStarted
  /// `net_kernel:start/1` failed with a network-related reason
  /// (`network`, `eaddrinuse`, `econnrefused`).
  NetworkError(String)
  /// `net_kernel:start/1` failed with another reason.
  StartFailed(String)
  /// The configured `max_distribution_atoms` budget has been exhausted.
  /// Creating the node-name or cookie atom would exceed the cap. Either
  /// raise `max_distribution_atoms` or stop accepting fresh node names
  /// from the upstream caller.
  StartAtomBudgetExceeded
}

/// Errors from `connect/1`.
///
/// `net_kernel:connect_node/1` returns only `true | false | ignored`.
/// It cannot distinguish "node does not exist" from "unreachable". A
/// `NodeNotFound` variant would be a lie at this layer, so it is not
/// exposed; both cases collapse to `ConnectFailed`.
pub type ConnectError {
  /// The peer was reachable in principle (distribution is up) but
  /// refused or did not answer. Returned by `net_kernel:connect_node/1 = false`.
  ConnectFailed
  /// The local node is not running distribution (`net_kernel` not started),
  /// so `connect_node` declined to even try. Returned by `connect_node = ignored`.
  ConnectIgnored
  /// The supplied name failed format validation (missing `@`, disallowed
  /// charset, length). Carries a human-readable reason.
  InvalidNodeFormat(String)
  /// Connecting would create a fresh node atom but the configured
  /// `max_distribution_atoms` budget is exhausted. Refused before
  /// touching `binary_to_atom`. The VM atom table stays safe.
  ConnectAtomBudgetExceeded
}

pub fn start_error_to_string(err: StartError) -> String {
  case err {
    InvalidNodeName(r) -> "Invalid node name: " <> r
    InvalidCookieFormat(r) -> "Invalid cookie format: " <> r
    AlreadyStarted -> "Node is already started"
    NetworkError(r) -> "Network error: " <> r
    StartFailed(r) -> "Node start failed: " <> r
    StartAtomBudgetExceeded ->
      "Atom budget exceeded (raise config.max_distribution_atoms)"
  }
}

pub fn connect_error_to_string(err: ConnectError) -> String {
  case err {
    ConnectFailed -> "Connect failed (peer unreachable or refused)"
    ConnectIgnored -> "Connect ignored (local node not distributed)"
    InvalidNodeFormat(r) -> "Invalid node format: " <> r
    ConnectAtomBudgetExceeded ->
      "Atom budget exceeded (raise config.max_distribution_atoms)"
  }
}

// ---------------------------------------------------------------------------
// FFI bindings (private)
// ---------------------------------------------------------------------------

/// Returns {ok, nil} | {error, already_started} | {error, {start_failed, R}}
/// etc. maps directly to Result(Nil, StartError).
/// Name/@-validation and cookie-length check are done in Gleam before calling.
@external(erlang, "cluster_ffi", "start_node")
fn start_node_ffi(name: String, cookie: String) -> Result(Nil, StartError)

/// Returns {ok, nil} | {error, connect_failed} | {error, connect_ignored}.
@external(erlang, "cluster_ffi", "connect")
fn connect_ffi(node: String) -> Result(Nil, ConnectError)

@external(erlang, "cluster_ffi", "nodes")
fn nodes_ffi() -> List(String)

@external(erlang, "cluster_ffi", "self_node")
fn self_node_ffi() -> String

@external(erlang, "cluster_ffi", "ping")
fn ping_ffi(node: String) -> Bool

@external(erlang, "cluster_ffi", "is_alive")
fn is_alive_ffi() -> Bool

@external(erlang, "distribute_ffi_utils", "monotonic_ms")
fn monotonic_ms() -> Int

@external(erlang, "distribute_ffi_utils", "exit_shutdown")
fn exit_shutdown(pid: process.Pid) -> Bool

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Start a distributed BEAM node.
///
/// `name` must contain `@` (e.g. `"myapp@127.0.0.1"`).
/// Cookie length and charset are enforced byte-wise by the FFI: any
/// failure surfaces as `InvalidCookieFormat`.
///
/// Atom-budget exhaustion: the FFI emits
/// `AtomBudgetExhausted(<offending input>, AtomBudgetOnStartNode)`
/// before returning, with the actual offending input (name or
/// cookie). We do not re-emit here because the public unit
/// constructor `StartAtomBudgetExceeded` cannot carry that
/// attribution.
///
/// ## Blocking and OS-level dependencies
///
/// **This call can block.** It delegates to `net_kernel:start/1`,
/// which talks to `epmd` (Erlang Port Mapper Daemon) and resolves
/// the host portion of `name` against the OS resolver. If `epmd`
/// is not running, if DNS is misconfigured, or if the network goes
/// down a moment before the call, the BEAM may hang on a libc
/// resolver timeout for tens of seconds and there is no Gleam-side
/// timeout the library can interpose.
///
/// Mitigations callers can apply:
///
/// - Run `epmd -daemon` before the process boots, and treat its
///   absence as a fatal startup condition rather than something
///   `start_node` should recover from.
/// - Use IP literals (`myapp@127.0.0.1`) when the deployment allows,
///   bypassing DNS entirely.
/// - In container deployments, ensure `/etc/hosts` resolves the
///   chosen host before `start_node` is called.
///
/// If you cannot accept a potentially long boot wait, supervise the
/// boot itself: spawn a process that calls `start_node`, monitor
/// it, and treat a deadline miss as a startup failure. The library
/// does not bake a timeout in because the right value is
/// deployment-specific (a 2 s timeout is generous for IP literals
/// but a hair-trigger for DNS-backed names).
pub fn start_node(name: String, cookie: String) -> Result(Nil, StartError) {
  start_node_ffi(name, cookie)
}

/// Start the cluster monitor actor. It listens for Erlang node events
/// and broadcasts them to all Gleam subscribers.
pub fn start_monitor() -> Result(
  Subject(cluster_monitor.Message),
  actor.StartError,
) {
  cluster_monitor.start()
}

/// Subscribe a subject to cluster events (NodeUp/NodeDown).
pub fn subscribe(
  monitor: Subject(cluster_monitor.Message),
  listener: Subject(cluster_monitor.ClusterEvent),
) {
  cluster_monitor.subscribe(monitor, listener)
}

/// Unsubscribe from cluster events.
pub fn unsubscribe(
  monitor: Subject(cluster_monitor.Message),
  listener: Subject(cluster_monitor.ClusterEvent),
) {
  cluster_monitor.unsubscribe(monitor, listener)
}

/// Connect to a remote node. Returns `Ok(Nil)` on success.
///
/// ## Atom-table guardrail
///
/// Each call with a previously-unseen node name interns one atom in
/// the BEAM atom table (atoms are never garbage collected, and the
/// table is capped at 1 048 576 entries by default). To prevent a
/// caller, malicious or buggy, from exhausting the table by
/// looping over millions of valid-looking names, every fresh atom
/// creation is **counted against `config.max_distribution_atoms`**.
///
/// The check is atomic (`atomics:add_get/3`) and lock-free. Once the
/// budget is reached, this function returns
/// `Error(ConnectAtomBudgetExceeded)` *before* `binary_to_atom/2` is
/// called: the VM atom table cannot be exhausted through this path.
///
/// Default budget: 10 000 fresh atoms over the process lifetime.
/// 10x a generous cluster size, four orders of magnitude below the
/// VM cap. Tune via `config.configure(... max_distribution_atoms:)`.
pub fn connect(node: String) -> Result(Nil, ConnectError) {
  // Format/charset/byte-size validation is delegated to the FFI
  // (`is_valid_node_name/1`); a Gleam-side `string.contains("@")`
  // duplicate is not necessary and would diverge on edge inputs
  // (e.g. multi-byte codepoints) from the byte-wise FFI check.
  case connect_ffi(node) {
    Error(ConnectAtomBudgetExceeded) -> {
      telemetry.emit(telemetry.AtomBudgetExhausted(
        node,
        telemetry.AtomBudgetOnConnect,
      ))
      Error(ConnectAtomBudgetExceeded)
    }
    other -> other
  }
}

/// List all currently connected nodes.
pub fn nodes() -> List(String) {
  nodes_ffi()
}

/// Get the current node's name.
pub fn self_node() -> String {
  self_node_ffi()
}

/// Ping a remote node. Returns `True` if it responds.
///
/// Subject to the same `config.max_distribution_atoms` guardrail as
/// [`connect`](#connect): once the fresh-atom budget is exhausted,
/// `ping` returns `False` (cannot reach) without touching the VM
/// atom table.
pub fn ping(node: String) -> Bool {
  ping_ffi(node)
}

/// Whether this node is running BEAM distribution.
///
/// Backed by `erlang:is_alive/0`, which is the authoritative signal.
/// It returns `true` iff `net_kernel` has been started. Previous versions
/// compared the string form of the node name against `"nonode@nohost"`,
/// which would lie if the runtime ever changed that placeholder.
pub fn is_distributed() -> Bool {
  is_alive_ffi()
}

/// Number of currently connected nodes.
pub fn connected_count() -> Int {
  list.length(nodes())
}

/// Whether this node has at least one connected peer.
///
/// This is a topology check, not a health check: a single-node deployment
/// is operationally fine and will return `False` here.
pub fn has_peers() -> Bool {
  is_distributed() && nodes() != []
}

/// Deprecated alias for `has_peers/0`, kept for compatibility with direct
/// `distribute/cluster` imports from pre-facade code.
pub fn is_healthy() -> Bool {
  has_peers()
}

// ---------------------------------------------------------------------------
// Cluster health
// ---------------------------------------------------------------------------

pub type ClusterHealth {
  ClusterHealth(
    self_node: String,
    is_distributed: Bool,
    connected_nodes: List(String),
    connected_count: Int,
    reachable_nodes: List(String),
    unreachable_nodes: List(String),
  )
}

/// Perform a cluster health check, pinging each known node **in parallel**.
///
/// See also: `has_peers/0` (boolean topology shortcut), `is_healthy/0`
/// (compatibility alias), `is_distributed/0`,
/// `ping/1` (single node).
///
/// `net_adm:ping/1` is a synchronous network call with an implicit BEAM
/// distribution timeout of several seconds. Pinging N nodes sequentially
/// would block the caller for up to N * timeout_per_ping (e.g. 50 nodes
/// during a partition = ~6 minutes). We fan out with bounded parallelism
/// and collect results with a single 8 s deadline. Worst-case wall clock
/// is still bounded by the deadline, not by cluster size.
///
/// Output ordering is deterministic: `reachable_nodes` and
/// `unreachable_nodes` are projected in the same order as
/// `connected_nodes`, regardless of worker reply timing.
pub fn health() -> ClusterHealth {
  let self = self_node()
  let is_dist = is_distributed()
  case is_dist {
    False ->
      ClusterHealth(
        self_node: self,
        is_distributed: False,
        connected_nodes: [],
        connected_count: 0,
        reachable_nodes: [],
        unreachable_nodes: [],
      )
    True -> {
      let connected = nodes()
      let #(reachable, unreachable) = parallel_partition_reachable(connected)
      ClusterHealth(
        self_node: self,
        is_distributed: True,
        connected_nodes: connected,
        connected_count: list.length(connected),
        reachable_nodes: reachable,
        unreachable_nodes: unreachable,
      )
    }
  }
}

/// Hard deadline for `health()` collecting parallel-ping results.
const health_collect_deadline_ms: Int = 8000

/// Upper bound for in-flight health ping workers per call.
const health_max_parallel_pings: Int = 32

/// Small caller-side grace window beyond `health_collect_deadline_ms` to
/// absorb the proxy hop and mailbox scheduling jitter. This is not part of
/// the semantic deadline for ping collection itself. It only covers the
/// final result handoff from proxy to caller.
const health_proxy_settle_buffer_ms: Int = 500

fn health_proxy_shutdown_grace_ms() -> Int {
  config.get().health_proxy_shutdown_grace_ms
}

/// Effective worker cap used by `health`.
@internal
pub fn health_parallelism_cap(connected_count: Int) -> Int {
  case connected_count <= 0 {
    True -> 0
    False ->
      case connected_count < health_max_parallel_pings {
        True -> connected_count
        False -> health_max_parallel_pings
      }
  }
}

/// Spawn up to `health_parallelism_cap/1` ping workers, collect their
/// results into reachable and unreachable buckets. Bounded by an
/// *absolute* monotonic deadline (`health_collect_deadline_ms` from
/// `monotonic_ms()` at fan-out time): any node whose ping has not
/// returned by that instant is filed as unreachable. Total wall-clock
/// cost is therefore O(1) in cluster size, regardless of how many slow
/// pings stack up.
///
/// The collector and the per-node ping workers run inside a **proxy
/// process** spawned just for this call. The caller waits only on a
/// one-shot result Subject owned by itself. When the proxy exits (after
/// the deadline or when all workers have replied), its mailbox dies
/// with it. Any slow worker whose `net_adm:ping` returns *after* the
/// deadline sends its report to the now-dead proxy Pid, which the BEAM
/// drops at zero cost.
///
/// Without the proxy, the slow worker's late report would land in the
/// caller's mailbox tagged with the orphan collector Subject, paying
/// the selective-receive penalty for every subsequent message. A
/// long-running cluster manager polling `health()` during a partition
/// would then leak orphan reports indefinitely.
///
/// Earlier drafts also passed `deadline_ms` unchanged to every
/// recursive `process.receive`, turning the bound into "deadline PER
/// MESSAGE" and stacking back into O(N) latency. We now compute the
/// remaining budget from the absolute deadline on each iteration.
/// Internal sum type for the unified select on
/// `parallel_partition_reachable` routes the proxy's normal reply
/// and the proxy's `DOWN` (if it crashed before answering) through one
/// `selector_receive` call.
type ProxyResult {
  PartitionReady(#(List(String), List(String)))
  ProxyExited
}

fn parallel_partition_reachable(
  connected: List(String),
) -> #(List(String), List(String)) {
  let result_subject: process.Subject(#(List(String), List(String))) =
    process.new_subject()
  let proxy_pid =
    process.spawn_unlinked(fn() {
      let collector = process.new_subject()
      let max_parallel = health_parallelism_cap(list.length(connected))
      let #(queued, workers) =
        seed_ping_workers(collector, connected, [], max_parallel)
      let deadline = monotonic_ms() + health_collect_deadline_ms
      // Pass `connected` as the initial pending set. Whenever a reply
      // arrives we drop that node from `pending`; whenever the deadline
      // fires we file every still-pending node as unreachable. This
      // preserves the invariant `reachable ∪ unreachable = connected`
      // even on partial timeouts, where the previous design silently
      // dropped pending nodes from the result.
      let pending =
        list.fold(connected, dict.new(), fn(acc, node) {
          dict.insert(acc, node, Nil)
        })
      let partition =
        collect_pings(collector, queued, pending, workers, [], [], deadline)
      process.send(result_subject, partition)
      // Proxy exits here. Its `collector` mailbox is reaped, so any
      // ping worker whose `net_adm:ping` returns after the deadline
      // sends to a dead Pid. A silent BEAM no-op.
    })

  // Monitor the proxy and select on result OR DOWN in one selector.
  // Two reasons:
  //   1. If the proxy crashes before sending, we surface the failure
  //      cleanly instead of waiting the full 8.5 s.
  //   2. On caller-side timeout we need to *synchronously wait* for
  //      the proxy to actually be dead before draining the result
  //      Subject. `process.kill` is asynchronous, the proxy can
  //      still complete one more reduction (typically the
  //      `process.send` we are racing against) before the kill signal
  //      is processed. Without the post-kill DOWN wait, an orphan
  //      tuple could land in the caller's mailbox after our drain,
  //      reintroducing the very leak the proxy was meant to prevent.
  let proxy_mon = process.monitor(proxy_pid)
  let selector =
    process.new_selector()
    |> process.select_map(result_subject, PartitionReady)
    |> process.select_specific_monitor(proxy_mon, fn(_) { ProxyExited })
  case
    process.selector_receive(
      selector,
      health_collect_deadline_ms + health_proxy_settle_buffer_ms,
    )
  {
    Ok(PartitionReady(partition)) -> {
      // Discard the DOWN message that arrives when the proxy exits
      // normally after sending. `demonitor_process` flushes it from
      // the caller's mailbox.
      process.demonitor_process(proxy_mon)
      let #(raw_reachable, _raw_unreachable) = partition
      stable_partition(connected, raw_reachable)
    }
    Ok(ProxyExited) ->
      // Proxy crashed before sending a partition; treat every node
      // as unreachable so the partition stays total.
      #([], connected)
    Error(Nil) -> {
      // Caller-side timeout. Kill the proxy and wait for its DOWN
      // signal before touching the result Subject. Only then is
      // the proxy guaranteed to have stopped scheduling, so any
      // in-flight `process.send` is either already in our mailbox
      // (catchable) or never sent at all (because the kill arrived
      // first).
      //
      // If the DOWN does not arrive within the grace window
      // (NIF-bound proxy, scheduler stall, etc.) we fall back to an
      // explicit `demonitor_process` so a late DOWN cannot land in
      // the caller's mailbox after this function returns. Same
      // teardown pattern as `global.call_isolated`. Without it the
      // mailbox-pollution leak the proxy was meant to fix would
      // re-open at the very edge case the proxy was supposed to
      // cover.
      //
      // Accepted residual risk (same as `call_isolated`): a NIF-bound
      // proxy that yields after the demonitor fallback, mid-
      // `process.send`, can leave one orphan partition tuple in the
      // caller's mailbox. Microsecond window, scheduler-dependent.
      // The fundamental fix waits on `erlang:alias/0`-aware Subjects
      // upstream; impact is bounded to one tuple per affected call.
      process.kill(proxy_pid)
      let down_selector =
        process.new_selector()
        |> process.select_specific_monitor(proxy_mon, fn(_) { Nil })
      case
        process.selector_receive(
          down_selector,
          health_proxy_shutdown_grace_ms(),
        )
      {
        Ok(Nil) -> Nil
        Error(Nil) -> process.demonitor_process(proxy_mon)
      }
      drain_proxy_reply(result_subject)
      #([], connected)
    }
  }
}

/// Consume a late partition tuple raced past our caller-side timeout.
fn drain_proxy_reply(
  result_subject: process.Subject(#(List(String), List(String))),
) -> Nil {
  let drain =
    process.new_selector()
    |> process.select_map(result_subject, fn(_) { Nil })
  let _ = process.selector_receive(drain, 0)
  Nil
}

/// Canonicalise health partitions to the order of `connected`.
///
/// Ping workers reply out-of-order by design; timeout fallback paths may also
/// carry pending nodes from map keys whose ordering is implementation-defined.
/// Projecting over `connected` gives deterministic output while preserving the
/// total-partition invariant (`reachable ∪ unreachable = connected`).
@internal
pub fn stable_partition(
  connected: List(String),
  reachable_candidates: List(String),
) -> #(List(String), List(String)) {
  let reachable_set =
    list.fold(reachable_candidates, dict.new(), fn(acc, node) {
      dict.insert(acc, node, Nil)
    })

  let #(reachable_rev, unreachable_rev) =
    list.fold(connected, #([], []), fn(acc, node) {
      let #(reachable, unreachable) = acc
      case dict.has_key(reachable_set, node) {
        True -> #([node, ..reachable], unreachable)
        False -> #(reachable, [node, ..unreachable])
      }
    })

  #(list.reverse(reachable_rev), list.reverse(unreachable_rev))
}

fn collect_pings(
  collector: Subject(#(String, Bool)),
  queued: List(String),
  pending: dict.Dict(String, Nil),
  workers: List(#(String, process.Pid)),
  reachable: List(String),
  unreachable: List(String),
  deadline: Int,
) -> #(List(String), List(String)) {
  case dict.is_empty(pending) {
    True -> #(reachable, unreachable)
    False -> {
      let now = monotonic_ms()
      let timeout_ms = case deadline > now {
        True -> deadline - now
        False -> 0
      }
      case process.receive(collector, timeout_ms) {
        Ok(#(node, True)) ->
          case dict.has_key(pending, node) {
            True -> {
              let next_pending = dict.delete(pending, node)
              let next_workers = drop_ping_worker(workers, node)
              let #(next_queued, next_workers) =
                maybe_spawn_next_worker(collector, queued, next_workers)
              collect_pings(
                collector,
                next_queued,
                next_pending,
                next_workers,
                [node, ..reachable],
                unreachable,
                deadline,
              )
            }
            False ->
              collect_pings(
                collector,
                queued,
                pending,
                workers,
                reachable,
                unreachable,
                deadline,
              )
          }
        Ok(#(node, False)) ->
          case dict.has_key(pending, node) {
            True -> {
              let next_pending = dict.delete(pending, node)
              let next_workers = drop_ping_worker(workers, node)
              let #(next_queued, next_workers) =
                maybe_spawn_next_worker(collector, queued, next_workers)
              collect_pings(
                collector,
                next_queued,
                next_pending,
                next_workers,
                reachable,
                [node, ..unreachable],
                deadline,
              )
            }
            False ->
              collect_pings(
                collector,
                queued,
                pending,
                workers,
                reachable,
                unreachable,
                deadline,
              )
          }
        // Deadline expired with workers still in flight. Every
        // node we have NOT yet heard from is filed as unreachable
        // so the partition stays total: a stalled ping is
        // operationally indistinguishable from "down", and the
        // caller's invariant `reachable ∪ unreachable = connected`
        // must hold even on partial timeouts.
        Error(Nil) -> {
          terminate_ping_workers(workers)
          #(reachable, list.append(unreachable, dict.keys(pending)))
        }
      }
    }
  }
}

fn seed_ping_workers(
  collector: Subject(#(String, Bool)),
  queue: List(String),
  workers: List(#(String, process.Pid)),
  slots_left: Int,
) -> #(List(String), List(#(String, process.Pid))) {
  case slots_left <= 0 {
    True -> #(queue, workers)
    False ->
      case queue {
        [] -> #(queue, workers)
        [node, ..rest] -> {
          let pid =
            process.spawn_unlinked(fn() {
              process.send(collector, #(node, ping_ffi(node)))
            })
          seed_ping_workers(
            collector,
            rest,
            [#(node, pid), ..workers],
            slots_left - 1,
          )
        }
      }
  }
}

fn maybe_spawn_next_worker(
  collector: Subject(#(String, Bool)),
  queue: List(String),
  workers: List(#(String, process.Pid)),
) -> #(List(String), List(#(String, process.Pid))) {
  case queue {
    [] -> #(queue, workers)
    [node, ..rest] -> {
      let pid =
        process.spawn_unlinked(fn() {
          process.send(collector, #(node, ping_ffi(node)))
        })
      #(rest, [#(node, pid), ..workers])
    }
  }
}

fn drop_ping_worker(
  workers: List(#(String, process.Pid)),
  node: String,
) -> List(#(String, process.Pid)) {
  list.filter(workers, fn(entry) {
    let #(worker_node, _pid) = entry
    worker_node != node
  })
}

fn terminate_ping_workers(workers: List(#(String, process.Pid))) -> Nil {
  list.each(workers, fn(entry) {
    let #(_node, pid) = entry
    let _ = exit_shutdown(pid)
    case process.is_alive(pid) {
      True -> process.kill(pid)
      False -> Nil
    }
  })
}
