/// SWIM-like cluster membership with failure detection and gossip.
///
/// This module provides a background service that periodically probes cluster nodes,
/// maintains their status (alive/suspect/dead), and propagates membership changes via gossip.
///
/// It offers a deterministic view of the cluster membership, which can be used
/// for building higher-level distributed systems (like leader election or consistent hashing).
///
/// ## Features
///
/// - Direct and indirect ping for failure detection
/// - Incarnation numbers to handle false positives
/// - Anti-entropy gossip to propagate membership state
/// - Metrics for observability (ping success/fail counts)
import gleam/list

pub type Status {
  Alive
  Suspect
  Dead
}

@external(erlang, "membership_ffi", "start")
fn membership_start_ffi(interval_ms: Int) -> Nil

@external(erlang, "membership_ffi", "stop")
fn membership_stop_ffi() -> Nil

@external(erlang, "membership_ffi", "members_with_status")
fn members_with_status_ffi() -> List(#(String, String, Int, Int))

@external(erlang, "membership_ffi", "alive")
fn alive_ffi() -> List(String)

@external(erlang, "membership_ffi", "suspect")
fn suspect_ffi() -> List(String)

@external(erlang, "membership_ffi", "current_leader")
fn current_leader_ffi() -> String

@external(erlang, "membership_ffi", "metrics")
fn metrics_ffi() -> List(#(String, Int))

@external(erlang, "membership_ffi", "metrics_increment")
fn metrics_increment_ffi(name: String) -> Nil

@external(erlang, "membership_ffi", "metrics_get")
fn metrics_get_ffi(name: String) -> Int

/// Start the background membership poller with interval in milliseconds.
pub fn start_service(interval_ms: Int) -> Nil {
  membership_start_ffi(interval_ms)
}

/// Stop the background membership service.
pub fn stop_service() -> Nil {
  membership_stop_ffi()
}

/// Return the list of nodes and their status and incarnation according to the background poller.
/// Returns a list of tuples `#(NodeName, Status, Incarnation)`.
pub fn members_with_status() -> List(#(String, Status, Int)) {
  let raw = members_with_status_ffi()
  list.map(raw, fn(item) {
    case item {
      #(n, s, inc, _ts) ->
        case s {
          "alive" -> #(n, Alive, inc)
          "suspect" -> #(n, Suspect, inc)
          _ -> #(n, Dead, inc)
        }
    }
  })
}

/// Get a list of all nodes currently considered 'alive'.
pub fn alive() -> List(String) {
  alive_ffi()
}

/// Get a list of all nodes currently considered 'suspect' (potentially failing).
pub fn suspect() -> List(String) {
  suspect_ffi()
}

import gleam/option

pub fn current_leader() -> option.Option(String) {
  let s = current_leader_ffi()
  case s {
    "" -> option.None
    _ -> option.Some(s)
  }
}

/// Return internal metrics as a list of `(name, value)` tuples.
pub fn metrics() -> List(#(String, Int)) {
  metrics_ffi()
}

/// Increment a named metric (useful in tests).
pub fn metrics_inc(name: String) -> Nil {
  metrics_increment_ffi(name)
}

pub fn metrics_get(name: String) -> Int {
  metrics_get_ffi(name)
}
