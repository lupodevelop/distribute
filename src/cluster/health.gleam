/// Health check utilities for cluster nodes.
///
/// This module provides functions to check the health of the local node
/// and the overall cluster, based on membership status and connectivity.
import cluster/membership
import gleam/list
import gleam/option

/// Health status of the cluster.
pub type ClusterHealth {
  /// All nodes are alive and healthy.
  Healthy
  /// Some nodes are suspected or unreachable.
  Degraded(suspect_count: Int, dead_count: Int)
  /// No other nodes are connected.
  Isolated
}

/// Node health summary.
pub type NodeHealth {
  NodeHealthy
  NodeUnhealthy(reason: String)
}

/// Check if the local node is healthy.
/// A node is considered healthy if:
/// - The membership service is running
/// - It can see at least one other node (or is intentionally standalone)
pub fn is_healthy() -> Bool {
  let suspects = membership.suspect()
  // Healthy if no nodes are in suspect state
  suspects == []
}

/// Get detailed health status of the local node.
pub fn node_health() -> NodeHealth {
  let suspects = membership.suspect()
  case suspects {
    [] -> NodeHealthy
    _ -> NodeUnhealthy(reason: "Nodes in suspect state")
  }
}

/// Get the overall cluster health status.
pub fn cluster_health() -> ClusterHealth {
  let members = membership.members_with_status()
  let alive_count =
    list.length(
      list.filter(members, fn(m) {
        case m {
          #(_, membership.Alive, _) -> True
          _ -> False
        }
      }),
    )
  let suspect_count =
    list.length(
      list.filter(members, fn(m) {
        case m {
          #(_, membership.Suspect, _) -> True
          _ -> False
        }
      }),
    )
  let dead_count =
    list.length(
      list.filter(members, fn(m) {
        case m {
          #(_, membership.Dead, _) -> True
          _ -> False
        }
      }),
    )

  case alive_count, suspect_count, dead_count {
    0, 0, 0 -> Isolated
    _, 0, 0 -> Healthy
    _, s, d -> Degraded(suspect_count: s, dead_count: d)
  }
}

/// Get count of alive nodes.
pub fn alive_count() -> Int {
  list.length(membership.alive())
}

/// Get count of suspect nodes.
pub fn suspect_count() -> Int {
  list.length(membership.suspect())
}

/// Check if we have quorum (majority of expected nodes alive).
/// `expected_nodes` is the total expected cluster size including self.
pub fn has_quorum(expected_nodes: Int) -> Bool {
  let alive = alive_count() + 1
  // +1 for self
  alive > expected_nodes / 2
}

/// Get the current leader if available.
pub fn current_leader() -> option.Option(String) {
  membership.current_leader()
}
