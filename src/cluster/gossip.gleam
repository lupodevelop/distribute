/// Gossip protocol for membership state propagation.
///
/// This module provides functions for gossip-based anti-entropy,
/// allowing nodes to exchange membership state and converge on
/// a consistent view of the cluster.
///
/// ## Protocol
///
/// Each node maintains a local view of membership state (node, status, incarnation).
/// Periodically, nodes exchange their views via gossip:
///
/// 1. Sender selects random peers and sends its local view
/// 2. Receiver merges the incoming view with its local state
/// 3. Merge rules: higher incarnation wins; on tie, alive > suspect > dead
import gleam/list

/// Represents the status of a node in the cluster.
pub type NodeStatus {
  Alive
  Suspect
  Dead
}

/// A gossip entry representing a node's state.
pub type GossipEntry {
  GossipEntry(
    node: String,
    status: NodeStatus,
    incarnation: Int,
    timestamp: Int,
  )
}

/// Gossip message containing a list of entries.
pub type GossipMessage {
  GossipMessage(entries: List(GossipEntry))
}

@external(erlang, "gossip_ffi", "local_view")
fn local_view_ffi() -> List(#(String, String, Int, Int))

@external(erlang, "gossip_ffi", "send_gossip")
fn send_gossip_ffi(
  node: String,
  entries: List(#(String, String, Int, Int)),
) -> Nil

@external(erlang, "gossip_ffi", "receive_gossip")
fn receive_gossip_ffi(entries: List(#(String, String, Int, Int))) -> Nil

@external(erlang, "gossip_ffi", "merge_entry")
fn merge_entry_ffi(
  node: String,
  status: String,
  incarnation: Int,
  timestamp: Int,
) -> Nil

/// Get the local gossip view as a list of entries.
pub fn local_view() -> List(GossipEntry) {
  let raw = local_view_ffi()
  list.map(raw, fn(item) {
    let #(node, status_str, inc, ts) = item
    let status = case status_str {
      "alive" -> Alive
      "suspect" -> Suspect
      _ -> Dead
    }
    GossipEntry(node: node, status: status, incarnation: inc, timestamp: ts)
  })
}

/// Convert a status to string for FFI.
fn status_to_string(status: NodeStatus) -> String {
  case status {
    Alive -> "alive"
    Suspect -> "suspect"
    Dead -> "dead"
  }
}

/// Send gossip to a specific node.
pub fn send_to(node: String, entries: List(GossipEntry)) -> Nil {
  let raw =
    list.map(entries, fn(e) {
      #(e.node, status_to_string(e.status), e.incarnation, e.timestamp)
    })
  send_gossip_ffi(node, raw)
}

/// Receive and merge gossip from a peer.
pub fn receive_and_merge(entries: List(GossipEntry)) -> Nil {
  let raw =
    list.map(entries, fn(e) {
      #(e.node, status_to_string(e.status), e.incarnation, e.timestamp)
    })
  receive_gossip_ffi(raw)
}

/// Merge a single entry into the local view.
/// Uses the rule: higher incarnation wins; on tie, alive > suspect > dead.
pub fn merge_single(entry: GossipEntry) -> Nil {
  merge_entry_ffi(
    entry.node,
    status_to_string(entry.status),
    entry.incarnation,
    entry.timestamp,
  )
}

/// Pick random peers from a list of nodes.
pub fn pick_random_peers(nodes: List(String), count: Int) -> List(String) {
  list.take(nodes, count)
}
