/// Lightweight leader election inspired by Raft.
///
/// This module provides two approaches to leader election:
///
/// 1. **Stateful Service**: A background process that implements term-based voting
///    and heartbeats (via `start_service`, `request_vote`, etc.).
/// 2. **Stateless Deterministic**: A simple function (`elect`) that picks a leader
///    based on the lexicographically largest node among the alive members.
import distribute/cluster/membership
import gleam/list
import gleam/option
import gleam/string

@external(erlang, "raft_ffi", "start")
fn raft_start_ffi() -> Nil

@external(erlang, "raft_ffi", "stop")
fn raft_stop_ffi() -> Nil

@external(erlang, "raft_ffi", "current_term")
fn current_term_ffi() -> Int

@external(erlang, "raft_ffi", "get_leader")
fn get_leader_ffi() -> String

@external(erlang, "raft_ffi", "am_i_leader")
fn am_i_leader_ffi() -> Bool

@external(erlang, "raft_ffi", "start_election")
fn start_election_ffi() -> Nil

@external(erlang, "raft_ffi", "request_vote")
fn request_vote_ffi(term: Int, candidate: String) -> Bool

@external(erlang, "raft_ffi", "heartbeat")
fn heartbeat_ffi(leader: String) -> Nil

/// Start the Raft-lite election service.
/// This spawns a background process that handles election timeouts and heartbeats.
pub fn start_service() -> Nil {
  raft_start_ffi()
}

/// Stop the Raft-lite election service.
pub fn stop_service() -> Nil {
  raft_stop_ffi()
}

/// Get the current election term.
pub fn current_term() -> Int {
  current_term_ffi()
}

/// Check if this node is currently the leader.
pub fn am_i_leader() -> Bool {
  am_i_leader_ffi()
}

/// Trigger a new election. The node will become a candidate and request votes.
pub fn start_election() -> Nil {
  start_election_ffi()
}

/// Request a vote from a peer for the given term and candidate.
/// Returns True if the vote was granted.
pub fn request_vote(term: Int, candidate: String) -> Bool {
  request_vote_ffi(term, candidate)
}

/// Send a heartbeat as leader to maintain leadership.
pub fn send_heartbeat(leader: String) -> Nil {
  heartbeat_ffi(leader)
}

/// Get the current leader from the Raft service.
/// Returns the leader node name, or empty string if no leader.
pub fn get_raft_leader() -> option.Option(String) {
  let leader = get_leader_ffi()
  case leader {
    "" -> option.None
    _ -> option.Some(leader)
  }
}

/// A tiny "raft-lite" helper. This is a lightweight, deterministic
/// leader selection module built on top of `membership`. It does not
/// implement full Raft semantics (log replication, terms, safety), but
/// provides a small and well-tested way to elect a leader among alive
/// nodes. It is useful as a pragmatic default and as a base for a
/// future full Raft implementation.
pub type ElectionResult {
  Leader(String)
  NoLeader
}

/// Elect a leader from the current alive members. The election rule is
/// deterministic: choose the lexicographically largest node returned by
/// `membership.alive()`. Returns `NoLeader` when there are no alive nodes.
pub fn elect() -> ElectionResult {
  let alive = membership.alive()
  case alive {
    [] -> NoLeader
    [first, ..] ->
      // Use list.max with string.compare to pick a deterministic leader
      case list.max(alive, string.compare) {
        Ok(leader) -> Leader(leader)
        Error(_) -> Leader(first)
      }
  }
}

/// Convenience: return the current leader as `Option(String)`.
pub fn current_leader() -> option.Option(String) {
  case elect() {
    Leader(n) -> option.Some(n)
    NoLeader -> option.None
  }
}
