/// Process groups for pub/sub and worker pools.
///
/// This module wraps Erlang's `:pg` (process groups) module to provide
/// cluster-wide process grouping with join/leave/broadcast semantics.
///
/// Groups are distributed and eventually consistent. Processes can join multiple
/// groups, and groups can span multiple nodes.
pub type GroupError {
  /// A group operation failed (e.g. invalid group name or internal error).
  GroupFailed(String)
}

type Dynamic

pub type Pid

@external(erlang, "groups_ffi", "join")
fn join_ffi(group: String, pid: Pid) -> Dynamic

@external(erlang, "groups_ffi", "leave")
fn leave_ffi(group: String, pid: Pid) -> Dynamic

@external(erlang, "groups_ffi", "members")
fn members_ffi(group: String) -> Dynamic

@external(erlang, "groups_ffi", "broadcast")
fn broadcast_ffi(group: String, msg: a) -> Dynamic

@external(erlang, "groups_ffi", "is_ok_atom")
fn is_ok_atom(value: Dynamic) -> Bool

@external(erlang, "groups_ffi", "get_error_reason")
fn get_error_reason(value: Dynamic) -> String

@external(erlang, "groups_ffi", "unwrap_members")
fn unwrap_members(value: Dynamic) -> List(Pid)

/// Join a process to a named group.
pub fn join(group: String, pid: Pid) -> Result(Nil, GroupError) {
  let res = join_ffi(group, pid)
  case is_ok_atom(res) {
    True -> Ok(Nil)
    False -> Error(GroupFailed(get_error_reason(res)))
  }
}

/// Remove a process from a named group.
pub fn leave(group: String, pid: Pid) -> Result(Nil, GroupError) {
  let res = leave_ffi(group, pid)
  case is_ok_atom(res) {
    True -> Ok(Nil)
    False -> Error(GroupFailed(get_error_reason(res)))
  }
}

/// Get the list of member pids in a group.
pub fn members(group: String) -> List(Pid) {
  let res = members_ffi(group)
  unwrap_members(res)
}

/// Broadcast a message to all members of a group.
pub fn broadcast(group: String, msg: a) -> Result(Nil, GroupError) {
  let res = broadcast_ffi(group, msg)
  case is_ok_atom(res) {
    True -> Ok(Nil)
    False -> Error(GroupFailed(get_error_reason(res)))
  }
}
