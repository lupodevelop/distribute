/// Process groups for pub/sub and worker pools.
///
/// This module wraps Erlang's `:pg` (process groups) module to provide
/// cluster-wide process grouping with join/leave/broadcast semantics.
///
/// Groups are distributed and eventually consistent. Processes can join multiple
/// groups, and groups can span multiple nodes.
///
/// For type-safe group operations, use the `_typed` variants which require
/// `Encoder`/`Decoder` and work with `Subject(a)` instead of raw Pids.
import distribute/codec
import distribute/registry
import distribute/typed_process
import gleam/list

pub type GroupError {
  /// A group operation failed (e.g. invalid group name or internal error).
  GroupFailed(String)
  /// Message encoding failed during broadcast.
  EncodeFailed(codec.EncodeError)
  /// Member conversion failed (e.g. invalid Pid to Subject).
  MemberConversionFailed(String)
}

type Dynamic

pub type Pid =
  registry.Pid

@external(erlang, "groups_ffi", "join")
fn join_ffi(group: String, pid: Pid) -> Dynamic

@external(erlang, "groups_ffi", "leave")
fn leave_ffi(group: String, pid: Pid) -> Dynamic

@external(erlang, "groups_ffi", "members")
fn members_ffi(group: String) -> Dynamic

@external(erlang, "groups_ffi", "broadcast")
fn broadcast_ffi(group: String, msg: a) -> Dynamic

@external(erlang, "groups_ffi", "broadcast_binary")
fn broadcast_binary_ffi(group: String, binary_msg: BitArray) -> Dynamic

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

// ============================================================================
// Typed groups API
// ============================================================================

/// Join a typed subject to a named group.
pub fn join_typed(
  group: String,
  subject: typed_process.Subject(a),
) -> Result(Nil, GroupError) {
  join(group, typed_process.to_pid(subject))
}

/// Remove a typed subject from a named group.
pub fn leave_typed(
  group: String,
  subject: typed_process.Subject(a),
) -> Result(Nil, GroupError) {
  leave(group, typed_process.to_pid(subject))
}

/// Get the list of typed members in a group.
/// Each Pid is wrapped as a Subject with the provided tag.
/// Use this when you know all members of the group share the same message type.
pub fn members_typed(
  group: String,
  tag: String,
) -> List(typed_process.Subject(a)) {
  members(group)
  |> list.map(fn(pid) { typed_process.from_pid_with_tag(pid, tag) })
}

/// Broadcast a typed message to all members of a group.
/// The message is encoded using the provided encoder before sending.
pub fn broadcast_typed(
  group: String,
  msg: a,
  encoder: codec.Encoder(a),
) -> Result(Nil, GroupError) {
  case codec.encode(encoder, msg) {
    Ok(binary_msg) -> {
      let res = broadcast_binary_ffi(group, binary_msg)
      case is_ok_atom(res) {
        True -> Ok(Nil)
        False -> Error(GroupFailed(get_error_reason(res)))
      }
    }
    Error(encode_error) -> Error(EncodeFailed(encode_error))
  }
}
