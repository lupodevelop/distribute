/// Typed process subject wrapper
///
/// `Subject(a)` represents a process that accepts messages of type `a`.
/// It is a compile-time wrapper around an opaque `Pid` value.
/// The goal is to make the public API explicitly typed: functions operating
/// on `Subject(a)` guarantee at compile-time the type of messages being sent.
import distribute/registry

/// The system uses the `Pid` type from `distribute/registry` to remain
/// consistent with the rest of the codebase.
pub type Pid =
  registry.Pid

/// `Subject(a)` includes an optional `tag` (String) used as a contract
/// identifier (e.g., codec id, module name). The tag is informational
/// and serves for debugging and simple runtime assertions without
/// altering the underlying `Pid`.
pub type Subject(a) {
  Subject(Pid, String)
}

/// Returns the underlying `Pid` for interoperating with FFI or
/// legacy functions that require the raw value.
pub fn to_pid(subject: Subject(a)) -> Pid {
  case subject {
    Subject(pid, _) -> pid
  }
}

/// Creates a `Subject` from a raw `Pid` without any tag. UNSAFE: this
/// performs no validation and should be used only when the caller can
/// guarantee the process speaks the expected message contract.
pub fn from_pid_unsafe(pid: Pid) -> Subject(a) {
  Subject(pid, "")
}

/// Creates a `Subject` from a raw `Pid` and attaches a developer-supplied tag.
/// The tag is purely informational and can be asserted later with `assert_tag`.
pub fn from_pid_with_tag(pid: Pid, tag: String) -> Subject(a) {
  Subject(pid, tag)
}

/// Unsafe cast between subject types. Keeps the same `Pid` and `tag`.
/// This is a last-resort escape hatch; prefer creating properly-typed
/// subjects from spawn/register helpers.
pub fn unsafe_cast(subject: Subject(b)) -> Subject(a) {
  case subject {
    Subject(pid, tag) -> Subject(pid, tag)
  }
}

/// Returns the tag attached to a `Subject`.
pub fn tag(subject: Subject(a)) -> String {
  case subject {
    Subject(_, t) -> t
  }
}

/// Checks if two subjects refer to the same underlying pid.
pub fn same(subject1: Subject(a), subject2: Subject(b)) -> Bool {
  to_pid(subject1) == to_pid(unsafe_cast(subject2))
}

/// Asserts that a `Subject` carries the expected tag. Returns `Ok(subject)`
/// if the tag matches, otherwise `Error(reason)`. This provides a small
/// runtime check that callers can use when wrapping external pids.
pub fn assert_tag(
  subject: Subject(a),
  expected: String,
) -> Result(Subject(a), String) {
  let actual = tag(subject)
  case actual == expected {
    True -> Ok(subject)
    False ->
      Error("tag_mismatch: expected=" <> expected <> ", actual=" <> actual)
  }
}

// ============================================================================
// Typed registry helpers
// ============================================================================

/// Register a typed subject under a global name.
pub fn register_typed(
  name: String,
  subject: Subject(a),
) -> Result(Nil, registry.RegisterError) {
  registry.register(name, to_pid(subject))
}

/// Look up a globally-registered name and return a typed `Subject(a)` with
/// the provided `tag`. Returns `Ok(subject)` if found, `Error(Nil)` if not.
pub fn whereis_typed(name: String, tag: String) -> Result(Subject(a), Nil) {
  case registry.whereis(name) {
    Ok(pid) -> Ok(from_pid_with_tag(pid, tag))
    Error(_) -> Error(Nil)
  }
}

/// Create a typed `Subject(a)` from an external `Pid` and register it in the
/// global registry under `name`. Returns the registration result.
pub fn register_pid_with_tag(
  name: String,
  pid: Pid,
  tag: String,
) -> Result(Nil, registry.RegisterError) {
  let subject = from_pid_with_tag(pid, tag)
  register_typed(name, subject)
}
