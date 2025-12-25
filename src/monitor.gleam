/// Process and node monitoring.
///
/// This module provides functions to monitor processes and nodes for failure detection.
///
/// ## Handling Monitor Messages
///
/// When a monitored process terminates, the monitoring process receives a message.
/// Since this library wraps Erlang primitives, the message format is an Erlang tuple:
/// `{'DOWN', Reference, process, Pid, Reason}`.
///
/// To handle this in Gleam, you typically need to use `gleam/erlang/process` with
/// a `Selector` that can match this message, or use a custom receiver.
/// Represents a process identifier.
pub type Pid

/// Represents a unique monitor reference.
pub type Reference

type Atom

@external(erlang, "erlang", "monitor")
fn monitor_process_ffi(type_atom: Atom, pid: Pid) -> Reference

@external(erlang, "erlang", "monitor_node")
fn monitor_node_ffi(node: Atom, flag: Bool) -> Bool

@external(erlang, "erlang", "self")
fn self_ffi() -> Pid

@external(erlang, "erlang", "demonitor")
fn demonitor_ffi(ref: Reference) -> Bool

@external(erlang, "monitor_ffi", "to_atom")
fn to_atom(name: String) -> Atom

/// Get the Pid of the current process.
pub fn self() -> Pid {
  self_ffi()
}

/// Monitor a process. Returns a monitor reference.
/// When the monitored process terminates, a message of the form
/// {'DOWN', Reference, process, Pid, Reason} is sent to the caller.
pub fn monitor(pid: Pid) -> Reference {
  monitor_process_ffi(to_atom("process"), pid)
}

/// Stop monitoring a process.
/// Returns True if the monitor was found and removed.
pub fn demonitor(ref: Reference) -> Bool {
  demonitor_ffi(ref)
}

/// Monitor a node for up/down events.
/// Pass True to start monitoring, False to stop.
/// Note: This function requires the current node to be distributed,
/// otherwise it will raise a 'notalive' error.
pub fn monitor_node(node: String, flag: Bool) -> Bool {
  monitor_node_ffi(to_atom(node), flag)
}
