import distribute/cluster/monitor.{NodeDown, NodeUp}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleeunit/should

@external(erlang, "cluster_ffi", "simulate_node_event")
fn simulate_node_event(pid: process.Pid, tag: Dynamic, node: String) -> Nil

@external(erlang, "cluster_ffi", "nodeup_atom")
fn nodeup_atom() -> Dynamic

@external(erlang, "cluster_ffi", "nodedown_atom")
fn nodedown_atom() -> Dynamic

pub fn monitor_lifecycle_test() {
  let subj = process.new_subject()
  let assert Ok(monitor_subj) = monitor.subscribe(subj)
  
  // Stopping the monitor
  monitor.unsubscribe(monitor_subj)
  
  // Wait a bit to ensure it stops
  process.sleep(100)
  
  // Actor should be dead
  should.be_false(process.is_alive(process.subject_owner(monitor_subj) |> should.be_ok()))
}

pub fn monitor_node_events_test() {
  let subj = process.new_subject()
  let assert Ok(monitor_subj) = monitor.subscribe(subj)
  let monitor_pid = process.subject_owner(monitor_subj) |> should.be_ok()
  
  // Simulate native Erlang nodeup message
  simulate_node_event(monitor_pid, nodeup_atom(), "slave@127.0.0.1")
  
  // Verify it's received as Gleam variant
  let assert Ok(event1) = process.receive(subj, 500)
  should.equal(event1, NodeUp("slave@127.0.0.1"))
  
  // Simulate native Erlang nodedown message
  simulate_node_event(monitor_pid, nodedown_atom(), "slave@127.0.0.1")
  
  // Verify it's received as Gleam variant
  let assert Ok(event2) = process.receive(subj, 500)
  should.equal(event2, NodeDown("slave@127.0.0.1"))
  
  monitor.unsubscribe(monitor_subj)
}

pub fn monitor_events_signature_test() {
  // Just checking types and basic construction
  let up = NodeUp("test@host")
  let down = NodeDown("test@host")
  
  case up {
    NodeUp(n) -> should.equal(n, "test@host")
  }

  case down {
    NodeDown(n) -> should.equal(n, "test@host")
  }
}
