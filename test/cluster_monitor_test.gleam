import distribute/cluster
import distribute/cluster_monitor
import gleam/erlang/process
import gleeunit
import gleeunit/should

/// Send a raw Erlang term directly to a Pid. This is how net_kernel delivers
/// {nodeup, Node} and {nodedown, Node}. they bypass any Subject mechanism
/// and land as raw messages on the process mailbox, caught by `select_other`.
@external(erlang, "cluster_monitor_test_ffi", "send_nodedown_event")
fn send_nodedown_event(pid: process.Pid, node_name: String) -> Nil

@external(erlang, "cluster_monitor_test_ffi", "send_nodeup_event")
fn send_nodeup_event(pid: process.Pid, node_name: String) -> Nil

pub fn main() {
  gleeunit.main()
}

pub fn monitor_start_stop_test() {
  let assert Ok(monitor) = cluster.start_monitor()
  // Should be alive
  should.be_true(process.is_alive(
    process.subject_owner(monitor) |> should.be_ok(),
  ))

  // Cleanup
  process.send_exit(process.subject_owner(monitor) |> should.be_ok())
}

pub fn monitor_subscription_test() {
  let assert Ok(monitor) = cluster.start_monitor()
  let event_subj = process.new_subject()

  cluster.subscribe(monitor, event_subj)

  // Simulate Erlang raw {nodedown, 'other_node@127.0.0.1'} message sent directly
  // to the actor's PID. same as net_kernel would deliver it.
  let assert Ok(monitor_pid) = process.subject_owner(monitor)
  send_nodedown_event(monitor_pid, "other_node@127.0.0.1")

  // Give the actor time to process the event and deliver to subscribers
  process.sleep(100)

  let assert Ok(event) = process.receive(event_subj, 1000)
  should.equal(event, cluster_monitor.NodeDown("other_node@127.0.0.1"))

  process.send_exit(monitor_pid)
}

pub fn monitor_prunes_dead_subscribers_proactively_test() {
  // O(1) pruning regression: a subscriber whose owner dies must be
  // dropped via the BEAM monitor, NOT just at the next nodeup/nodedown.
  // We verify by spawning a transient subscriber, killing it, then
  // sending a nodeup and checking the monitor still functions and the
  // dead subscriber receives nothing.
  let assert Ok(monitor) = cluster.start_monitor()
  let assert Ok(monitor_pid) = process.subject_owner(monitor)

  // Spawn a process that subscribes and immediately sleeps, holding its
  // subject so we can introspect.
  let live_events = process.new_subject()
  cluster.subscribe(monitor, live_events)

  // Spawn a transient subscriber that will die.
  let dead_events_owner =
    process.spawn(fn() {
      let dead_events = process.new_subject()
      cluster.subscribe(monitor, dead_events)
      // Hold the subject for a moment then exit normally.
      process.sleep(50)
    })
  process.sleep(20)
  // Wait until the transient subscriber has exited.
  process.sleep(80)
  should.be_false(process.is_alive(dead_events_owner))

  // Push an event. the monitor must not crash, and the live subscriber
  // must still receive it. The dead one is gone (proactive prune).
  send_nodeup_event(monitor_pid, "live@127.0.0.1")
  let assert Ok(event) = process.receive(live_events, 500)
  should.equal(event, cluster_monitor.NodeUp("live@127.0.0.1"))

  process.send_exit(monitor_pid)
}

pub fn monitor_subscribe_dedup_test() {
  // Subscribing the same Subject twice must not double-deliver events;
  // a single `nodeup` produces exactly one event per logical
  // subscriber, no matter how many times `subscribe/2` was called for
  // the same Subject.
  let assert Ok(monitor) = cluster.start_monitor()
  let assert Ok(monitor_pid) = process.subject_owner(monitor)
  let events = process.new_subject()

  cluster.subscribe(monitor, events)
  cluster.subscribe(monitor, events)
  cluster.subscribe(monitor, events)

  send_nodeup_event(monitor_pid, "dedup@127.0.0.1")

  // Expect exactly one event, then a clean timeout.
  let assert Ok(_first) = process.receive(events, 500)
  let extra = process.receive(events, 100)
  should.be_error(extra)

  process.send_exit(monitor_pid)
}

pub fn monitor_observed_unknown_message_hook_test() {
  // The diagnostic hook fires for messages the monitor cannot classify.
  let unknown_subj = process.new_subject()
  let assert Ok(monitor) =
    cluster_monitor.start_observed(fn(dyn) { process.send(unknown_subj, dyn) })
  let assert Ok(monitor_pid) = process.subject_owner(monitor)

  // Inject a garbage term into the mailbox (not a Subject message,
  // not a node event).
  send_garbage_term(monitor_pid)

  let result = process.receive(unknown_subj, 500)
  should.be_ok(result)

  process.send_exit(monitor_pid)
}

@external(erlang, "cluster_monitor_test_ffi", "send_garbage_term")
fn send_garbage_term(pid: process.Pid) -> Nil

pub fn monitor_nodeup_test() {
  let assert Ok(monitor) = cluster.start_monitor()
  let event_subj = process.new_subject()

  cluster.subscribe(monitor, event_subj)

  let assert Ok(monitor_pid) = process.subject_owner(monitor)
  send_nodeup_event(monitor_pid, "fresh_node@127.0.0.1")

  process.sleep(100)

  let assert Ok(event) = process.receive(event_subj, 1000)
  should.equal(event, cluster_monitor.NodeUp("fresh_node@127.0.0.1"))

  process.send_exit(monitor_pid)
}
