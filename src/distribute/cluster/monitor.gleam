import distribute/internal/telemetry
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/result

pub type ClusterEvent {
  NodeUp(node: String)
  NodeDown(node: String)
}

@external(erlang, "cluster_ffi", "nodeup_atom")
fn nodeup_atom() -> Dynamic

@external(erlang, "cluster_ffi", "nodedown_atom")
fn nodedown_atom() -> Dynamic

@external(erlang, "cluster_ffi", "monitor_nodes")
fn monitor_nodes_ffi(flag: Bool) -> Dynamic

@external(erlang, "cluster_ffi", "atom_to_string")
fn atom_to_string_ffi(atom: Dynamic) -> String

@external(erlang, "cluster_ffi", "get_node_from_tuple")
fn get_node_from_tuple_ffi(msg: Dynamic) -> Dynamic

pub type ControlMessage {
  Stop
  InternalEvent(ClusterEvent)
}

/// Subscribe to cluster topology events.
/// Events are forwarded to the provided user_subject.
/// Returns a control subject to stop the subscription.
pub fn subscribe(
  user_subject: Subject(ClusterEvent),
) -> Result(Subject(ControlMessage), actor.StartError) {
  actor.new_with_initialiser(5000, fn(self_subject) {
    // Start node monitoring in the actor's context
    monitor_nodes_ffi(True)

    // Select standard nodeup/nodedown Erlang messages
    let selector =
      process.new_selector()
      |> process.select(self_subject)
      |> process.select_record(nodeup_atom(), 1, fn(msg) {
        let node_atom = get_node_from_tuple_ffi(msg)
        InternalEvent(NodeUp(atom_to_string_ffi(node_atom)))
      })
      |> process.select_record(nodedown_atom(), 1, fn(msg) {
        let node_atom = get_node_from_tuple_ffi(msg)
        InternalEvent(NodeDown(atom_to_string_ffi(node_atom)))
      })

    Ok(
      actor.initialised(user_subject)
      |> actor.selecting(selector)
      |> actor.returning(self_subject),
    )
  })
  |> actor.on_message(fn(state, msg) {
    case msg {
      InternalEvent(event) -> {
        case event {
          NodeUp(node) -> telemetry.emit_node_event(node, "up")
          NodeDown(node) -> telemetry.emit_node_event(node, "down")
        }
        process.send(state, event)
        actor.continue(state)
      }
      Stop -> actor.stop()
    }
  })
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

/// Stop the monitoring for a specific subscription.
pub fn unsubscribe(monitor_subject: Subject(ControlMessage)) -> Nil {
  process.send(monitor_subject, Stop)
}
