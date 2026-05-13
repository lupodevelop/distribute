//// Cluster-event monitor.
////
//// Wraps Erlang's `net_kernel:monitor_nodes/1` in a typed actor.
//// Subscribers register a `Subject(ClusterEvent)` and receive
//// `NodeUp(name)` / `NodeDown(name)` events for the lifetime of the
//// monitor. Subscribers are pruned proactively on owner death via
//// `process.monitor`, so the subscriber list is bounded under churn
//// regardless of cluster activity.

import gleam/dynamic
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor
import gleam/result

pub type ClusterEvent {
  NodeUp(node: String)
  NodeDown(node: String)
}

/// User-facing protocol for the cluster monitor.
///
/// `opaque` so external code cannot construct internal variants (e.g.
/// `SubscriberDown`, `ClassifiedNodeEvent`, `UnknownMessage`) and
/// inject them through `process.send` to corrupt the monitor's state.
/// The public surface is `subscribe/2` and `unsubscribe/2`, which build
/// `Subscribe`/`Unsubscribe` themselves. Callers never need to name
/// the constructors directly.
pub opaque type Message {
  Subscribe(Subject(ClusterEvent))
  Unsubscribe(Subject(ClusterEvent))
  /// Internal: a process the monitor was watching went down. The selector
  /// rewrites the raw `DOWN` into this variant so `handle_message` can
  /// prune in O(1) regardless of cluster activity.
  SubscriberDown(process.Pid)
  /// Internal: a `nodeup`/`nodedown` raw term that the selector
  /// successfully classified into a typed `ClusterEvent`. Carrying the
  /// already-typed event avoids re-decoding inside `handle_message`.
  ClassifiedNodeEvent(ClusterEvent)
  /// Internal: anything that was neither a valid Subject message nor a
  /// recognised cluster event. Surfaced to the optional unknown-message
  /// hook for observability instead of being silently dropped.
  UnknownMessage(dynamic.Dynamic)
}

type Subscriber {
  Subscriber(
    subject: Subject(ClusterEvent),
    owner: process.Pid,
    monitor: process.Monitor,
  )
}

type State {
  State(subscribers: List(Subscriber))
}

@external(erlang, "cluster_ffi", "monitor_nodes")
fn monitor_nodes_ffi(flag: Bool) -> Nil

@external(erlang, "cluster_ffi", "decode_node_event")
fn decode_node_event(dyn: dynamic.Dynamic) -> Result(#(String, String), Nil)

pub fn start() -> Result(Subject(Message), actor.StartError) {
  start_observed(fn(_) { Nil })
}

/// Like `start`, but fires `on_unknown_msg(dyn)` whenever the monitor
/// receives a mailbox term it cannot classify as a Subject message or
/// a recognised `nodeup`/`nodedown` event. Useful as a diagnostic hook
/// Silent drops in cluster discovery are a debugging nightmare.
pub fn start_observed(
  on_unknown_msg: fn(dynamic.Dynamic) -> Nil,
) -> Result(Subject(Message), actor.StartError) {
  actor.new_with_initialiser(5000, fn(self) {
    // Start monitoring node events (messages sent to THIS process)
    monitor_nodes_ffi(True)

    let selector =
      process.new_selector()
      |> process.select(self)
      // Subscriber owners are monitored on Subscribe; their `DOWN`
      // messages are rewritten here into a typed `SubscriberDown(pid)`
      // for O(1) pruning, regardless of cluster activity.
      |> process.select_monitors(fn(down) {
        case down {
          process.ProcessDown(pid:, ..) -> SubscriberDown(pid)
          // PortDown should never happen here (we only monitor processes)
          // but the type forces us to handle it; surface as unknown.
          process.PortDown(..) ->
            UnknownMessage(dynamic.string("unexpected port down"))
        }
      })
      // `select_other` classifies non-Subject mailbox terms once at the
      // selector boundary. Recognised node events arrive at
      // `handle_message` as `ClassifiedNodeEvent(typed)`, no second
      // decode needed. Anything that fails classification surfaces as
      // `UnknownMessage` for the diagnostic hook.
      |> process.select_other(fn(dyn) {
        case classify_event(dyn) {
          Ok(event) -> ClassifiedNodeEvent(event)
          Error(Nil) -> UnknownMessage(dyn)
        }
      })

    actor.initialised(State(subscribers: []))
    |> actor.selecting(selector)
    |> actor.returning(self)
    |> Ok()
  })
  |> actor.on_message(fn(state, msg) {
    handle_message(state, msg, on_unknown_msg)
  })
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

fn handle_message(
  state: State,
  msg: Message,
  on_unknown_msg: fn(dynamic.Dynamic) -> Nil,
) -> actor.Next(State, Message) {
  case msg {
    Subscribe(sub) -> {
      case process.subject_owner(sub) {
        Ok(pid) -> {
          // Reject duplicate subscriptions for the same Subject. Without
          // dedup, every duplicate `subscribe/2` call would add another
          // entry plus another `process.monitor`, producing duplicate
          // event deliveries on every cluster event AND duplicate
          // `SubscriberDown` messages when the owner finally dies.
          case list.any(state.subscribers, fn(s) { s.subject == sub }) {
            True -> actor.continue(state)
            False -> {
              // Monitor the subscriber's owner. When it dies, BEAM
              // delivers a DOWN message that our selector rewrites into
              // `SubscriberDown`, letting us prune in O(1) without
              // waiting for the next node event.
              let mon = process.monitor(pid)
              let subscriber =
                Subscriber(subject: sub, owner: pid, monitor: mon)
              actor.continue(
                State(subscribers: [subscriber, ..state.subscribers]),
              )
            }
          }
        }
        // Owner already dead. Don't add, don't monitor.
        Error(Nil) -> actor.continue(state)
      }
    }
    Unsubscribe(sub) -> {
      // Demonitor anything we are about to drop, then filter.
      let kept =
        list.filter(state.subscribers, fn(s) {
          case s.subject == sub {
            True -> {
              process.demonitor_process(s.monitor)
              False
            }
            False -> True
          }
        })
      actor.continue(State(subscribers: kept))
    }
    SubscriberDown(pid) -> {
      // Proactive cleanup: a subscriber owner has gone DOWN. Drop every
      // entry pointing at that PID. Typically one, but tolerate dupes.
      let kept = list.filter(state.subscribers, fn(s) { s.owner != pid })
      actor.continue(State(subscribers: kept))
    }
    ClassifiedNodeEvent(event) -> {
      // Pure broadcast. No defensive `is_alive` check, no second
      // decode (the selector already classified the term).
      //
      // Pruning is handled proactively by `SubscriberDown` via
      // `process.monitor`, which the BEAM resolves on the C side the
      // moment the owner dies. A `process.send` to a dead PID is a
      // silent no-op, so a stale entry is at worst a single wasted
      // send before the DOWN message reaches us.
      list.each(state.subscribers, fn(s) { process.send(s.subject, event) })
      actor.continue(state)
    }
    UnknownMessage(dyn) -> {
      on_unknown_msg(dyn)
      actor.continue(state)
    }
  }
}

fn classify_event(dyn: dynamic.Dynamic) -> Result(ClusterEvent, Nil) {
  case decode_node_event(dyn) {
    Ok(#("nodeup", name)) -> Ok(NodeUp(name))
    Ok(#("nodedown", name)) -> Ok(NodeDown(name))
    _ -> Error(Nil)
  }
}

/// Subscribe `listener` to receive `NodeUp`/`NodeDown` events from
/// `monitor`. Idempotent: subscribing the same `listener` twice
/// produces a single subscription (the handler dedups internally).
///
/// See also: `unsubscribe/2`, `start/0`, `start_observed/1`.
pub fn subscribe(monitor: Subject(Message), listener: Subject(ClusterEvent)) {
  process.send(monitor, Subscribe(listener))
}

/// Unsubscribe `listener` from `monitor`. The corresponding
/// `process.monitor` is demonitored so we no longer hear about the
/// owner's death.
pub fn unsubscribe(monitor: Subject(Message), listener: Subject(ClusterEvent)) {
  process.send(monitor, Unsubscribe(listener))
}
