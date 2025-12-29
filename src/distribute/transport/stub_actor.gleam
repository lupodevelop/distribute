//// Stateful transport stub with circuit breaker (Phase 2).
////
//// This module wraps the Phase 1 stub in an OTP actor, adding:
//// - Stateful per-node circuit breakers
//// - Metrics tracking (messages sent/received, bytes, errors)
//// - Actor-based lifecycle (start_link, shutdown)
////
//// ## Design
////
//// Follows the `registry/actor` pattern:
//// - Actor manages internal state
//// - Commands sent via Subject(Command)
//// - Replies sent back via reply subjects
//// - Integration with gleam/otp/actor
////
//// ## Gleam/OTP Best Practices
////
//// - Type-safe command/state handling
//// - Exhaustive pattern matching
//// - Explicit error types
//// - No panics; graceful degradation
////
//// ## Usage
////
//// ```gleam
//// // Start the actor
//// let assert Ok(transport) = stub_actor.start_link()
////
//// // Send a message
//// let result = stub_actor.send(transport, "node2@host", payload)
////
//// // Check metrics
//// let metrics = stub_actor.metrics(transport)
//// ```

import distribute/transport/behaviour
import distribute/transport/circuit_breaker as cb
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None}
import gleam/otp/actor
import gleam/result

/// Commands handled by the transport actor.
pub type Command {
  Send(
    node: behaviour.NodeId,
    payload: BitArray,
    reply: process.Subject(Result(Nil, behaviour.TransportError)),
  )
  Broadcast(
    nodes: List(behaviour.NodeId),
    payload: BitArray,
    reply: process.Subject(
      Result(behaviour.BroadcastResult, behaviour.TransportError),
    ),
  )
  Health(reply: process.Subject(behaviour.HealthStatus))
  Metrics(reply: process.Subject(dict.Dict(String, Int)))
  GetCircuitState(
    node: behaviour.NodeId,
    reply: process.Subject(Option(behaviour.NodeCircuitBreaker)),
  )
  ResetCircuit(node: behaviour.NodeId, reply: process.Subject(Nil))
}

/// Internal state of the transport actor.
type State {
  State(
    messages_sent: Int,
    messages_received: Int,
    bytes_sent: Int,
    send_errors: Int,
    known_nodes: List(behaviour.NodeId),
    retry_policy: behaviour.RetryPolicy,
    circuit_breaker_policy: behaviour.CircuitBreakerPolicy,
    circuit_breakers: cb.CircuitBreakerRegistry,
  )
}

/// Start the transport actor with default settings.
///
/// Returns a Subject for sending commands to the actor.
pub fn start_link() -> Result(process.Subject(Command), actor.StartError) {
  let initial_state =
    State(
      messages_sent: 0,
      messages_received: 0,
      bytes_sent: 0,
      send_errors: 0,
      known_nodes: ["node2@host", "node3@host"],
      retry_policy: behaviour.RetryPolicy(
        max_attempts: 3,
        initial_backoff_ms: 10,
        max_backoff_ms: 1000,
      ),
      circuit_breaker_policy: cb.default_policy(),
      circuit_breakers: dict.new(),
    )

  actor.new(initial_state)
  |> actor.on_message(fn(state, message) { handle_message(state, message) })
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

/// Send a message to a node with circuit breaker protection.
pub fn send(
  transport: process.Subject(Command),
  node: behaviour.NodeId,
  payload: BitArray,
) -> Result(Nil, behaviour.TransportError) {
  let reply_subject = process.new_subject()
  process.send(transport, Send(node, payload, reply_subject))

  case process.receive(reply_subject, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error(behaviour.Timeout)
  }
}

/// Broadcast to multiple nodes with circuit breaker protection.
pub fn broadcast(
  transport: process.Subject(Command),
  nodes: List(behaviour.NodeId),
  payload: BitArray,
) -> Result(behaviour.BroadcastResult, behaviour.TransportError) {
  let reply_subject = process.new_subject()
  process.send(transport, Broadcast(nodes, payload, reply_subject))

  case process.receive(reply_subject, 5000) {
    Ok(result) -> result
    Error(Nil) -> Error(behaviour.Timeout)
  }
}

/// Get health status.
pub fn health(transport: process.Subject(Command)) -> behaviour.HealthStatus {
  let reply_subject = process.new_subject()
  process.send(transport, Health(reply_subject))

  case process.receive(reply_subject, 1000) {
    Ok(status) -> status
    Error(Nil) -> behaviour.Down("health check timeout")
  }
}

/// Get operational metrics.
pub fn metrics(transport: process.Subject(Command)) -> dict.Dict(String, Int) {
  let reply_subject = process.new_subject()
  process.send(transport, Metrics(reply_subject))

  case process.receive(reply_subject, 1000) {
    Ok(metrics) -> metrics
    Error(Nil) -> dict.new()
  }
}

/// Get circuit breaker state for a node.
pub fn get_circuit_state(
  transport: process.Subject(Command),
  node: behaviour.NodeId,
) -> Option(behaviour.NodeCircuitBreaker) {
  let reply_subject = process.new_subject()
  process.send(transport, GetCircuitState(node, reply_subject))

  case process.receive(reply_subject, 1000) {
    Ok(state) -> state
    Error(Nil) -> None
  }
}

/// Reset circuit breaker for a node.
pub fn reset_circuit(
  transport: process.Subject(Command),
  node: behaviour.NodeId,
) -> Nil {
  let reply_subject = process.new_subject()
  process.send(transport, ResetCircuit(node, reply_subject))

  case process.receive(reply_subject, 1000) {
    Ok(result) -> result
    Error(Nil) -> Nil
  }
}

// ============================================================================
// ACTOR MESSAGE HANDLER
// ============================================================================

fn handle_message(state: State, message: Command) -> actor.Next(State, Command) {
  case message {
    Send(node, payload, reply) -> {
      let #(new_state, result) = handle_send(state, node, payload)
      process.send(reply, result)
      actor.continue(new_state)
    }

    Broadcast(nodes, payload, reply) -> {
      let #(new_state, result) = handle_broadcast(state, nodes, payload)
      process.send(reply, result)
      actor.continue(new_state)
    }

    Health(reply) -> {
      let status = handle_health(state)
      process.send(reply, status)
      actor.continue(state)
    }

    Metrics(reply) -> {
      let metrics = handle_metrics(state)
      process.send(reply, metrics)
      actor.continue(state)
    }

    GetCircuitState(node, reply) -> {
      let breaker_state =
        dict.get(state.circuit_breakers, node) |> option.from_result()
      process.send(reply, breaker_state)
      actor.continue(state)
    }

    ResetCircuit(node, reply) -> {
      let new_state =
        State(..state, circuit_breakers: cb.reset(state.circuit_breakers, node))
      process.send(reply, Nil)
      actor.continue(new_state)
    }
  }
}

// ============================================================================
// COMMAND HANDLERS WITH CIRCUIT BREAKER
// ============================================================================

fn handle_send(
  state: State,
  node: behaviour.NodeId,
  payload: BitArray,
) -> #(State, Result(Nil, behaviour.TransportError)) {
  // Get current time (simplified - in real impl, use erlang:now())
  let current_time_ms = 0

  // Get or create circuit breaker
  let #(breaker, registry1) = cb.get_or_create(state.circuit_breakers, node)

  // Check if circuit allows request
  let #(allowed, breaker2) =
    cb.should_allow_request(
      breaker,
      state.circuit_breaker_policy,
      current_time_ms,
    )

  case allowed {
    False -> {
      // Circuit is open
      let state2 =
        State(
          ..state,
          circuit_breakers: cb.update(registry1, node, breaker2),
          send_errors: state.send_errors + 1,
        )
      #(state2, Error(behaviour.Backpressure))
    }

    True -> {
      // Attempt send with retry
      case attempt_send_with_retry(node, payload, state.retry_policy) {
        Ok(Nil) -> {
          // Success - record in circuit breaker
          let breaker3 =
            cb.record_success(breaker2, state.circuit_breaker_policy)
          let state2 =
            State(
              ..state,
              circuit_breakers: cb.update(registry1, node, breaker3),
              messages_sent: state.messages_sent + 1,
              bytes_sent: state.bytes_sent + byte_size(payload),
            )
          #(state2, Ok(Nil))
        }

        Error(err) -> {
          // Failure - record in circuit breaker
          let breaker3 =
            cb.record_failure(
              breaker2,
              state.circuit_breaker_policy,
              current_time_ms,
            )
          let state2 =
            State(
              ..state,
              circuit_breakers: cb.update(registry1, node, breaker3),
              send_errors: state.send_errors + 1,
            )
          #(state2, Error(err))
        }
      }
    }
  }
}

fn handle_broadcast(
  state: State,
  nodes: List(behaviour.NodeId),
  payload: BitArray,
) -> #(State, Result(behaviour.BroadcastResult, behaviour.TransportError)) {
  // Send to each node with circuit breaker
  let #(final_state, outcomes) =
    list.fold(nodes, #(state, dict.new()), fn(acc, node) {
      let #(current_state, results) = acc
      let #(new_state, result) = handle_send(current_state, node, payload)
      let updated_results = dict.insert(results, node, result)
      #(new_state, updated_results)
    })

  // Check if all failed
  let all_failed =
    outcomes
    |> dict.values()
    |> list.all(fn(res) { result.is_error(res) })

  case all_failed {
    True -> #(final_state, Error(behaviour.AdapterFailure("all nodes failed")))
    False -> #(final_state, Ok(outcomes))
  }
}

fn handle_health(state: State) -> behaviour.HealthStatus {
  case state.known_nodes {
    [] -> behaviour.Down("no known nodes")
    _ -> behaviour.Up
  }
}

fn handle_metrics(state: State) -> dict.Dict(String, Int) {
  dict.new()
  |> dict.insert("messages_sent", state.messages_sent)
  |> dict.insert("messages_received", state.messages_received)
  |> dict.insert("bytes_sent", state.bytes_sent)
  |> dict.insert("send_errors", state.send_errors)
  |> dict.insert("known_nodes_count", list.length(state.known_nodes))
  |> dict.insert("active_circuits", dict.size(state.circuit_breakers))
}

// ============================================================================
// PRIVATE HELPERS
// ============================================================================

fn attempt_send_with_retry(
  node: behaviour.NodeId,
  _payload: BitArray,
  _retry_policy: behaviour.RetryPolicy,
) -> Result(Nil, behaviour.TransportError) {
  // Simple simulation: known nodes succeed, unknown fail
  case list.contains(["node2@host", "node3@host"], node) {
    True -> Ok(Nil)
    False -> Error(behaviour.InvalidNode)
  }
}

fn byte_size(payload: BitArray) -> Int {
  // Get bit array size in bytes
  // In real implementation, use erlang:byte_size/1
  case payload {
    <<>> -> 0
    _ -> 8
    // Stub: assume 8 bytes
  }
}
