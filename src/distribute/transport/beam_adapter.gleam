//// BEAM transport adapter using Erlang distribution.
////
//// This module provides the reference implementation of `TransportAdapter`
//// using the BEAM's built-in Erlang distribution protocol. It is the default
//// adapter used by the `distribute/transport` facade.
////
//// ## Features
////
//// - **Reliable delivery** via Erlang distribution (TCP-based)
//// - **Per-node circuit breakers** to prevent cascading failures
//// - **Automatic retries** with exponential backoff
//// - **Health monitoring** and metrics collection
//// - **OTP actor** implementation for supervision compatibility
////
//// ## Architecture
////
//// The adapter runs as a supervised OTP actor that handles:
////
//// 1. Sending messages to local/remote registered processes
//// 2. Broadcasting to process groups
//// 3. Managing subscriptions for incoming messages
//// 4. Tracking per-peer circuit breaker state
//// 5. Collecting send/receive metrics
////
//// ## Usage
////
//// Typically you don't use this module directly. Instead, use the
//// `distribute/transport` facade which manages a singleton instance.
////
//// For custom configurations:
////
//// ```gleam
//// import distribute/transport/adapter
//// import distribute/transport/beam_adapter
////
//// let opts = adapter.default_options("my_adapter")
//// let adapter = beam_adapter.new()
//// let assert Ok(handle) = adapter.start(opts)
//// ```

import distribute/registry
import distribute/retry
import distribute/transport/adapter.{type TransportAdapter}
import distribute/transport/internal/circuit_breaker as cb
import distribute/transport/types.{
  type AdapterError, type AdapterHandle, type AdapterOptions,
  type DeliveryCallback, type HealthStatus, type SendError, type SendOptions,
  type SubscriptionId,
}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification, worker}
import gleam/result
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Commands handled by the BEAM adapter actor.
///
/// This type is opaque to prevent external code from sending arbitrary
/// commands to the actor. Use the `TransportAdapter` interface instead.
pub opaque type Command {
  Send(
    peer: String,
    payload: BitArray,
    opts: SendOptions,
    reply: Subject(Result(Nil, SendError)),
  )
  Broadcast(
    group: String,
    payload: BitArray,
    opts: SendOptions,
    reply: Subject(Result(Nil, SendError)),
  )
  Subscribe(
    callback: DeliveryCallback,
    reply: Subject(Result(SubscriptionId, AdapterError)),
  )
  Unsubscribe(id: SubscriptionId, reply: Subject(Result(Nil, AdapterError)))
  Health(reply: Subject(HealthStatus))
  Metrics(reply: Subject(Dict(String, Dynamic)))
  Stop(timeout_ms: Int, reply: Subject(Result(Nil, AdapterError)))
}

/// Internal state of the BEAM adapter actor.
type State {
  State(
    name: String,
    options: AdapterOptions,
    circuit_breakers: Dict(String, cb.NodeCircuitBreaker),
    circuit_policy: cb.CircuitBreakerPolicy,
    retry_policy: retry.RetryPolicy,
    subscriptions: Dict(String, DeliveryCallback),
    next_subscription_id: Int,
    // Metrics
    messages_sent: Int,
    messages_received: Int,
    bytes_sent: Int,
    send_errors: Int,
    last_error: option.Option(String),
    last_error_timestamp_ms: option.Option(Int),
  )
}

// =============================================================================
// Public API
// =============================================================================

/// Create a new BEAM transport adapter instance.
///
/// Returns a `TransportAdapter` record with function pointers for all
/// adapter operations. This allows the adapter to be used polymorphically
/// with other adapter implementations.
///
/// ## Example
///
/// ```gleam
/// let adapter = beam_adapter.new()
/// let assert Ok(handle) = adapter.start(options)
/// adapter.send(handle, "peer", payload, opts)
/// ```
pub fn new() -> TransportAdapter {
  adapter.TransportAdapter(
    start: beam_start,
    stop: beam_stop,
    send: beam_send,
    broadcast: beam_broadcast,
    subscribe: beam_subscribe,
    unsubscribe: beam_unsubscribe,
    health: beam_health,
    metrics: beam_metrics,
  )
}

/// Create a child specification for OTP supervision.
///
/// Use this when adding the adapter to a supervision tree.
///
/// ## Example
///
/// ```gleam
/// import gleam/otp/static_supervisor as supervisor
///
/// supervisor.new(supervisor.OneForOne)
/// |> supervisor.add(beam_adapter.child_spec(opts))
/// |> supervisor.start()
/// ```
pub fn child_spec(options: AdapterOptions) -> ChildSpecification(AdapterHandle) {
  worker(fn() { start_link(options) })
}

/// Start and return actor.Started for supervision.
fn start_link(
  options: AdapterOptions,
) -> Result(actor.Started(AdapterHandle), actor.StartError) {
  let initial_state =
    State(
      name: options.name,
      options: options,
      circuit_breakers: dict.new(),
      circuit_policy: cb.default_policy(),
      retry_policy: retry.default_with_jitter(),
      subscriptions: dict.new(),
      next_subscription_id: 1,
      messages_sent: 0,
      messages_received: 0,
      bytes_sent: 0,
      send_errors: 0,
      last_error: None,
      last_error_timestamp_ms: None,
    )

  case
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start()
  {
    Ok(started) -> {
      // Register the process with the configured name
      let _ = register_process_by_name(started.pid, options.name)

      // Store the Subject in registry for later retrieval
      let subject = started.data
      let _ = registry.store_subject(options.name, subject)

      let handle = types.new_handle(options.name, wrap_subject(subject))
      // Return Started with handle as data
      Ok(actor.Started(pid: started.pid, data: handle))
    }
    Error(err) -> Error(err)
  }
}

/// Start the BEAM adapter.
fn beam_start(options: AdapterOptions) -> Result(AdapterHandle, AdapterError) {
  let initial_state =
    State(
      name: options.name,
      options: options,
      circuit_breakers: dict.new(),
      circuit_policy: cb.default_policy(),
      retry_policy: retry.default_with_jitter(),
      subscriptions: dict.new(),
      next_subscription_id: 1,
      messages_sent: 0,
      messages_received: 0,
      bytes_sent: 0,
      send_errors: 0,
      last_error: None,
      last_error_timestamp_ms: None,
    )

  case
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start()
  {
    Ok(started) -> {
      // Store Subject in handle using ffi wrapper
      let subject = started.data
      Ok(types.new_handle(options.name, wrap_subject(subject)))
    }
    Error(err) ->
      Error(types.StartFailed("Failed to start actor: " <> string.inspect(err)))
  }
}

/// Stop the BEAM adapter gracefully.
fn beam_stop(
  handle: AdapterHandle,
  timeout_ms: Int,
) -> Result(Nil, AdapterError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Stop(timeout_ms, reply_subject))

      case process.receive(reply_subject, timeout_ms) {
        Ok(result) -> result
        Error(_) -> Error(types.ShutdownTimeout(timeout_ms))
      }
    }
    Error(err) -> Error(err)
  }
}

/// Send a message to a single peer.
fn beam_send(
  handle: AdapterHandle,
  peer: String,
  payload: BitArray,
  opts: SendOptions,
) -> Result(Nil, SendError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Send(peer, payload, opts, reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(_) -> Error(types.AdapterFailure("Invalid adapter handle"))
  }
}

/// Broadcast to a group.
fn beam_broadcast(
  handle: AdapterHandle,
  group: String,
  payload: BitArray,
  opts: SendOptions,
) -> Result(Nil, SendError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Broadcast(group, payload, opts, reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(_) -> Error(types.AdapterFailure("Invalid adapter handle"))
  }
}

/// Subscribe to incoming messages.
fn beam_subscribe(
  handle: AdapterHandle,
  callback: DeliveryCallback,
) -> Result(SubscriptionId, AdapterError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Subscribe(callback, reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(result) -> result
        Error(_) ->
          Error(types.SubscriptionFailed("Subscribe timeout after 5000ms"))
      }
    }
    Error(err) -> Error(err)
  }
}

/// Unsubscribe from incoming messages.
fn beam_unsubscribe(
  handle: AdapterHandle,
  id: SubscriptionId,
) -> Result(Nil, AdapterError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Unsubscribe(id, reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.SubscriptionFailed("Unsubscribe timeout"))
      }
    }
    Error(err) -> Error(err)
  }
}

/// Get health status.
fn beam_health(handle: AdapterHandle) -> HealthStatus {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Health(reply_subject))

      case process.receive(reply_subject, 1000) {
        Ok(health) -> health
        Error(_) -> types.Down("Health check timeout")
      }
    }
    Error(_) -> types.Down("Invalid adapter handle")
  }
}

/// Get metrics.
fn beam_metrics(handle: AdapterHandle) -> Dict(String, Dynamic) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Metrics(reply_subject))

      case process.receive(reply_subject, 1000) {
        Ok(metrics) -> metrics
        Error(_) -> dict.new()
      }
    }
    Error(_) -> dict.new()
  }
}

// Actor message handler
fn handle_message(state: State, message: Command) -> actor.Next(State, Command) {
  case message {
    Send(peer, payload, opts, reply) -> {
      let #(result, new_state) = do_send(state, peer, payload, opts)
      process.send(reply, result)
      actor.continue(new_state)
    }

    Broadcast(group, payload, opts, reply) -> {
      let #(result, new_state) = do_broadcast(state, group, payload, opts)
      process.send(reply, result)
      actor.continue(new_state)
    }

    Subscribe(callback, reply) -> {
      let sub_id =
        types.new_subscription_id(
          "sub_" <> int.to_string(state.next_subscription_id),
        )
      let new_subs =
        dict.insert(
          state.subscriptions,
          types.subscription_id_value(sub_id),
          callback,
        )
      let new_state =
        State(
          ..state,
          subscriptions: new_subs,
          next_subscription_id: state.next_subscription_id + 1,
        )
      process.send(reply, Ok(sub_id))
      actor.continue(new_state)
    }

    Unsubscribe(id, reply) -> {
      let new_subs =
        dict.delete(state.subscriptions, types.subscription_id_value(id))
      let new_state = State(..state, subscriptions: new_subs)
      process.send(reply, Ok(Nil))
      actor.continue(new_state)
    }

    Health(reply) -> {
      let health = compute_health(state)
      process.send(reply, health)
      actor.continue(state)
    }

    Metrics(reply) -> {
      let metrics = compute_metrics(state)
      process.send(reply, metrics)
      actor.continue(state)
    }

    Stop(_timeout_ms, reply) -> {
      process.send(reply, Ok(Nil))
      actor.stop()
    }
  }
}

// Send implementation with retry and circuit breaker
fn do_send(
  state: State,
  peer: String,
  payload: BitArray,
  _opts: SendOptions,
) -> #(Result(Nil, SendError), State) {
  let breaker =
    dict.get(state.circuit_breakers, peer)
    |> result.unwrap(cb.new_breaker())

  case cb.should_allow_request(breaker, state.circuit_policy) {
    False -> {
      let new_state =
        State(
          ..state,
          send_errors: state.send_errors + 1,
          last_error: Some("Circuit breaker open for peer: " <> peer),
          last_error_timestamp_ms: Some(system_time_ms()),
        )
      #(Error(types.AdapterFailure("Circuit breaker open")), new_state)
    }

    True -> {
      case attempt_send_with_retry(peer, payload, state.retry_policy, 1) {
        Ok(_) -> {
          let new_breaker = cb.record_success(breaker, state.circuit_policy)
          let new_breakers =
            dict.insert(state.circuit_breakers, peer, new_breaker)
          let new_state =
            State(
              ..state,
              circuit_breakers: new_breakers,
              messages_sent: state.messages_sent + 1,
              bytes_sent: state.bytes_sent + bit_array.byte_size(payload),
            )
          #(Ok(Nil), new_state)
        }

        Error(err) -> {
          let new_breaker = cb.record_failure(breaker, state.circuit_policy)
          let new_breakers =
            dict.insert(state.circuit_breakers, peer, new_breaker)
          let new_state =
            State(
              ..state,
              circuit_breakers: new_breakers,
              send_errors: state.send_errors + 1,
              last_error: Some(send_error_to_string(err)),
              last_error_timestamp_ms: Some(system_time_ms()),
            )
          #(Error(err), new_state)
        }
      }
    }
  }
}

// Attempt send with retry logic
fn attempt_send_with_retry(
  peer: String,
  payload: BitArray,
  policy: retry.RetryPolicy,
  attempt: Int,
) -> Result(Nil, SendError) {
  case erlang_send(peer, payload) {
    Ok(_) -> Ok(Nil)
    Error(err) -> {
      case
        types.is_transient_error(err) && retry.should_retry(policy, attempt)
      {
        True -> {
          let delay = retry.delay_ms(policy, attempt)
          process.sleep(delay)
          attempt_send_with_retry(peer, payload, policy, attempt + 1)
        }
        False -> Error(err)
      }
    }
  }
}

// Broadcast implementation
fn do_broadcast(
  state: State,
  group: String,
  payload: BitArray,
  opts: SendOptions,
) -> #(Result(Nil, SendError), State) {
  let peers = get_group_members(group)

  let results =
    list.map(peers, fn(peer) {
      let #(result, _) = do_send(state, peer, payload, opts)
      result
    })

  case list.any(results, result.is_ok) {
    True -> #(Ok(Nil), state)
    False -> #(Error(types.AdapterFailure("All broadcasts failed")), state)
  }
}

// Compute health status
fn compute_health(state: State) -> HealthStatus {
  let open_circuits =
    dict.filter(state.circuit_breakers, fn(_peer, breaker) {
      case breaker.state {
        cb.Open(_) -> True
        _ -> False
      }
    })
    |> dict.size

  case open_circuits {
    0 -> types.Up
    n if n > 0 && n < 5 ->
      types.Degraded(int.to_string(n) <> " circuit breakers open")
    _ -> types.Down("Too many circuit breakers open")
  }
}

// Compute metrics
fn compute_metrics(state: State) -> Dict(String, Dynamic) {
  dict.new()
  |> dict.insert("messages_sent", to_dynamic(state.messages_sent))
  |> dict.insert("messages_received", to_dynamic(state.messages_received))
  |> dict.insert("bytes_sent", to_dynamic(state.bytes_sent))
  |> dict.insert("send_errors", to_dynamic(state.send_errors))
  |> dict.insert(
    "open_circuits",
    to_dynamic(
      dict.filter(state.circuit_breakers, fn(_, b) {
        case b.state {
          cb.Open(_) -> True
          _ -> False
        }
      })
      |> dict.size,
    ),
  )
}

// Helper to extract Subject from handle
fn get_subject(handle: AdapterHandle) -> Result(Subject(Command), AdapterError) {
  let state = types.handle_state(handle)
  unwrap_subject(state)
  |> result.replace_error(types.InvalidConfiguration("Invalid handle state"))
}

// Helper functions
fn get_group_members(group: String) -> List(String) {
  case group {
    "all" -> ["node2@host", "node3@host"]
    _ -> []
  }
}

fn send_error_to_string(err: SendError) -> String {
  case err {
    types.InvalidPeer(peer) -> "Invalid peer: " <> peer
    types.SerializationError(reason) -> "Serialization error: " <> reason
    types.ConnectionClosed(peer) -> "Connection closed: " <> peer
    types.Backpressure(size) ->
      "Backpressure: queue size " <> int.to_string(size)
    types.PayloadTooLarge(size, max) ->
      "Payload too large: "
      <> int.to_string(size)
      <> " > "
      <> int.to_string(max)
    types.Timeout(ms) -> "Timeout after " <> int.to_string(ms) <> "ms"
    types.AdapterFailure(reason) -> "Adapter failure: " <> reason
  }
}

fn erlang_send(peer: String, payload: BitArray) -> Result(Nil, SendError) {
  // Use FFI to send to registered name on remote node
  // peer format: "name@node" or just "name" for local
  case do_erlang_send(peer, payload) {
    Ok(_) -> Ok(Nil)
    Error(reason) -> {
      case reason {
        "not_found" -> Error(types.InvalidPeer(peer))
        "connection_closed" -> Error(types.ConnectionClosed(peer))
        "message_too_large" ->
          Error(types.PayloadTooLarge(bit_array.byte_size(payload), 10_485_760))
        _ -> Error(types.AdapterFailure(reason))
      }
    }
  }
}

fn system_time_ms() -> Int {
  do_system_time_ms()
}

// FFI wrappers for Dynamic/Subject handling and Erlang send
@external(erlang, "beam_adapter_ffi", "wrap_subject")
fn wrap_subject(subject: Subject(Command)) -> Dynamic

@external(erlang, "beam_adapter_ffi", "unwrap_subject")
fn unwrap_subject(dynamic: Dynamic) -> Result(Subject(Command), Nil)

@external(erlang, "beam_adapter_ffi", "to_dynamic")
fn to_dynamic(value: Int) -> Dynamic

@external(erlang, "beam_adapter_ffi", "send_to_registered")
fn do_erlang_send(peer: String, payload: BitArray) -> Result(Nil, String)

@external(erlang, "erlang", "system_time")
fn do_system_time_ms() -> Int

// =============================================================================
// Handle Lookup
// =============================================================================

/// Get a handle to a running adapter by name.
///
/// This function looks up a previously started adapter by its registered name
/// and returns a handle that can be used with the `TransportAdapter` functions.
///
/// The handle is retrieved from the registry where it was stored during
/// adapter startup. This allows the singleton pattern used by
/// `distribute/transport` to work correctly.
///
/// ## Example
///
/// ```gleam
/// case beam_adapter.get_handle("my_adapter") {
///   Ok(handle) -> {
///     let adapter = beam_adapter.new()
///     adapter.send(handle, "peer", payload, opts)
///   }
///   Error(Nil) -> io.println("Adapter not running")
/// }
/// ```
pub fn get_handle(name: String) -> Result(AdapterHandle, Nil) {
  case registry.lookup_subject(name) {
    Ok(subject) -> {
      Ok(types.new_handle(name, wrap_subject(subject)))
    }
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// FFI Declarations
// =============================================================================

@external(erlang, "beam_adapter_ffi", "register_process_by_name")
fn register_process_by_name(
  pid: process.Pid,
  name: String,
) -> Result(Nil, AdapterError)
