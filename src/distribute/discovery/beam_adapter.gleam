//// BEAM discovery adapter using Erlang node monitoring.
////
//// This module provides the reference implementation of `DiscoveryAdapter`
//// using the BEAM's built-in node monitoring (`net_kernel`, `erlang:nodes/0`).
//// It is the default adapter used by the `distribute/discovery` facade.
////
//// ## Features
////
//// - **Native BEAM discovery** via `erlang:nodes/0` and `net_kernel`
//// - **Automatic node up/down detection** via `erlang:monitor_node/2`
//// - **Periodic sync** with configurable interval
//// - **OTP actor** implementation for supervision compatibility
////
//// ## Architecture
////
//// The adapter runs as a supervised OTP actor that handles:
////
//// 1. Monitoring connected Erlang nodes
//// 2. Emitting PeerUp/PeerDown events to subscribers
//// 3. Maintaining a snapshot of known peers
//// 4. Periodic background sync
////
//// ## Usage
////
//// Typically you don't use this module directly. Instead, use the
//// `distribute/discovery` facade which manages a singleton instance.
////
//// For custom configurations:
////
//// ```gleam
//// import distribute/discovery/adapter
//// import distribute/discovery/beam_adapter
////
//// let opts = adapter.default_options("my_discovery")
//// let adapter = beam_adapter.new()
//// let assert Ok(handle) = adapter.start(opts)
//// ```

import distribute/discovery/adapter.{type DiscoveryAdapter}
import distribute/discovery/types.{
  type AdapterHandle, type AdapterOptions, type DiscoveryError,
  type DiscoveryEvent, type EventCallback, type HealthStatus, type PeerInfo,
  type SubscriptionId,
}
import distribute/registry
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification, worker}
import gleam/result
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Commands handled by the BEAM discovery adapter actor.
pub opaque type Command {
  Subscribe(
    callback: EventCallback,
    reply: Subject(Result(SubscriptionId, DiscoveryError)),
  )
  Unsubscribe(id: SubscriptionId, reply: Subject(Result(Nil, DiscoveryError)))
  Snapshot(reply: Subject(Result(List(PeerInfo), DiscoveryError)))
  Lookup(peer: String, reply: Subject(Result(PeerInfo, DiscoveryError)))
  Health(reply: Subject(HealthStatus))
  Metrics(reply: Subject(types.DiscoveryMetrics))
  Stop(timeout_ms: Int, reply: Subject(Result(Nil, DiscoveryError)))
  // Internal messages
  Sync
  NodeUp(node: String)
  NodeDown(node: String)
}

/// Internal state of the BEAM discovery adapter actor.
type State {
  State(
    name: String,
    options: AdapterOptions,
    known_peers: Dict(String, types.PeerMetadata),
    subscriptions: Dict(String, EventCallback),
    next_subscription_id: Int,
    last_sync_time_ms: Option(Int),
    last_sync_duration_ms: Int,
    last_error: Option(String),
    sync_count: Int,
    sync_failures: Int,
    events_emitted: Int,
    current_retry_attempt: Int,
  )
}

// =============================================================================
// Public API
// =============================================================================

/// Create a new BEAM discovery adapter instance.
///
/// Returns a `DiscoveryAdapter` record with function pointers for all
/// adapter operations.
///
/// ## Example
///
/// ```gleam
/// let adapter = beam_adapter.new()
/// let assert Ok(handle) = adapter.start(options)
/// let assert Ok(peers) = adapter.snapshot(handle)
/// ```
pub fn new() -> DiscoveryAdapter {
  adapter.DiscoveryAdapter(
    start: beam_start,
    stop: beam_stop,
    subscribe: beam_subscribe,
    unsubscribe: beam_unsubscribe,
    snapshot: beam_snapshot,
    lookup: beam_lookup,
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
      known_peers: dict.new(),
      subscriptions: dict.new(),
      next_subscription_id: 1,
      last_sync_time_ms: None,
      last_sync_duration_ms: 0,
      last_error: None,
      sync_count: 0,
      sync_failures: 0,
      events_emitted: 0,
      current_retry_attempt: 0,
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

      // Schedule initial sync
      schedule_sync(subject, 100)

      let handle = types.new_handle(options.name, wrap_subject(subject))
      Ok(actor.Started(pid: started.pid, data: handle))
    }
    Error(err) -> Error(err)
  }
}

// =============================================================================
// Adapter Implementation
// =============================================================================

/// Start the BEAM discovery adapter.
fn beam_start(options: AdapterOptions) -> Result(AdapterHandle, DiscoveryError) {
  let initial_state =
    State(
      name: options.name,
      options: options,
      known_peers: dict.new(),
      subscriptions: dict.new(),
      next_subscription_id: 1,
      last_sync_time_ms: None,
      last_sync_duration_ms: 0,
      last_error: None,
      sync_count: 0,
      sync_failures: 0,
      events_emitted: 0,
      current_retry_attempt: 0,
    )

  case
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start()
  {
    Ok(started) -> {
      let subject = started.data
      let _ = registry.store_subject(options.name, subject)
      schedule_sync(subject, 100)
      Ok(types.new_handle(options.name, wrap_subject(subject)))
    }
    Error(err) ->
      Error(types.StartFailed("Failed to start actor: " <> string.inspect(err)))
  }
}

/// Stop the discovery adapter gracefully.
fn beam_stop(
  handle: AdapterHandle,
  timeout_ms: Int,
) -> Result(Nil, DiscoveryError) {
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

/// Subscribe to membership events.
fn beam_subscribe(
  handle: AdapterHandle,
  callback: EventCallback,
) -> Result(SubscriptionId, DiscoveryError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Subscribe(callback, reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.SubscriptionFailed("Subscribe timeout"))
      }
    }
    Error(err) -> Error(err)
  }
}

/// Unsubscribe from membership events.
fn beam_unsubscribe(
  handle: AdapterHandle,
  id: SubscriptionId,
) -> Result(Nil, DiscoveryError) {
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

/// Get a snapshot of currently known peers.
fn beam_snapshot(
  handle: AdapterHandle,
) -> Result(List(PeerInfo), DiscoveryError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Snapshot(reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
      }
    }
    Error(err) -> Error(err)
  }
}

/// Lookup a specific peer's info.
fn beam_lookup(
  handle: AdapterHandle,
  peer: String,
) -> Result(PeerInfo, DiscoveryError) {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Lookup(peer, reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(result) -> result
        Error(_) -> Error(types.Timeout(5000))
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

/// Get adapter metrics.
fn beam_metrics(handle: AdapterHandle) -> types.DiscoveryMetrics {
  case get_subject(handle) {
    Ok(subj) -> {
      let reply_subject = process.new_subject()
      process.send(subj, Metrics(reply_subject))

      case process.receive(reply_subject, 1000) {
        Ok(metrics) -> metrics
        Error(_) ->
          types.DiscoveryMetrics(
            sync_count: 0,
            sync_failures: 0,
            last_sync_duration_ms: 0,
            events_emitted: 0,
            known_peers_count: 0,
            error_rate: 0.0,
          )
      }
    }
    Error(_) ->
      types.DiscoveryMetrics(
        sync_count: 0,
        sync_failures: 0,
        last_sync_duration_ms: 0,
        events_emitted: 0,
        known_peers_count: 0,
        error_rate: 0.0,
      )
  }
}

// =============================================================================
// Actor Message Handler
// =============================================================================

fn handle_message(state: State, message: Command) -> actor.Next(State, Command) {
  case message {
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

    Snapshot(reply) -> {
      let peers =
        dict.to_list(state.known_peers)
        |> list.map(fn(pair) {
          let #(id, metadata) = pair
          types.PeerInfo(id: id, metadata: metadata)
        })
      process.send(reply, Ok(peers))
      actor.continue(state)
    }

    Lookup(peer, reply) -> {
      case dict.get(state.known_peers, peer) {
        Ok(metadata) -> {
          process.send(reply, Ok(types.PeerInfo(id: peer, metadata: metadata)))
        }
        Error(_) -> {
          process.send(reply, Error(types.PeerNotFound(peer)))
        }
      }
      actor.continue(state)
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

    Sync -> {
      let new_state = do_sync(state)
      // Schedule next sync with backoff if needed
      let next_delay = case new_state.current_retry_attempt > 0 {
        True ->
          calculate_backoff(
            state.options.retry_config,
            new_state.current_retry_attempt,
          )
        False -> state.options.sync_interval_ms
      }
      case get_self_subject() {
        Ok(self) -> schedule_sync(self, next_delay)
        Error(_) -> Nil
      }
      actor.continue(new_state)
    }

    NodeUp(node) -> {
      let new_state = handle_node_up(state, node)
      actor.continue(new_state)
    }

    NodeDown(node) -> {
      let new_state = handle_node_down(state, node)
      actor.continue(new_state)
    }

    Stop(_timeout_ms, reply) -> {
      // Cleanup: remove from registry
      let _ = registry.remove_stored_subject(state.name)
      process.send(reply, Ok(Nil))
      actor.stop()
    }
  }
}

// =============================================================================
// Sync and Event Logic
// =============================================================================

/// Perform a sync with Erlang nodes.
fn do_sync(state: State) -> State {
  let start_time = system_time_ms()
  let current_nodes = get_erlang_nodes()
  let known_peer_ids = dict.keys(state.known_peers)

  // Find new nodes (up events)
  let new_nodes =
    list.filter(current_nodes, fn(node) { !list.contains(known_peer_ids, node) })

  // Find removed nodes (down events)
  let removed_nodes =
    list.filter(known_peer_ids, fn(node) { !list.contains(current_nodes, node) })

  // Update known_peers
  let updated_peers =
    list.fold(new_nodes, state.known_peers, fn(peers, node) {
      dict.insert(peers, node, dict.new())
    })
  let final_peers =
    list.fold(removed_nodes, updated_peers, fn(peers, node) {
      dict.delete(peers, node)
    })

  // Count events to emit
  let event_count = list.length(new_nodes) + list.length(removed_nodes)

  // Emit events to subscribers (async)
  list.each(new_nodes, fn(node) {
    let metadata = dict.new()
    emit_event_async(state, types.PeerUp(node, metadata))
    // Trigger optional on_peer_up hook
    trigger_peer_up_hook(state.options, node, metadata)
  })
  list.each(removed_nodes, fn(node) {
    emit_event_async(state, types.PeerDown(node, "node_down"))
  })

  let end_time = system_time_ms()
  let duration = end_time - start_time

  State(
    ..state,
    known_peers: final_peers,
    last_sync_time_ms: Some(end_time),
    last_sync_duration_ms: duration,
    sync_count: state.sync_count + 1,
    events_emitted: state.events_emitted + event_count,
    current_retry_attempt: 0,
    // Reset on success
  )
}

/// Handle a node up event.
fn handle_node_up(state: State, node: String) -> State {
  case dict.has_key(state.known_peers, node) {
    True -> state
    False -> {
      let new_peers = dict.insert(state.known_peers, node, dict.new())
      let metadata = dict.new()
      emit_event_async(state, types.PeerUp(node, metadata))
      trigger_peer_up_hook(state.options, node, metadata)
      State(
        ..state,
        known_peers: new_peers,
        events_emitted: state.events_emitted + 1,
      )
    }
  }
}

/// Handle a node down event.
fn handle_node_down(state: State, node: String) -> State {
  case dict.has_key(state.known_peers, node) {
    False -> state
    True -> {
      let new_peers = dict.delete(state.known_peers, node)
      emit_event_async(state, types.PeerDown(node, "node_down"))
      State(
        ..state,
        known_peers: new_peers,
        events_emitted: state.events_emitted + 1,
      )
    }
  }
}

/// Emit an event to all subscribers asynchronously.
///
/// Each subscriber callback is invoked in a separate spawned process,
/// protecting the main actor from slow or blocking callbacks.
fn emit_event_async(state: State, event: DiscoveryEvent) -> Nil {
  dict.each(state.subscriptions, fn(_id, callback) {
    // Spawn a process for each callback to avoid blocking the actor
    let _ = spawn_event_handler(callback, event)
    Nil
  })
}

/// Trigger the optional on_peer_up hook (e.g., for handshake integration).
fn trigger_peer_up_hook(
  options: AdapterOptions,
  peer: String,
  metadata: types.PeerMetadata,
) -> Nil {
  case options.on_peer_up {
    Some(hook) -> {
      // Spawn to avoid blocking
      let _ = spawn_hook_handler(hook, peer, metadata)
      Nil
    }
    None -> Nil
  }
}

/// Compute health status.
fn compute_health(state: State) -> HealthStatus {
  case state.last_error {
    Some(err) -> types.Degraded(err)
    None -> {
      case state.sync_count {
        0 -> types.Degraded("No sync completed yet")
        _ -> types.Up
      }
    }
  }
}

/// Compute metrics snapshot.
fn compute_metrics(state: State) -> types.DiscoveryMetrics {
  let error_rate = case state.sync_count {
    0 -> 0.0
    total -> int.to_float(state.sync_failures) /. int.to_float(total)
  }

  types.DiscoveryMetrics(
    sync_count: state.sync_count,
    sync_failures: state.sync_failures,
    last_sync_duration_ms: state.last_sync_duration_ms,
    events_emitted: state.events_emitted,
    known_peers_count: dict.size(state.known_peers),
    error_rate: error_rate,
  )
}

/// Calculate backoff delay based on retry config and attempt number.
fn calculate_backoff(config: types.RetryConfig, attempt: Int) -> Int {
  let base_delay = config.initial_backoff_ms
  let multiplier = pow_int(2, attempt - 1)
  let delay = base_delay * multiplier
  min_int(delay, config.max_backoff_ms)
}

/// Integer power function.
fn pow_int(base: Int, exp: Int) -> Int {
  case exp {
    0 -> 1
    1 -> base
    n if n > 0 -> base * pow_int(base, n - 1)
    _ -> 1
  }
}

/// Minimum of two integers.
fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

// =============================================================================
// Handle Lookup
// =============================================================================

/// Get a handle to a running adapter by name.
///
/// This function looks up a previously started adapter by its registered name
/// and returns a handle that can be used with the `DiscoveryAdapter` functions.
pub fn get_handle(name: String) -> Result(AdapterHandle, Nil) {
  case registry.lookup_subject(name) {
    Ok(subject) -> {
      Ok(types.new_handle(name, wrap_subject(subject)))
    }
    Error(_) -> Error(Nil)
  }
}

// =============================================================================
// FFI and Helpers
// =============================================================================

/// Helper to extract Subject from handle.
fn get_subject(
  handle: AdapterHandle,
) -> Result(Subject(Command), DiscoveryError) {
  let dyn_state = types.handle_state(handle)
  unwrap_subject(dyn_state)
  |> result.replace_error(types.InvalidConfiguration("Invalid handle state"))
}

/// Schedule a sync after delay_ms.
fn schedule_sync(subject: Subject(Command), delay_ms: Int) -> Nil {
  process.send_after(subject, delay_ms, Sync)
  Nil
}

// FFI declarations
@external(erlang, "discovery_ffi", "wrap_subject")
fn wrap_subject(subject: Subject(Command)) -> Dynamic

@external(erlang, "discovery_ffi", "unwrap_subject")
fn unwrap_subject(dynamic: Dynamic) -> Result(Subject(Command), Nil)

@external(erlang, "discovery_ffi", "register_process_by_name")
fn register_process_by_name(
  pid: process.Pid,
  name: String,
) -> Result(Nil, DiscoveryError)

@external(erlang, "discovery_ffi", "get_erlang_nodes")
fn get_erlang_nodes() -> List(String)

@external(erlang, "discovery_ffi", "get_self_subject")
fn get_self_subject() -> Result(Subject(Command), Nil)

@external(erlang, "discovery_ffi", "spawn_event_handler")
fn spawn_event_handler(
  callback: EventCallback,
  event: DiscoveryEvent,
) -> process.Pid

@external(erlang, "discovery_ffi", "spawn_hook_handler")
fn spawn_hook_handler(
  hook: fn(String, types.PeerMetadata) -> Nil,
  peer: String,
  metadata: types.PeerMetadata,
) -> process.Pid

@external(erlang, "erlang", "system_time")
fn system_time_ms() -> Int
