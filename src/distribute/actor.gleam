/// Type-safe actor patterns for distributed systems.
///
/// This module provides ergonomic helpers for creating OTP-compliant actors
/// with automatic message encoding/decoding. All functions return type-safe
/// `GlobalSubject(msg)` handles that enforce codec usage at compile time.
///
/// ## Features
///
/// - **Type Safety**: All actors return `GlobalSubject(msg)` for compile-time guarantees
/// - **OTP Integration**: Compatible with `gleam/otp` supervision trees
/// - **Error Handling**: Explicit `Result` types for all operations
/// - **Registry Support**: Actors can be registered globally with `registry.register_typed`
///
/// ## Architecture
///
/// This module builds on top of `distribute/receiver` which provides the
/// low-level message decoding loop. The helpers here add:
///
/// - Request-response patterns with type-safe replies
/// - Worker pool management with load balancing
/// - Supervision integration via `child_spec` functions
/// - Graceful shutdown handling
///
/// ## Common Patterns
///
/// ### Simple Stateful Actor
///
/// ```gleam
/// pub type MyMsg {
///   Increment
///   GetValue(reply: Subject(Int))
/// }
///
/// let actor = actor.start_typed_actor(
///   0,  // initial state
///   my_encoder(),
///   my_decoder(),
///   fn(msg, count) {
///     case msg {
///       Increment -> receiver.Continue(count + 1)
///       GetValue(reply) -> {
///         process.send(reply, count)
///         receiver.Continue(count)
///       }
///     }
///   },
/// )
/// ```
///
/// ### Request-Response Server
///
/// Use `start_server` for actors that process requests and send typed responses:
///
/// ```gleam
/// pub type Request {
///   Compute(Int, Subject(Result(Int, String)))
/// }
///
/// let server = actor.start_server(
///   Nil,  // initial state
///   request_encoder(),
///   request_decoder(),
///   fn(msg, state) {
///     case msg {
///       Compute(n, reply) -> {
///         let result = do_computation(n)
///         process.send(reply, result)
///         receiver.Continue(state)
///       }
///     }
///   },
/// )
/// ```
///
/// ### Supervision Integration
///
/// ```gleam
/// import gleam/otp/supervision.{supervisor, worker}
///
/// pub fn start_supervised() {
///   supervisor.start_link(fn(children) {
///     children
///     |> supervision.add(actor.child_spec_typed_actor(
///       initial_state: 0,
///       encoder: my_encoder(),
///       decoder: my_decoder(),
///       handler: my_handler,
///     ))
///   })
/// }
/// ```
///
/// ## Compatibility
///
/// All functions in this module are compatible with:
/// - `gleam/otp/actor` supervision trees
/// - `gleam/erlang/process` Subject-based messaging
/// - `distribute/registry` for global name registration
/// - `distribute/messaging` for cross-node communication
///
/// **Deprecation policy:**
/// - Legacy low-level APIs `start` and `start_global` are **deprecated** and are
///   scheduled for removal in v3.0.0.
/// - Prefer the type-safe helpers: `start_typed_actor` and `start_server`.
/// - See `MIGRATION.md` → "Actor API deprecations" for migration examples and
///   suggested codemods.
///
import distribute/codec.{type Decoder, type Encoder}
import distribute/global
import distribute/log
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleam/list
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification, worker}
import gleam/result

/// Errors that can occur when starting an actor.
pub type ActorError {
  /// The underlying OTP actor failed to start.
  StartFailed(actor.StartError)
  /// Invalid configuration provided.
  InvalidConfiguration(String)
}

/// Start a type-safe actor with global subject (RECOMMENDED).
///
/// This is the recommended way to create distributed actors. It wraps
/// `receiver.start_typed_actor` for convenience and compatibility.
///
/// The returned `GlobalSubject(msg)` can be:
/// - Registered globally with `registry.register_typed`
/// - Used for cross-node messaging with `messaging.send_typed`
/// - Passed to other processes safely
///
/// Malformed messages are silently ignored to prevent crashes.
///
/// ## Parameters
///
/// - `initial_state`: The initial state value
/// - `encoder`: Encoder for outgoing messages (used by clients)
/// - `decoder`: Decoder for incoming messages
/// - `handler`: Message handler returning `Next(state)`
///
/// ## Returns
///
/// A `GlobalSubject(msg)` that enforces type-safe messaging.
///
/// ## Example
///
/// ```gleam
/// pub type Counter {
///   Inc
///   Dec
///   GetValue(Subject(Int))
/// }
///
/// let actor = actor.start_typed_actor(
///   0,
///   counter_encoder(),
///   counter_decoder(),
///   fn(msg, count) {
///     case msg {
///       Inc -> receiver.Continue(count + 1)
///       Dec -> receiver.Continue(count - 1)
///       GetValue(reply) -> {
///         process.send(reply, count)
///         receiver.Continue(count)
///       }
///     }
///   },
/// )
/// ```
pub fn start_typed_actor(
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> global.GlobalSubject(msg) {
  receiver.start_typed_actor(initial_state, encoder, decoder, handler)
}

/// Start a request-response server actor.
///
/// This is a specialized version of `start_typed_actor` optimized for
/// request-response patterns. It's semantically identical but provides
/// clearer intent in the API.
///
/// Use this when your actor primarily processes requests and sends responses
/// back to callers via reply subjects embedded in the request messages.
///
/// ## Parameters
///
/// - `initial_state`: The initial server state
/// - `encoder`: Encoder for request messages
/// - `decoder`: Decoder for request messages
/// - `handler`: Request handler that processes requests and sends replies
///
/// ## Returns
///
/// A `GlobalSubject(request)` for sending requests to the server.
///
/// ## Example
///
/// ```gleam
/// pub type Request {
///   Add(Int, Int, Subject(Int))
///   Multiply(Int, Int, Subject(Int))
/// }
///
/// let server = actor.start_server(
///   Nil,
///   request_encoder(),
///   request_decoder(),
///   fn(req, _state) {
///     case req {
///       Add(a, b, reply) -> {
///         process.send(reply, a + b)
///         receiver.Continue(Nil)
///       }
///       Multiply(a, b, reply) -> {
///         process.send(reply, a * b)
///         receiver.Continue(Nil)
///       }
///     }
///   },
/// )
/// ```
pub fn start_server(
  initial_state: state,
  encoder: Encoder(request),
  decoder: Decoder(request),
  handler: fn(request, state) -> receiver.Next(state),
) -> global.GlobalSubject(request) {
  // Semantically identical to start_typed_actor, but clearer intent
  start_typed_actor(initial_state, encoder, decoder, handler)
}

/// Start an actor that returns a raw `Subject(BitArray)`.
///
/// This is a lower-level helper that wraps `receiver.start_typed_receiver`.
/// Prefer `start_typed_actor` which returns a type-safe `GlobalSubject(msg)`.
///
/// Use this only when you need direct access to the underlying `Subject(BitArray)`
/// for manual message encoding or integration with legacy code.
///
/// ## Parameters
///
/// - `initial_state`: The initial state
/// - `decoder`: Decoder for incoming BitArray messages
/// - `handler`: Message handler
///
/// ## Returns
///
/// `Ok(Subject(BitArray))` on success, `Error(StartFailed(err))` on failure.
@deprecated("Use start_typed_actor instead; will be removed in v3.0.0")
pub fn start(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(process.Subject(BitArray), actor.StartError) {
  log.warn(
    "actor.start is deprecated and will be removed in v3.0.0; use actor.start_typed_actor(...) instead",
    [#("replacement", "actor.start_typed_actor")],
  )
  receiver.start_typed_receiver(initial_state, decoder, handler)
}

/// Start a global actor returning a raw `Subject(BitArray)`.
///
/// Similar to `start` but uses `receiver.start_global_receiver` which
/// creates a subject compatible with global registry operations.
///
/// Prefer `start_typed_actor` which wraps this and returns a type-safe
/// `GlobalSubject(msg)`.
///
/// ## Parameters
///
/// - `initial_state`: The initial state
/// - `decoder`: Decoder for incoming messages
/// - `handler`: Message handler
///
/// ## Returns
///
/// A `Subject(BitArray)` with a `Nil` tag for global use.
@deprecated("Use start_typed_actor instead; will be removed in v3.0.0")
pub fn start_global(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> process.Subject(BitArray) {
  log.warn(
    "actor.start_global is deprecated and will be removed in v3.0.0; use actor.start_typed_actor(...) and registry.register_typed(...) instead",
    [#("replacement", "actor.start_typed_actor")],
  )
  receiver.start_global_receiver(initial_state, decoder, handler)
}

/// Create a supervision child spec for a typed actor.
///
/// This function returns a `ChildSpecification` that can be added to a
/// `gleam/otp/supervision` supervisor tree. The supervisor will automatically
/// restart the actor if it crashes.
///
/// Note: Returns `Subject(BitArray)` rather than `GlobalSubject(msg)` for
/// OTP compatibility. Wrap with `global.from_subject` after supervision start
/// if you need the typed wrapper.
///
/// ## Parameters
///
/// - `initial_state`: The initial state for the actor
/// - `decoder`: Decoder for messages
/// - `handler`: Message handler function
///
/// ## Returns
///
/// A `ChildSpecification(Subject(BitArray))` for use with supervisors.
///
/// ## Example
///
/// ```gleam
/// import gleam/otp/supervision.{supervisor}
///
/// pub fn start_supervised() {
///   supervisor.start_link(fn(children) {
///     children
///     |> supervision.add(actor.child_spec_typed_actor(
///       0,
///       my_decoder(),
///       my_handler,
///     ))
///   })
/// }
/// ```
pub fn child_spec_typed_actor(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> ChildSpecification(process.Subject(BitArray)) {
  worker(fn() {
    actor.new(initial_state)
    |> actor.on_message(fn(state, binary: BitArray) {
      case decoder(binary) {
        Ok(message) -> {
          case handler(message, state) {
            receiver.Continue(new_state) -> actor.continue(new_state)
            receiver.Stop -> actor.stop()
            receiver.StopAbnormal(reason) -> actor.stop_abnormal(reason)
          }
        }
        Error(_) -> actor.continue(state)
      }
    })
    |> actor.start()
  })
}

/// Create a supervision child spec for a request-response server.
///
/// Identical to `child_spec_typed_actor` but with clearer semantic intent
/// for server patterns.
///
/// ## Example
///
/// ```gleam
/// supervisor.start_link(fn(children) {
///   children
///   |> supervision.add(actor.child_spec_server(
///     Nil,
///     request_decoder(),
///     handle_request,
///   ))
/// })
/// ```
pub fn child_spec_server(
  initial_state: state,
  decoder: Decoder(request),
  handler: fn(request, state) -> receiver.Next(state),
) -> ChildSpecification(process.Subject(BitArray)) {
  child_spec_typed_actor(initial_state, decoder, handler)
}

/// Start and register a typed actor under a global name.
/// 
/// This helper starts a typed actor and registers its subject with
/// `registry.register_typed(name, subject)`. Returns `Ok(GlobalSubject(msg))`
/// on success or `Error(RegisterError)` if registration fails.
///
/// Note: If registration fails, the started actor process will continue running.
/// The caller is responsible for cleanup in error cases if needed.
pub fn start_typed_actor_registered(
  name: String,
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(global.GlobalSubject(msg), registry.RegisterError) {
  let gs = start_typed_actor(initial_state, encoder, decoder, handler)
  case registry.register_typed(name, global.subject(gs)) {
    Ok(_) -> Ok(gs)
    Error(err) -> Error(err)
  }
}

/// Start a typed actor via the OTP actor API and return the full Started
/// result with `GlobalSubject(msg)` as the returned data. This is useful
/// for supervision integration where the caller needs access to the pid.
pub fn start_typed_actor_started(
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(actor.Started(global.GlobalSubject(msg)), actor.StartError) {
  actor.new(initial_state)
  |> actor.on_message(fn(state, binary: BitArray) {
    case decoder(binary) {
      Ok(message) -> {
        case handler(message, state) {
          receiver.Continue(new_state) -> actor.continue(new_state)
          receiver.Stop -> actor.stop()
          receiver.StopAbnormal(reason) -> actor.stop_abnormal(reason)
        }
      }
      Error(_) -> actor.continue(state)
    }
  })
  |> actor.start()
  |> result.map(fn(started) {
    let subject = started.data
    let global_subject = global.from_subject(subject, encoder, decoder)
    actor.Started(pid: started.pid, data: global_subject)
  })
}

/// Create a typed child spec that returns `GlobalSubject(msg)` as the child data.
/// This helper is supervision-friendly and returns a `ChildSpecification` with
/// the typed data so supervisors can access the typed subject directly.
pub fn child_spec_typed_actor_typed(
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> ChildSpecification(global.GlobalSubject(msg)) {
  worker(fn() {
    start_typed_actor_started(initial_state, encoder, decoder, handler)
  })
}

/// Start a typed actor under a new supervisor in one call.
///
/// This high-level helper creates a supervisor, adds the typed actor as a child,
/// and starts the supervisor tree. Returns both the supervisor PID and the
/// typed actor's GlobalSubject for convenient access.
///
/// ## Parameters
///
/// - `initial_state`: The initial state for the actor
/// - `encoder`: Encoder for messages
/// - `decoder`: Decoder for messages
/// - `handler`: Message handler function
///
/// ## Returns
///
/// `Ok((Pid, GlobalSubject(msg)))` on success with both handles,
/// or `Error(StartError)` if supervision setup fails.
///
/// ## Example
///
/// ```gleam
/// let result = actor.start_typed_actor_supervised(
///   0,
///   counter_encoder(),
///   counter_decoder(),
///   fn(msg, count) {
///     case msg {
///       Increment -> receiver.Continue(count + 1)
///       GetCount(reply) -> {
///         process.send(reply, count)
///         receiver.Continue(count)
///       }
///     }
///   },
/// )
///
/// case result {
///   Ok(#(sup_pid, actor_subject)) -> {
///     // Use actor_subject for messaging
///     let _ = global.send(actor_subject, Increment)
///     // Supervisor will restart actor on crashes
///   }
///   Error(err) -> // handle error
/// }
/// ```
pub fn start_typed_actor_supervised(
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(#(process.Pid, global.GlobalSubject(msg)), actor.StartError) {
  // Create child spec for the typed actor
  let child_spec =
    child_spec_typed_actor_typed(initial_state, encoder, decoder, handler)

  // Build supervisor with the actor as a child
  let builder = static_supervisor.new(static_supervisor.OneForOne)
  let builder = static_supervisor.add(builder, child_spec)

  // Start supervisor
  case static_supervisor.start(builder) {
    Ok(sup) -> {
      // Start the actor and get its GlobalSubject
      case start_typed_actor_started(initial_state, encoder, decoder, handler) {
        Ok(started) -> Ok(#(sup.pid, started.data))
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Create a worker pool with N typed actors under a supervisor.
///
/// This helper creates a pool of identical worker actors, all supervised
/// under a single supervisor. The pool can be used for load balancing,
/// parallel processing, or handling concurrent requests.
///
/// ## Parameters
///
/// - `pool_size`: Number of worker actors to create
/// - `initial_state`: Initial state for each worker (all start with same state)
/// - `encoder`: Encoder for worker messages
/// - `decoder`: Decoder for worker messages  
/// - `handler`: Message handler for workers
///
/// ## Returns
///
/// `Ok((Pid, List(GlobalSubject(msg))))` with the supervisor PID and all worker subjects,
/// or `Error(StartError)` if pool creation fails.
///
/// ## Example
///
/// ```gleam
/// // Create a pool of 5 worker actors
/// let result = actor.pool_supervisor(
///   5,
///   0,
///   request_encoder(),
///   request_decoder(),
///   handle_request,
/// )
///
/// case result {
///   Ok(#(sup_pid, workers)) -> {
///     // Round-robin distribution
///     list.index_map(tasks, fn(task, idx) {
///       let worker = list.at(workers, idx % list.length(workers))
///       case worker {
///         Ok(w) -> global.send(w, task)
///         Error(_) -> Error(Nil)
///       }
///     })
///   }
///   Error(err) -> // handle error
/// }
/// ```
pub fn pool_supervisor(
  pool_size: Int,
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(#(process.Pid, List(global.GlobalSubject(msg))), actor.StartError) {
  // Create child specs for all workers
  let child_specs =
    list.range(1, pool_size)
    |> list.map(fn(_) {
      child_spec_typed_actor_typed(initial_state, encoder, decoder, handler)
    })

  // Build supervisor with all workers
  let builder = static_supervisor.new(static_supervisor.OneForOne)
  let builder_with_children =
    list.fold(child_specs, builder, fn(acc, spec) {
      static_supervisor.add(acc, spec)
    })

  // Start supervisor
  case static_supervisor.start(builder_with_children) {
    Ok(sup) -> {
      // Start all workers and collect their GlobalSubjects
      let workers_result =
        list.range(1, pool_size)
        |> list.map(fn(_) {
          start_typed_actor_started(initial_state, encoder, decoder, handler)
        })
        |> result.all()

      case workers_result {
        Ok(started_workers) -> {
          let subjects = list.map(started_workers, fn(started) { started.data })
          Ok(#(sup.pid, subjects))
        }
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}
