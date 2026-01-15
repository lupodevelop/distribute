/// Type-safe message reception with automatic decoding.
///
/// This module provides utilities for receiving and decoding messages using
/// the codec system, integrating seamlessly with gleam/erlang/process selectors.
import distribute/codec.{type DecodeError, type Decoder, type Encoder}
import distribute/global
import distribute/log
import gleam/dynamic as dyn
import gleam/erlang/process.{
  type Selector, type Subject, Abnormal, Killed, Normal,
}
import gleam/otp/actor
import gleam/result
import gleam/string

/// Errors that can occur when receiving messages.
pub type ReceiveError {
  /// The message could not be decoded.
  DecodeError(DecodeError)
  /// No message was received before the timeout.
  Timeout
}

/// Receive a single typed message from a Subject with timeout.
///
/// Waits for a message, decodes it using the provided decoder, and returns
/// the decoded value. Returns an error on timeout or decode failure.
pub fn receive_typed(
  subject: Subject(BitArray),
  decoder: Decoder(a),
  timeout_ms: Int,
) -> Result(a, ReceiveError) {
  let selector =
    process.new_selector()
    |> process.select_map(subject, fn(msg) { msg })

  case process.selector_receive(selector, timeout_ms) {
    Ok(binary) ->
      case decoder(binary) {
        Ok(value) -> Ok(value)
        Error(err) -> Error(DecodeError(err))
      }
    Error(Nil) -> Error(Timeout)
  }
}

/// Add a typed message handler to a Selector.
///
/// This function allows you to add a handler for messages from a specific
/// Subject to an existing Selector. The handler will decode messages using
/// the provided decoder and transform them with the mapper function.
///
/// The mapper function receives a `Result` containing either the decoded value
/// or a decode error. This allows the caller to handle malformed messages
/// gracefully (e.g. by logging them or ignoring them).
///
/// ## Parameters
///
/// - `selector`: The Selector to add the handler to.
/// - `subject`: The Subject to receive messages from.
/// - `decoder`: The Decoder to use for decoding binary messages.
/// - `mapper`: A function to transform the decode result into the selector's
///   return type `b`.
///
/// ## Returns
///
/// The updated Selector with the new handler added.
pub fn selecting_typed(
  selector: Selector(b),
  subject: Subject(BitArray),
  decoder: Decoder(a),
  mapper: fn(Result(a, ReceiveError)) -> b,
) -> Selector(b) {
  process.select_map(selector, subject, fn(binary) {
    case decoder(binary) {
      Ok(value) -> mapper(Ok(value))
      Error(err) -> mapper(Error(DecodeError(err)))
    }
  })
}

/// Receive messages in a loop with a typed handler function.
///
/// This function continuously receives messages from a Subject, decodes them,
/// and passes them to a handler function. The handler returns updated state,
/// and the loop continues until externally stopped (process termination).
///
/// Malformed messages that fail to decode are silently skipped.
///
/// ## Parameters
///
/// - `subject`: The Subject to receive messages from.
/// - `decoder`: The Decoder for incoming binary messages.
/// - `initial_state`: The initial state value.
/// - `handler`: Function that processes decoded messages and returns updated state.
///
/// ## Note
///
/// This function runs forever. Use it in a spawned process that can be terminated
/// externally, or structure your handler to perform side effects and ignore state
/// if you want graceful shutdown.
pub fn receive_loop_typed(
  subject: Subject(BitArray),
  decoder: Decoder(a),
  initial_state: state,
  handler: fn(a, state) -> state,
) -> Nil {
  do_receive_loop(subject, decoder, initial_state, handler)
}

fn do_receive_loop(
  subject: Subject(BitArray),
  decoder: Decoder(a),
  state: state,
  handler: fn(a, state) -> state,
) -> Nil {
  let selector =
    process.new_selector()
    |> process.select_map(subject, fn(msg) { msg })

  // Wait indefinitely for next message
  case process.selector_receive_forever(selector) {
    binary ->
      case decoder(binary) {
        Ok(value) -> {
          let new_state = handler(value, state)
          do_receive_loop(subject, decoder, new_state, handler)
        }
        Error(_decode_err) ->
          // Skip malformed messages and continue
          do_receive_loop(subject, decoder, state, handler)
      }
  }
}

/// Start an actor that receives and decodes typed messages.
///
/// This is a convenience wrapper around `actor.start` that handles binary
/// decoding automatically. The returned `Subject` accepts `BitArray` messages,
/// making it suitable for use with `distribute`'s typed messaging API.
///
/// Malformed messages are silently ignored.
///
/// Note: This helper does not support custom selectors in the handler return
/// value (they will be ignored). If you need complex selector logic, use
/// `selecting_typed` with a standard `actor.start_spec`.
///
/// ## Parameters
///
/// - `initial_state`: The initial state of the actor.
/// - `decoder`: The decoder for incoming messages.
/// - `handler`: The message handler function.
///
/// ## Returns
///
/// `Ok(Subject(BitArray))` on success, or `Error(StartError)`.
/// The return value for the handler function in `start_typed_receiver`.
pub type Next(state) {
  /// Continue handling messages with the new state.
  Continue(state: state)
  /// Stop the actor normally.
  Stop
  /// Stop the actor abnormally with a reason.
  StopAbnormal(reason: String)
}

// Internal inbox type used by the global receiver selector to unify
// normal message payloads and trapped EXIT messages from linked processes.
// Defined at module scope so it can be used by the loop implementation.
type Inbox {
  Message(BitArray)
  Exit(process.ExitMessage)
}

/// Start an actor that receives and decodes typed messages.
///
/// This is a convenience wrapper around `actor.start` that handles binary
/// decoding automatically. The returned `Subject` accepts `BitArray` messages,
/// making it suitable for use with `distribute`'s typed messaging API.
///
/// Malformed messages are silently ignored.
///
/// Note: This helper does not support custom selectors in the handler return
/// value. If you need complex selector logic, use `selecting_typed` with a
/// standard `actor.start_spec`.
///
/// ## Parameters
///
/// - `initial_state`: The initial state of the actor.
/// - `decoder`: The decoder for incoming messages.
/// - `handler`: The message handler function.
///
/// ## Returns
///
/// `Ok(Subject(BitArray))` on success, or `Error(StartError)`.
pub fn start_typed_receiver(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> Result(Subject(BitArray), actor.StartError) {
  actor.new(initial_state)
  |> actor.on_message(fn(state, binary) {
    case decoder(binary) {
      Ok(message) -> {
        case handler(message, state) {
          Continue(new_state) -> actor.continue(new_state)
          Stop -> actor.stop()
          StopAbnormal(reason) -> actor.stop_abnormal(reason)
        }
      }
      Error(_) -> actor.continue(state)
    }
  })
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

/// Start a global receiver actor.
///
/// This actor is compatible with `registry.register_typed` and `registry.whereis_typed`.
/// It accepts messages with a `Nil` tag, which allows it to be addressed globally
/// without sharing a specific Reference tag.
///
/// Use this function if you intend to register the process using `registry.register_typed`.
pub fn start_global_receiver(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> Subject(BitArray) {
  let pid =
    process.spawn(fn() {
      // Ensure exit signals are delivered as messages so we can handle them
      // gracefully instead of letting the process be killed abruptly.
      process.trap_exits(True)
      let subject = process.unsafely_create_subject(process.self(), dyn.nil())
      global_loop(initial_state, subject, decoder, handler)
    })
  process.unsafely_create_subject(pid, dyn.nil())
}

fn global_loop(
  state: state,
  subject: Subject(BitArray),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> Nil {
  let selector =
    process.new_selector()
    |> process.select_map(subject, fn(msg) { Message(msg) })
    |> process.select_trapped_exits(fn(exit_msg) { Exit(exit_msg) })

  case process.selector_receive_forever(selector) {
    Message(binary) ->
      case decoder(binary) {
        Ok(value) -> {
          case handler(value, state) {
            Continue(new_state) ->
              global_loop(new_state, subject, decoder, handler)
            Stop -> Nil
            StopAbnormal(_reason) -> Nil
          }
        }
        Error(_) -> global_loop(state, subject, decoder, handler)
      }

    Exit(process.ExitMessage(pid, reason)) -> {
      // Translate exit reason into a string for logging and diagnostics.
      let reason_str = case reason {
        Normal -> "normal"
        Killed -> "killed"
        Abnormal(r) -> string.inspect(r)
      }

      // Log at warning level so operators can observe abnormal terminations.
      log.warn("Linked process exited, shutting down receiver", [
        #("pid", string.inspect(pid)),
        #("reason", reason_str),
      ])

      // Stop the loop: treat a Normal exit as a graceful stop and others as abnormal.
      Nil
    }
  }
}

/// Start a typed actor that returns a GlobalSubject (RECOMMENDED).
///
/// This is the recommended way to create type-safe actors for distributed use.
/// The returned `GlobalSubject` enforces encoder/decoder usage and can be
/// registered globally with `registry.register_typed`.
///
/// Malformed messages are silently ignored.
///
/// ## Parameters
///
/// - `initial_state`: The initial state of the actor.
/// - `encoder`: The encoder for outgoing messages (used by clients).
/// - `decoder`: The decoder for incoming messages.
/// - `handler`: The message handler function returning `Next(state)`.
///
/// ## Returns
///
/// A `GlobalSubject(msg)` that can be used for type-safe messaging.
///
/// ## Example
///
/// ```gleam
/// pub type Request {
///   GetCount
///   Increment
/// }
///
/// let actor = receiver.start_typed_actor(
///   0,
///   my_encoder(),
///   my_decoder(),
///   fn(msg, count) {
///     case msg {
///       GetCount -> {
///         // Send response logic here
///         receiver.Continue(count)
///       }
///       Increment -> receiver.Continue(count + 1)
///     }
///   },
/// )
/// ```
pub fn start_typed_actor(
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> global.GlobalSubject(msg) {
  let subject = start_global_receiver(initial_state, decoder, handler)
  global.from_subject(subject, encoder, decoder)
}
