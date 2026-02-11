/// Typed message reception and OTP actor wrappers.
import distribute/codec.{type DecodeError, type Decoder, type Encoder}
import distribute/global
import gleam/erlang/process.{type Selector, type Subject}
import gleam/otp/actor
import gleam/result

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

pub type ReceiveError {
  DecodeError(DecodeError)
  Timeout
}

pub type Next(state) {
  Continue(state)
  Stop
  StopAbnormal(reason: String)
}

// ---------------------------------------------------------------------------
// One-shot receive
// ---------------------------------------------------------------------------

/// Receive and decode one message.
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

// ---------------------------------------------------------------------------
// Selector integration
// ---------------------------------------------------------------------------

/// Add a typed handler to a `Selector`.
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

// ---------------------------------------------------------------------------
// OTP actor wrapper (local Subject)
// ---------------------------------------------------------------------------

/// Start an OTP actor that decodes binary messages and forwards
/// them to `handler`. Returns the raw `Subject(BitArray)`.
pub fn start_receiver(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> Result(Subject(BitArray), actor.StartError) {
  actor.new(initial_state)
  |> actor.on_message(fn(state, binary) {
    case decoder(binary) {
      Ok(message) -> translate_next(handler(message, state))
      Error(_) -> actor.continue(state)
    }
  })
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

// ---------------------------------------------------------------------------
// Distributed actor (nil-tagged Subject, linked)
// ---------------------------------------------------------------------------

/// Start a gen_statem actor with a deterministic name-based tag.
/// Remote nodes can reconstruct the Subject via `registry.lookup`.
pub fn start_distributed_worker(
  name: String,
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> Result(actor.Started(global.GlobalSubject(msg)), actor.StartError) {
  actor.new_with_initialiser(5000, fn(_default_subject) {
    // Create a GlobalSubject with a deterministic tag based on the name.
    // This tag can be reconstructed on any node that knows the name.
    let gs = global.from_name(name, process.self(), encoder, decoder)

    // Build a selector that listens on the name-tagged Subject.
    let selector =
      process.new_selector()
      |> process.select(global.subject(gs))

    Ok(
      actor.initialised(initial_state)
      |> actor.selecting(selector)
      |> actor.returning(gs),
    )
  })
  |> actor.on_message(fn(state, binary: BitArray) {
    case decoder(binary) {
      Ok(message) -> translate_next(handler(message, state))
      Error(_) -> actor.continue(state)
    }
  })
  |> actor.start()
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

fn translate_next(next: Next(state)) -> actor.Next(state, BitArray) {
  case next {
    Continue(s) -> actor.continue(s)
    Stop -> actor.stop()
    StopAbnormal(reason) -> actor.stop_abnormal(reason)
  }
}
