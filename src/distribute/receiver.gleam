/// Typed message reception and OTP actor wrappers.
import distribute/codec.{type DecodeError, type Decoder, type Encoder}
import distribute/config
import distribute/global
import distribute/telemetry
import gleam/bit_array
import gleam/dynamic
import gleam/erlang/process.{type Selector, type Subject}
import gleam/otp/actor
import gleam/result

/// Internal protocol for the actor wrappers.
///
/// The receiver actors register themselves under a globally-known name,
/// so any process on any cluster node can `erlang:send` arbitrary terms
/// straight into their mailbox. `gleam/otp/actor` is built on
/// `process.selector_receive`: terms that do not match the configured
/// selector are NOT silently dropped, they accumulate forever and pay a
/// linear selective-receive penalty on every subsequent message.
///
/// To stop a remote attacker (or a misconfigured peer) from filling the
/// mailbox with garbage, the actor selector explicitly maps non-Subject
/// terms into `Garbage` and the handler drops them with `actor.continue`.
type WorkerInbox {
  Incoming(BitArray)
  Garbage
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

pub type ReceiveError {
  DecodeError(DecodeError)
  /// Receive timed out before a message arrived. Renamed in v4.0.0
  /// from `Timeout` to disambiguate from `global.CallError.Timeout`.
  ReceiveTimeout
}

pub fn receive_error_to_string(err: ReceiveError) -> String {
  case err {
    DecodeError(e) ->
      "Receive decode failed: " <> codec.decode_error_to_string(e)
    ReceiveTimeout -> "Receive timed out"
  }
}

/// Result of a typed message handler. Renamed in v4.0.0 from `Next` so
/// it no longer collides with `gleam/otp/actor.Next` when both modules
/// are imported unqualified.
pub type HandlerStep(state) {
  Continue(state)
  Stop
  StopAbnormal(reason: String)
}

// ---------------------------------------------------------------------------
// One-shot receive
// ---------------------------------------------------------------------------

/// Receive and decode one message.
///
/// Creates a fresh selector on each call. For tight loops, prefer building
/// a selector once with `selecting_typed` and reusing it with
/// `process.selector_receive`.
///
/// Rejects payloads that exceed `config.get().max_payload_size_bytes` with
/// `Error(DecodeError(PayloadTooLarge(size)))`. The decoder is never invoked
/// on oversized binaries.
///
/// Negative `timeout_ms` is clamped to `0` (poll-once-and-return). Erlang's
/// `receive after Timeout -> ...` would otherwise raise `timeout_value`
/// and crash the caller; a typed `ReceiveTimeout` is a better contract.
pub fn receive_typed(
  subject: Subject(BitArray),
  decoder: Decoder(a),
  timeout_ms: Int,
) -> Result(a, ReceiveError) {
  let selector =
    process.new_selector()
    |> process.select_map(subject, fn(msg) { msg })

  case process.selector_receive(selector, clamp_timeout(timeout_ms)) {
    Ok(binary) ->
      case
        decode_checked(
          binary,
          decoder,
          telemetry.PayloadOnReceive,
          telemetry.DecodeOnReceive,
        )
      {
        Ok(value) -> Ok(value)
        Error(err) -> Error(DecodeError(err))
      }
    Error(Nil) -> Error(ReceiveTimeout)
  }
}

/// Clamp a user-supplied timeout to a non-negative value before it
/// reaches an Erlang `receive after Timeout -> ...` expression.
/// Shared with `global.clamp_timeout` via FFI so the clamp policy
/// cannot drift between modules.
@external(erlang, "distribute_ffi_utils", "clamp_timeout")
fn clamp_timeout(ms: Int) -> Int

// ---------------------------------------------------------------------------
// Selector integration
// ---------------------------------------------------------------------------

/// Add a typed handler to a `Selector`.
///
/// Rejects payloads exceeding `config.get().max_payload_size_bytes` with
/// `Error(DecodeError(PayloadTooLarge(size)))`. The decoder is never invoked
/// on oversized binaries.
pub fn selecting_typed(
  selector: Selector(b),
  subject: Subject(BitArray),
  decoder: Decoder(a),
  mapper: fn(Result(a, ReceiveError)) -> b,
) -> Selector(b) {
  process.select_map(selector, subject, fn(binary) {
    case
      decode_checked(
        binary,
        decoder,
        telemetry.PayloadOnReceive,
        telemetry.DecodeOnReceive,
      )
    {
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
///
/// Decode errors (including `PayloadTooLarge`) are silently dropped.
/// Use `start_receiver_observed` to log or meter malformed messages.
pub fn start_receiver(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> HandlerStep(state),
) -> Result(Subject(BitArray), actor.StartError) {
  start_receiver_observed(initial_state, decoder, handler, fn(_) { Nil })
}

/// Like `start_receiver` but calls `on_decode_error` for every binary
/// that fails decoding, before continuing with the current state.
///
/// Oversized payloads (over `config.get().max_payload_size_bytes`) trigger
/// `on_decode_error(PayloadTooLarge(size))` and are NOT forwarded to the
/// decoder. This protects the actor from OOM on malicious or buggy senders.
///
/// Non-Subject mailbox terms (raw `Pid ! garbage` from anywhere) are
/// silently dropped via `select_other` so they cannot accumulate and
/// pay the selective-receive penalty.
pub fn start_receiver_observed(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> HandlerStep(state),
  on_decode_error: fn(DecodeError) -> Nil,
) -> Result(Subject(BitArray), actor.StartError) {
  // Build a dedicated `Subject(BitArray)` for callers and a selector
  // that maps incoming binaries into our internal `WorkerInbox` while
  // routing every other mailbox term into `Garbage` for active drop.
  let started =
    actor.new_with_initialiser(
      config.get().default_init_timeout_ms,
      fn(_default_subject) {
        let my_subject: Subject(BitArray) = process.new_subject()
        let selector =
          process.new_selector()
          |> process.select_map(my_subject, Incoming)
          |> process.select_other(fn(_dyn: dynamic.Dynamic) { Garbage })
        Ok(
          actor.initialised(initial_state)
          |> actor.selecting(selector)
          |> actor.returning(my_subject),
        )
      },
    )
    |> actor.on_message(fn(state, msg) {
      case msg {
        Garbage -> actor.continue(state)
        Incoming(binary) ->
          case decode_observed(binary, decoder, on_decode_error) {
            Ok(message) -> translate_next(handler(message, state))
            Error(Nil) -> actor.continue(state)
          }
      }
    })
    |> actor.start()
  result.map(started, fn(s) { s.data })
}

// ---------------------------------------------------------------------------
// Distributed actor (name-tagged Subject)
// ---------------------------------------------------------------------------

/// Start a distributed actor whose subject carries a deterministic name-based
/// tag. Remote nodes can reconstruct the subject via `registry.lookup`.
///
/// Decode errors (including `PayloadTooLarge`) are silently dropped; use
/// `start_distributed_worker_observed` to log or meter them.
///
/// `init_timeout_ms` is the OTP initialiser timeout passed to
/// `actor.new_with_initialiser`.
pub fn start_distributed_worker(
  name: String,
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> HandlerStep(state),
  init_timeout_ms: Int,
) -> Result(actor.Started(global.GlobalSubject(msg)), actor.StartError) {
  start_distributed_worker_observed(
    name,
    initial_state,
    encoder,
    decoder,
    handler,
    fn(_) { Nil },
    init_timeout_ms,
  )
}

/// Like `start_distributed_worker` but calls `on_decode_error` for every
/// binary that fails decoding. Use this to log codec mismatches across nodes
/// (e.g. during rolling deploys).
///
/// Oversized payloads (over `config.get().max_payload_size_bytes`) trigger
/// `on_decode_error(PayloadTooLarge(size))` and are NOT forwarded to the
/// decoder. Critical defence against cross-node OOM attacks.
///
/// Non-Subject mailbox terms (raw `erlang:send(global:whereis_name(...),
/// garbage)` from any cluster node) are silently dropped via
/// `select_other`. Without this drain the unmatched terms would
/// accumulate in the mailbox forever and pay the linear selective-
/// receive penalty on every subsequent message. A remote DoS vector
/// open to any peer that knows the registered name.
pub fn start_distributed_worker_observed(
  name: String,
  initial_state: state,
  encoder: Encoder(msg),
  decoder: Decoder(msg),
  handler: fn(msg, state) -> HandlerStep(state),
  on_decode_error: fn(DecodeError) -> Nil,
  init_timeout_ms: Int,
) -> Result(actor.Started(global.GlobalSubject(msg)), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms, fn(_default_subject) {
    let gs = global.unsafe_from_name(name, process.self(), encoder, decoder)
    let selector =
      process.new_selector()
      |> process.select_map(global.subject(gs), Incoming)
      |> process.select_other(fn(_dyn: dynamic.Dynamic) { Garbage })
    Ok(
      actor.initialised(initial_state)
      |> actor.selecting(selector)
      |> actor.returning(gs),
    )
  })
  |> actor.on_message(fn(state, msg) {
    case msg {
      Garbage -> actor.continue(state)
      Incoming(binary) ->
        case decode_observed(binary, decoder, on_decode_error) {
          Ok(message) -> translate_next(handler(message, state))
          Error(Nil) -> actor.continue(state)
        }
    }
  })
  |> actor.start()
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/// Decode with payload size enforcement and telemetry emission.
///
/// Single source of truth for "size check before decode". Used by every
/// path that turns a raw binary into a typed message; the `payload_origin`
/// and `decode_origin` arguments distinguish the call site so the
/// telemetry sink can label events accurately.
fn decode_checked(
  binary: BitArray,
  decoder: Decoder(a),
  payload_origin: telemetry.PayloadOrigin,
  decode_origin: telemetry.DecodeOrigin,
) -> Result(a, DecodeError) {
  let cap = config.get().max_payload_size_bytes
  let size = bit_array.byte_size(binary)
  case size > cap {
    True -> {
      telemetry.emit(telemetry.PayloadRejected(size, cap, payload_origin))
      Error(codec.PayloadTooLarge(size))
    }
    False ->
      case decoder(binary) {
        Ok(v) -> Ok(v)
        Error(e) -> {
          telemetry.emit(telemetry.DecodeFailed(
            codec.decode_error_to_string(e),
            decode_origin,
          ))
          Error(e)
        }
      }
  }
}

/// Decode with size enforcement plus an observability hook.
///
/// On any error (oversized OR decode failure), calls `on_decode_error` and
/// returns `Error(Nil)`. The actor handler uses this to drop bad messages
/// while still notifying instrumentation.
fn decode_observed(
  binary: BitArray,
  decoder: Decoder(a),
  on_decode_error: fn(DecodeError) -> Nil,
) -> Result(a, Nil) {
  case
    decode_checked(
      binary,
      decoder,
      telemetry.PayloadOnActorInbound,
      telemetry.DecodeOnActorInbound,
    )
  {
    Ok(value) -> Ok(value)
    Error(err) -> {
      on_decode_error(err)
      Error(Nil)
    }
  }
}

fn translate_next(step: HandlerStep(state)) -> actor.Next(state, msg) {
  case step {
    Continue(s) -> actor.continue(s)
    Stop -> actor.stop()
    StopAbnormal(reason) -> actor.stop_abnormal(reason)
  }
}
