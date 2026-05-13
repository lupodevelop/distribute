/// Typed wrapper around `Subject(BitArray)` for cross-node messaging.
///
/// Pairs a subject with an encoder and decoder. Public constructors:
/// `new`, `from_pid`, `from_subject`. The reconstruction-from-name
/// path is `unsafe_from_name`, which is `@internal` and intended for
/// `registry.lookup` to call.
import distribute/codec
import distribute/config
import distribute/telemetry
import gleam/bit_array
import gleam/dynamic
import gleam/erlang/process
import gleam/int

// ---------------------------------------------------------------------------
// Subject construction. Single point of coupling with gleam_erlang internals
// ---------------------------------------------------------------------------

/// Build a Subject from a PID and a tag.
/// We keep our own FFI rather than depending on the @internal
/// `process.unsafely_create_subject`.
@external(erlang, "distribute_ffi_utils", "create_subject")
fn create_subject(
  owner: process.Pid,
  tag: dynamic.Dynamic,
) -> process.Subject(msg)

// ---------------------------------------------------------------------------
// Type
// ---------------------------------------------------------------------------

/// A subject bundled with its codec, usable across nodes.
pub opaque type GlobalSubject(msg) {
  GlobalSubject(
    subject: process.Subject(BitArray),
    encoder: codec.Encoder(msg),
    decoder: codec.Decoder(msg),
  )
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

/// New subject owned by the current process, with a unique tag.
pub fn new(
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> GlobalSubject(msg) {
  let subject: process.Subject(BitArray) = process.new_subject()
  GlobalSubject(subject:, encoder:, decoder:)
}

/// **Send-only.** A `GlobalSubject` built via `from_pid` must only be
/// used with `send`. Calling `receive`, `call`, or `call_isolated` on
/// it is a logic error: the shared `Nil` tag collides with the actor's
/// own mailbox selectors and produces silent message interleaving.
/// For any bidirectional path, register the actor and use
/// `registry.lookup` (which goes through the internal
/// `unsafe_from_name` constructor with a verified codec pairing).
///
/// ## When to use
///
/// Diagnostic and probe paths only. For example, calling on a PID you
/// just confirmed dead to test `TargetDown` semantics. Production
/// messaging should always go through `registry.lookup`.
///
/// ## What goes wrong otherwise
///
/// All subjects produced by `from_pid` on the same PID share the same
/// `Nil` tag. The BEAM does not distinguish them: messages sent through
/// any one of them all land in the same mailbox slot, *interleaved with
/// the actor's default subject*. Two `from_pid` Subjects with different
/// `msg` types pointing at the same actor will silently mix on the wire.
/// The compiler catches the *type* mismatch only if you use the same
/// `GlobalSubject` value at both ends. This is exactly what `from_pid`
/// makes hard to guarantee.
///
/// The `unsafe_from_name` path used by `registry.lookup` avoids this:
/// each name produces a distinct deterministic tag, so messages routed
/// through the registry cannot collide with each other or with the
/// actor's own selectors.
pub fn from_pid(
  pid: process.Pid,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> GlobalSubject(msg) {
  let subject = create_subject(pid, dynamic.nil())
  GlobalSubject(subject:, encoder:, decoder:)
}

/// Wrap an existing `Subject(BitArray)`, keeping its tag.
pub fn from_subject(
  subject: process.Subject(BitArray),
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> GlobalSubject(msg) {
  GlobalSubject(subject:, encoder:, decoder:)
}

/// Reconstruct a `GlobalSubject` from a name, PID and codec pair.
///
/// **`unsafe_` prefix is load-bearing.** This function lets the
/// caller pin *any* codec to *any* (name, PID) pair: nothing here
/// checks that the encoder and decoder match what the receiving
/// actor actually expects, and a forged subject built with the
/// wrong codec will silently encode garbage into a peer's mailbox.
/// The receiver will fail decode at the boundary (typed
/// `DecodeFailed`, message dropped, telemetry emit), so the wire
/// is still safe. But the caller's mental model "I have a
/// `GlobalSubject(MyMsg)`, therefore the actor at the other end
/// receives `MyMsg`" is the lie this function can tell.
///
/// External callers should use `registry.lookup` instead. It
/// threads the codec through the `TypedName` opaque value and
/// guarantees the `(name, encoder, decoder)` tuple is the same one
/// the actor was registered with.
///
/// Kept exported (and not just module-private) because Gleam has
/// no friend-module visibility: `distribute/registry`,
/// `distribute/receiver`, and the actor module need to call this
/// across module boundaries to implement the lookup path. The
/// `unsafe_` prefix flags the contract loud enough that an
/// accidental call from user code reads as a code smell on review.
/// `@internal` additionally drops it from generated docs and IDE
/// autocomplete.
@internal
pub fn unsafe_from_name(
  name: String,
  pid: process.Pid,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> GlobalSubject(msg) {
  let subject = create_subject(pid, dynamic.string(name))
  GlobalSubject(subject:, encoder:, decoder:)
}

// ---------------------------------------------------------------------------
// Accessors
// ---------------------------------------------------------------------------

pub fn subject(global: GlobalSubject(msg)) -> process.Subject(BitArray) {
  global.subject
}

pub fn owner(global: GlobalSubject(msg)) -> Result(process.Pid, Nil) {
  process.subject_owner(global.subject)
}

pub fn encoder(global: GlobalSubject(msg)) -> codec.Encoder(msg) {
  global.encoder
}

pub fn decoder(global: GlobalSubject(msg)) -> codec.Decoder(msg) {
  global.decoder
}

// ---------------------------------------------------------------------------
// Send error
// ---------------------------------------------------------------------------

pub type SendError {
  /// The encoder returned an error.
  SendEncodeFailed(codec.EncodeError)
  /// The encoded payload exceeds `config.max_payload_size_bytes`.
  /// The `Int` is the actual byte size.
  PayloadTooLarge(Int)
}

pub fn send_error_to_string(error: SendError) -> String {
  case error {
    SendEncodeFailed(e) -> "Send failed: " <> codec.encode_error_to_string(e)
    PayloadTooLarge(size) ->
      "Payload too large: " <> int.to_string(size) <> " bytes"
  }
}

// ---------------------------------------------------------------------------
// Messaging
// ---------------------------------------------------------------------------

/// Encode and send a message.
///
/// Returns `Error(PayloadTooLarge(size))` when the encoded payload exceeds
/// `config.get().max_payload_size_bytes`. The message is never enqueued.
///
/// ## Fire-and-forget semantics
///
/// `Ok(Nil)` means the BEAM accepted the message for local
/// dispatch. It does **not** mean the target received it. The
/// receiver may have died after a successful `lookup`, the
/// inter-node connection may drop before the payload is on the
/// wire, the receiving node may panic before its mailbox is read.
/// Successful `send` is the BEAM contract for `erlang:send/2`:
/// "queued for delivery", not "delivered".
///
/// If you need delivery confirmation, use `call` (synchronous,
/// monitor-backed: `Error(TargetDown)` fires immediately when the
/// PID is dead, `Error(Timeout)` fires when the reply does not
/// arrive in time). For higher-level reliability (at-least-once,
/// idempotency tokens, durable queues) build it on top of `call`
/// or `send` plus your own ack protocol. The library does not
/// bake retries in because the right strategy is application-
/// specific.
pub fn send(
  global: GlobalSubject(msg),
  message: msg,
) -> Result(Nil, SendError) {
  case codec.encode(global.encoder, message) {
    Error(e) -> Error(SendEncodeFailed(e))
    Ok(binary) -> {
      let cap = config.get().max_payload_size_bytes
      let size = bit_array.byte_size(binary)
      case size > cap {
        True -> {
          telemetry.emit(telemetry.PayloadRejected(
            size,
            cap,
            telemetry.PayloadOnSend,
          ))
          Error(PayloadTooLarge(size))
        }
        False -> {
          process.send(global.subject, binary)
          Ok(Nil)
        }
      }
    }
  }
}

/// Receive and decode a message. Only works on subjects you own.
///
/// Returns `Error(codec.PayloadTooLarge(size))` when the received binary
/// exceeds `config.get().max_payload_size_bytes`. Decode is skipped.
///
/// Negative `timeout_ms` is clamped to `0` (poll-once-and-return)
/// rather than propagated to Erlang's `receive after Timeout` clause,
/// which would raise `timeout_value` and crash the caller. A bug at
/// the call site that produces a negative number surfaces as an
/// immediate `DecodeTimeout` instead of a process exit.
pub fn receive(
  global: GlobalSubject(msg),
  timeout_ms: Int,
) -> Result(msg, codec.DecodeError) {
  case process.receive(global.subject, clamp_timeout(timeout_ms)) {
    Error(Nil) -> Error(codec.DecodeTimeout)
    Ok(binary) -> {
      let cap = config.get().max_payload_size_bytes
      let size = bit_array.byte_size(binary)
      case size > cap {
        True -> {
          telemetry.emit(telemetry.PayloadRejected(
            size,
            cap,
            telemetry.PayloadOnReceive,
          ))
          Error(codec.PayloadTooLarge(size))
        }
        False ->
          case codec.decode(global.decoder, binary) {
            Ok(value) -> Ok(value)
            Error(e) -> {
              telemetry.emit(telemetry.DecodeFailed(
                codec.decode_error_to_string(e),
                telemetry.DecodeOnReceive,
              ))
              Error(e)
            }
          }
      }
    }
  }
}

/// Clamp a user-supplied timeout to a non-negative value before it
/// reaches an Erlang `receive after Timeout -> ...` expression.
/// Erlang raises `timeout_value` for negative timeouts. We prefer a
/// typed `DecodeTimeout` / `Timeout` over a process crash. Implemented
/// in `distribute_ffi_utils:clamp_timeout/1` so `global` and
/// `receiver` cannot drift on the clamp policy.
@external(erlang, "distribute_ffi_utils", "clamp_timeout")
fn clamp_timeout(ms: Int) -> Int

@external(erlang, "distribute_ffi_utils", "monotonic_ms")
fn monotonic_ms() -> Int

// ---------------------------------------------------------------------------
// Request/response (call pattern)
// ---------------------------------------------------------------------------

pub type CallError {
  /// No reply arrived within the timeout.
  Timeout
  /// The target process was not alive at call time, or died before replying.
  TargetDown
  /// Encoding the request failed.
  CallEncodeFailed(codec.EncodeError)
  /// The response could not be decoded.
  CallDecodeFailed(codec.DecodeError)
  /// The request or response payload exceeds `config.max_payload_size_bytes`.
  /// The `Int` is the actual byte size of the offending payload.
  CallPayloadTooLarge(Int)
}

// Internal union for the call selector keeps the reply path type-safe.
type CallMessage {
  GotReply(BitArray)
  TargetDied
}

/// Synchronous request/response with monitor-based `TargetDown` detection.
///
/// See also: `call_default/3` (uses configured timeout),
/// `call_isolated/4` (mailbox-safe variant for long-running callers).
///
/// Flow:
/// 1. Resolve target PID and set a monitor. Dead target → `TargetDown`
/// 2. Encode request. Failure → `CallEncodeFailed`
/// 3. Size-check request. Oversized → `CallPayloadTooLarge`
/// 4. Send request
/// 5. Wait for reply OR monitor Down. Timeout → `Timeout`, Down → `TargetDown`
/// 6. Size-check response. Oversized → `CallPayloadTooLarge`
/// 7. Decode response. Failure → `CallDecodeFailed`
///
/// ## Late-reply caveat (read carefully)
///
/// `drain_reply` clears reply messages already in the caller's mailbox
/// at the moment the timeout fires, but the BEAM has no API (without
/// `erlang:alias/0`-aware Subjects, which would require bypassing
/// `gleam_erlang`'s Subject layout) to drop messages that arrive
/// *after* `call` returns. A late reply lands in the caller's mailbox
/// tagged with the orphan reply Subject and **stays there forever**.
/// no selector ever matches it again.
///
/// **`gleam/otp/actor` is NOT immune.** Its receive loop is built on
/// `process.selector_receive`, so unmatched messages are not dropped:
/// they accumulate in the actor's mailbox. The BEAM then pays the
/// "selective receive penalty". Every subsequent `selector_receive`
/// scans past every orphan first. A long-running actor that issues
/// thousands of timed-out `call`s will gradually grind to a halt and
/// can OOM.
///
/// The only safe mitigations:
///
/// - **Short-lived callers** (CLI tools, scripts, request handlers
///   that exit after returning their response): orphan messages die
///   with the process. `call` directly is fine.
/// - **High-volume callers, including long-running OTP actors that
///   issue many `call`s**: isolate every `call` in a short-lived
///   proxy process. The proxy makes the call, forwards the result to
///   the real caller via a one-shot Subject, then exits. Its
///   mailbox (and any orphan reply) is reaped. See the "Isolated
///   call" recipe in `docs/recipes.md`. This is the only design that
///   bounds the caller's mailbox under sustained timeouts.
pub fn call(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
  timeout_ms: Int,
) -> Result(resp, CallError) {
  let clamped_timeout_ms = clamp_timeout(timeout_ms)
  case process.subject_owner(target.subject) {
    Error(Nil) -> {
      telemetry.emit(telemetry.CallTargetDown)
      Error(TargetDown)
    }
    Ok(target_pid) -> {
      let mon = process.monitor(target_pid)
      let reply_subject: process.Subject(BitArray) = process.new_subject()
      let request = make_request(reply_subject)
      case codec.encode(target.encoder, request) {
        Error(e) -> {
          process.demonitor_process(mon)
          Error(CallEncodeFailed(e))
        }
        Ok(req_bits) -> {
          let cap = config.get().max_payload_size_bytes
          let req_size = bit_array.byte_size(req_bits)
          case req_size > cap {
            True -> {
              process.demonitor_process(mon)
              telemetry.emit(telemetry.PayloadRejected(
                req_size,
                cap,
                telemetry.PayloadOnCallRequest,
              ))
              Error(CallPayloadTooLarge(req_size))
            }
            False -> {
              process.send(target.subject, req_bits)
              let selector =
                process.new_selector()
                |> process.select_map(reply_subject, GotReply)
                |> process.select_specific_monitor(mon, fn(_) { TargetDied })
              case process.selector_receive(selector, clamped_timeout_ms) {
                Error(Nil) -> {
                  process.demonitor_process(mon)
                  drain_reply(reply_subject)
                  telemetry.emit(telemetry.CallTimedOut(clamped_timeout_ms))
                  Error(Timeout)
                }
                Ok(TargetDied) -> {
                  drain_reply(reply_subject)
                  telemetry.emit(telemetry.CallTargetDown)
                  Error(TargetDown)
                }
                Ok(GotReply(resp_bits)) -> {
                  process.demonitor_process(mon)
                  let resp_size = bit_array.byte_size(resp_bits)
                  case resp_size > cap {
                    True -> {
                      telemetry.emit(telemetry.PayloadRejected(
                        resp_size,
                        cap,
                        telemetry.PayloadOnCallResponse,
                      ))
                      Error(CallPayloadTooLarge(resp_size))
                    }
                    False ->
                      case codec.decode(response_decoder, resp_bits) {
                        Ok(value) -> Ok(value)
                        Error(e) -> {
                          telemetry.emit(telemetry.DecodeFailed(
                            codec.decode_error_to_string(e),
                            telemetry.DecodeOnCallReply,
                          ))
                          Error(CallDecodeFailed(e))
                        }
                      }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Like `receive`, but uses `config.get().default_call_timeout_ms` as the
/// timeout. Receive and call share the same "wait for a message" semantics,
/// so they share the configured default rather than introducing a third
/// timeout knob.
pub fn receive_default(
  global: GlobalSubject(msg),
) -> Result(msg, codec.DecodeError) {
  receive(global, config.get().default_call_timeout_ms)
}

/// Like `call`, but uses `config.get().default_call_timeout_ms` as the timeout.
pub fn call_default(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
) -> Result(resp, CallError) {
  call(
    target,
    make_request,
    response_decoder,
    config.get().default_call_timeout_ms,
  )
}

const isolated_call_proxy_buffer_ms: Int = 100

fn isolated_proxy_shutdown_grace_ms() -> Int {
  config.get().isolated_proxy_shutdown_grace_ms
}

// Upper bound for timeout-path reply draining in `call`: if a buggy or
// hostile peer keeps flooding replies after the caller timeout, draining
// forever would violate the timeout contract and stall the caller.
const call_timeout_drain_max_messages: Int = 2048

const call_timeout_drain_max_ms: Int = 5

/// Mailbox-safe variant of `call`: each invocation runs the actual
/// `call` inside a short-lived proxy process. The caller waits on a
/// one-shot result Subject; when the proxy exits, its mailbox and any
/// orphan late-reply tagged with the proxy-owned reply Subject are
/// reaped by the BEAM at zero cost.
///
/// Use this from long-running callers that issue many `call`s and
/// would otherwise accumulate orphan messages under sustained
/// timeouts: see the "Late-reply caveat" on `call/4`. For one-shot
/// callers (CLI, scripts) and short-lived processes the plain
/// `call/4` is cheaper.
///
/// The proxy is **spawned unlinked** and tracked via `process.monitor`.
/// An earlier draft used `process.spawn` (linked); a linked proxy that
/// somehow exits abnormally would propagate the exit signal back to the
/// caller, defeating the isolation guarantee. With unlinked + monitor,
/// the caller observes either the result (`Ok(...)`), an unexpected
/// proxy `DOWN` (returned as `Error(Timeout)`), or its own receive
/// timeout buffer expiring. The inner `call` could not produce a reply.
/// On caller-side timeout, `call_isolated` now kills the proxy, waits for
/// its `DOWN`, and only then drains the result Subject; this closes the
/// late-send race where a still-running proxy could otherwise pollute the
/// caller mailbox after return. In every path the caller stays alive.
///
/// Costs: one extra process spawn + one cross-process hop per call.
/// On the BEAM this is microseconds.
/// Like `call_isolated`, with `config.get().default_call_timeout_ms`.
pub fn call_isolated_default(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
) -> Result(resp, CallError) {
  call_isolated(
    target,
    make_request,
    response_decoder,
    config.get().default_call_timeout_ms,
  )
}

pub fn call_isolated(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
  timeout_ms: Int,
) -> Result(resp, CallError) {
  let clamped_timeout_ms = clamp_timeout(timeout_ms)
  let result_subject: process.Subject(Result(resp, CallError)) =
    process.new_subject()
  let proxy_pid =
    process.spawn_unlinked(fn() {
      process.send(
        result_subject,
        call(target, make_request, response_decoder, timeout_ms),
      )
      // Proxy exits normally. Its mailbox, which holds any late
      // reply that landed past the inner `call` deadline. Dies with it.
    })
  let proxy_mon = process.monitor(proxy_pid)
  let selector =
    process.new_selector()
    |> process.select_map(result_subject, IsolatedReady)
    |> process.select_specific_monitor(proxy_mon, fn(_) { IsolatedProxyDown })
  // Add a small buffer past the inner timeout for the proxy hop.
  case
    process.selector_receive(
      selector,
      clamped_timeout_ms + isolated_call_proxy_buffer_ms,
    )
  {
    Ok(IsolatedReady(result)) -> {
      // Discard the DOWN that fires when the proxy exits normally
      // after sending; the demonitor flush keeps the mailbox clean.
      process.demonitor_process(proxy_mon)
      result
    }
    Ok(IsolatedProxyDown) -> {
      // Proxy died before sending its result. Possible causes:
      // a crash inside `make_request`, the response codec, or another
      // component evaluated by the proxy. The caller still sees
      // `Error(Timeout)` for typed-error consistency, but the dedicated
      // `CallProxyCrashed` event distinguishes the cause from a real
      // RPC timeout so observability can flag user-side bugs.
      // The DOWN signal that fired this branch already cleared the
      // monitor, so no demonitor is needed.
      telemetry.emit(telemetry.CallProxyCrashed)
      Error(Timeout)
    }
    Error(Nil) -> {
      // Caller-side buffer expired with the proxy still in flight.
      // Kill the proxy and wait for its DOWN before draining the
      // result Subject. `process.kill` is asynchronous, so draining
      // first would reopen the race and let a late proxy send land in
      // the caller's mailbox after return.
      //
      // Accepted residual risk: a NIF-bound proxy that does not yield
      // within `isolated_proxy_shutdown_grace_ms` falls through to the
      // demonitor fallback. If the proxy was mid-`process.send` at
      // that moment, the BEAM may complete the send into the caller's
      // mailbox after our single-shot drain returned. A microsecond
      // window dependent on scheduler ordering. The fundamental fix
      // requires `erlang:alias/0`-aware Subjects (out of scope for
      // v4); documented in `docs/safety_and_limits.md` under
      // "Late-reply mailbox accumulation". Practical impact: at most
      // one orphan binary per affected call, in a path that already
      // requires a misbehaving NIF inside `make_request` or codec.
      process.kill(proxy_pid)
      let down_selector =
        process.new_selector()
        |> process.select_specific_monitor(proxy_mon, fn(_) { Nil })
      case
        process.selector_receive(
          down_selector,
          isolated_proxy_shutdown_grace_ms(),
        )
      {
        Ok(Nil) -> Nil
        Error(Nil) -> process.demonitor_process(proxy_mon)
      }
      drain_isolated_result(result_subject)
      // Defensive emit: if the proxy was hung past its inner timeout
      // and we killed it before the inner `call` could emit its own
      // `CallTimedOut`, observers would otherwise miss the event
      // entirely. Emit unconditionally at the outer boundary so the
      // contract "Error(Timeout) returned -> CallTimedOut emitted at
      // least once" holds. In the common case the inner already
      // emitted, producing a single duplicate event. A small price
      // for completeness.
      telemetry.emit(telemetry.CallTimedOut(clamped_timeout_ms))
      Error(Timeout)
    }
  }
}

// Internal sum type for the unified `call_isolated` selector.
type IsolatedOutcome(resp) {
  IsolatedReady(Result(resp, CallError))
  IsolatedProxyDown
}

/// Consume a late result tuple raced past our caller-side timeout.
/// The proxy only ever sends one message, so a 0-timeout pull suffices.
fn drain_isolated_result(
  result_subject: process.Subject(Result(resp, CallError)),
) -> Nil {
  let drain =
    process.new_selector()
    |> process.select_map(result_subject, fn(_) { Nil })
  let _ = process.selector_receive(drain, 0)
  Nil
}

/// Send a response through a reply subject. Used by the handler to answer a `call`.
///
/// Returns `Error(PayloadTooLarge(size))` when the encoded payload exceeds
/// `config.get().max_payload_size_bytes`. The message is never sent.
pub fn reply(
  reply_to: process.Subject(BitArray),
  response: resp,
  encoder: codec.Encoder(resp),
) -> Result(Nil, SendError) {
  case codec.encode(encoder, response) {
    Ok(bits) -> {
      let cap = config.get().max_payload_size_bytes
      let size = bit_array.byte_size(bits)
      case size > cap {
        True -> {
          telemetry.emit(telemetry.PayloadRejected(
            size,
            cap,
            telemetry.PayloadOnReply,
          ))
          Error(PayloadTooLarge(size))
        }
        False -> {
          process.send(reply_to, bits)
          Ok(Nil)
        }
      }
    }
    Error(e) -> Error(SendEncodeFailed(e))
  }
}

pub fn call_error_to_string(error: CallError) -> String {
  case error {
    Timeout -> "Call timed out"
    TargetDown -> "Target process is down"
    CallEncodeFailed(e) ->
      "Call encode failed: " <> codec.encode_error_to_string(e)
    CallDecodeFailed(e) ->
      "Call decode failed: " <> codec.decode_error_to_string(e)
    CallPayloadTooLarge(size) ->
      "Call payload too large: " <> int.to_string(size) <> " bytes"
  }
}

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/// Consume every pending message tagged with `reply_subject` from the
/// caller's mailbox. Called after Timeout or TargetDied so a target that
/// replies once or many times after the deadline (buggy / compromised /
/// retrying) cannot accumulate orphan messages in the mailbox.
///
/// Bounded by both message count and wall-clock budget so the timeout path
/// cannot be stretched indefinitely by a live flood of late replies.
@internal
pub fn drain_reply(reply_subject: process.Subject(BitArray)) -> Nil {
  let deadline = monotonic_ms() + call_timeout_drain_max_ms
  let drain_selector =
    process.new_selector()
    |> process.select_map(reply_subject, fn(_) { Nil })
  drain_reply_bounded(drain_selector, call_timeout_drain_max_messages, deadline)
}

fn drain_reply_bounded(
  drain_selector: process.Selector(Nil),
  remaining: Int,
  deadline: Int,
) -> Nil {
  case remaining <= 0 || monotonic_ms() >= deadline {
    True -> Nil
    False ->
      case process.selector_receive(drain_selector, 0) {
        Ok(Nil) -> drain_reply_bounded(drain_selector, remaining - 1, deadline)
        Error(Nil) -> Nil
      }
  }
}
