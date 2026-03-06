/// Typed wrapper around Subject(BitArray) for cross-node messaging.
import distribute/codec
import distribute/internal/telemetry
import gleam/bit_array
import gleam/dynamic
import gleam/erlang/process

// ---------------------------------------------------------------------------
// Subject construction (single point of coupling)
// ---------------------------------------------------------------------------

/// Build a Subject from a PID and a tag via FFI.
@external(erlang, "distribute_ffi_utils", "create_subject")
fn create_subject(
  owner: process.Pid,
  tag: dynamic.Dynamic,
) -> process.Subject(msg)

// ---------------------------------------------------------------------------
// Unique tag generation
// ---------------------------------------------------------------------------

@external(erlang, "erlang", "make_ref")
fn make_ref() -> dynamic.Dynamic

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
  let subject = create_subject(process.self(), make_ref())
  GlobalSubject(subject:, encoder:, decoder:)
}

/// Send-only subject from a remote PID. Uses a nil tag.
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

/// Subject from a name and PID. The name becomes the tag,
/// allowing any node with the same name to reconstruct it.
pub fn from_name(
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
// Messaging
// ---------------------------------------------------------------------------

/// Encode and send a message.
pub fn send(
  global: GlobalSubject(msg),
  message: msg,
) -> Result(Nil, codec.EncodeError) {
  let start = telemetry.system_time()
  case codec.encode(global.encoder, message) {
    Ok(binary) -> {
      let duration = telemetry.system_time() - start
      let size = bit_array.byte_size(binary)
      telemetry.emit_codec_encode_stop(duration, size, True)

      let start_send = telemetry.system_time()
      process.send(global.subject, binary)
      let duration_send = telemetry.system_time() - start_send

      // We don't always have a name for the subject (could be Pid-based)
      // But we can try to find the target info if possible.
      telemetry.emit_message_send_stop(duration_send, size, "unknown", True)
      Ok(Nil)
    }
    Error(err) -> {
      let duration = telemetry.system_time() - start
      telemetry.emit_codec_encode_stop(duration, 0, False)
      Error(err)
    }
  }
}

/// Receive and decode a message. Only works on subjects you own.
pub fn receive(
  global: GlobalSubject(msg),
  timeout_ms: Int,
) -> Result(msg, codec.DecodeError) {
  case process.receive(global.subject, timeout_ms) {
    Ok(binary) -> {
      let start_decode = telemetry.system_time()
      let res = codec.decode(global.decoder, binary)
      let duration_decode = telemetry.system_time() - start_decode
      telemetry.emit_codec_decode_stop(duration_decode, case res {
        Ok(_) -> True
        _ -> False
      })
      res
    }
    Error(Nil) -> Error(codec.DecodeTimeout)
  }
}

// ---------------------------------------------------------------------------
// Request/response (call pattern)
// ---------------------------------------------------------------------------

pub type CallError {
  CallTimeout
  CallEncodeFailed(codec.EncodeError)
  CallDecodeFailed(codec.DecodeError)
}

/// Synchronous request/response. Creates a temporary subject,
/// sends the request, waits for the reply.
pub fn call(
  target: GlobalSubject(req),
  make_request: fn(process.Subject(BitArray)) -> req,
  response_decoder: codec.Decoder(resp),
  timeout_ms: Int,
) -> Result(resp, CallError) {
  let reply_subject = process.new_subject()
  let request = make_request(reply_subject)
  case send(target, request) {
    Error(e) -> Error(CallEncodeFailed(e))
    Ok(Nil) -> {
      case process.receive(reply_subject, timeout_ms) {
        Error(Nil) -> Error(CallTimeout)
        Ok(bits) -> {
          case codec.decode(response_decoder, bits) {
            Ok(value) -> Ok(value)
            Error(e) -> Error(CallDecodeFailed(e))
          }
        }
      }
    }
  }
}

/// Send a response through a reply subject.
pub fn reply(
  reply_to: process.Subject(BitArray),
  response: resp,
  encoder: codec.Encoder(resp),
) -> Result(Nil, codec.EncodeError) {
  case codec.encode(encoder, response) {
    Ok(bits) -> {
      process.send(reply_to, bits)
      Ok(Nil)
    }
    Error(e) -> Error(e)
  }
}

pub fn call_error_to_string(error: CallError) -> String {
  case error {
    CallTimeout -> "Call timed out"
    CallEncodeFailed(e) ->
      "Call encode failed: " <> codec.encode_error_to_string(e)
    CallDecodeFailed(e) ->
      "Call decode failed: " <> codec.decode_error_to_string(e)
  }
}
