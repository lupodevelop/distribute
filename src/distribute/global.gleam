/// Typed wrapper around `Subject(BitArray)` for cross-node messaging.
///
/// Pairs a subject with an encoder and decoder. Constructors:
/// `new`, `from_pid`, `from_name`, `from_subject`.
import distribute/codec
import gleam/dynamic
import gleam/erlang/process

// ---------------------------------------------------------------------------
// Subject construction (single point of coupling)
// ---------------------------------------------------------------------------

/// Build a Subject from a PID and a tag via our own FFI.
/// Single point of coupling with gleam_erlang's Subject layout.
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

/// A subject with a codec, usable across nodes.
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

/// Subject from a remote PID. Nil tag, send-only — you can't
/// receive on it from the calling process.
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

/// Subject from a name and PID. The name is the tag, so any node
/// that knows the name can reconstruct the same Subject.
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
  case codec.encode(global.encoder, message) {
    Ok(binary) -> {
      process.send(global.subject, binary)
      Ok(Nil)
    }
    Error(err) -> Error(err)
  }
}

/// Receive and decode a message. Only works on subjects you own.
pub fn receive(
  global: GlobalSubject(msg),
  timeout_ms: Int,
) -> Result(msg, codec.DecodeError) {
  case process.receive(global.subject, timeout_ms) {
    Ok(binary) -> codec.decode(global.decoder, binary)
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

/// Synchronous request/response. Creates a temporary subject, sends
/// the request (built by `make_request`), waits for the reply.
/// The handler must call `reply` with the same subject.
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

/// Send a response through a reply subject. Used by the handler
/// to answer a `call`.
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
