/// Type-safe global subjects for cross-node communication.
///
/// This module provides a type-safe wrapper around subjects that are compatible
/// with global registry and process groups. It enforces the use of codecs for
/// all message sending and receiving, ensuring type safety across the cluster.
import distribute/codec
import gleam/dynamic
import gleam/erlang/process

/// A subject that can be registered globally and used with process groups.
/// This type enforces codec usage for all operations, providing compile-time
/// type safety for distributed messaging.
pub opaque type GlobalSubject(msg) {
  GlobalSubject(
    subject: process.Subject(BitArray),
    encoder: codec.Encoder(msg),
    decoder: codec.Decoder(msg),
  )
}

/// Create a new global subject for the current process with required codec.
/// The encoder and decoder ensure all messages are type-safe.
pub fn new(
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> GlobalSubject(msg) {
  let subject = process.unsafely_create_subject(process.self(), dynamic.nil())
  GlobalSubject(subject, encoder, decoder)
}

/// Create a global subject from a Pid with required codec.
/// Useful when you have a Pid from registry lookup.
pub fn from_pid(
  pid: process.Pid,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> GlobalSubject(msg) {
  let subject = process.unsafely_create_subject(pid, dynamic.nil())
  GlobalSubject(subject, encoder, decoder)
}

/// Create a global subject from an existing Subject(BitArray) with required codec.
/// Useful when you already have a Subject (e.g., from `start_global_receiver`).
pub fn from_subject(
  subject: process.Subject(BitArray),
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> GlobalSubject(msg) {
  GlobalSubject(subject, encoder, decoder)
}

/// Get the underlying Subject(BitArray) for low-level operations.
/// Prefer using `send` and `receive` methods for type-safe messaging.
pub fn subject(global: GlobalSubject(msg)) -> process.Subject(BitArray) {
  global.subject
}

/// Get the owner Pid of this global subject.
pub fn owner(global: GlobalSubject(msg)) -> Result(process.Pid, Nil) {
  process.subject_owner(global.subject)
}

/// Get the encoder for this global subject.
pub fn encoder(global: GlobalSubject(msg)) -> codec.Encoder(msg) {
  global.encoder
}

/// Get the decoder for this global subject.
pub fn decoder(global: GlobalSubject(msg)) -> codec.Decoder(msg) {
  global.decoder
}

/// Send a type-safe message through this global subject.
/// The message is automatically encoded using the subject's encoder.
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

/// Receive a type-safe message from this global subject.
/// The message is automatically decoded using the subject's decoder.
pub fn receive(
  global: GlobalSubject(msg),
  timeout_ms: Int,
) -> Result(msg, codec.DecodeError) {
  case process.receive(global.subject, timeout_ms) {
    Ok(binary) -> codec.decode(global.decoder, binary)
    Error(Nil) -> Error(codec.DecodeTimeout)
  }
}
