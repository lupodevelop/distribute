/// Helper functions for common distributed patterns.
///
/// This module provides convenient sugar functions for frequent operations
/// like lookup-and-send, retries, and batch operations.
import distribute/codec.{type Decoder, type Encoder}
import distribute/global
import distribute/messaging.{type SendError}
import distribute/receiver.{type ReceiveError}
import distribute/registry.{register_typed, unregister}
import gleam/erlang/process
import gleam/result
import gleam/string

/// Look up a global name and send a typed message in one operation.
/// This is more efficient than separate lookup + send.
pub fn lookup_and_send(
  name: String,
  msg: a,
  encoder: Encoder(a),
) -> Result(Nil, SendError) {
  messaging.send_global_typed(name, msg, encoder)
}

/// Send a typed message with automatic retry on failure.
pub fn send_with_retry(
  subject: process.Subject(BitArray),
  msg: a,
  encoder: Encoder(a),
  max_attempts: Int,
) -> Result(Nil, SendError) {
  do_send_with_retry(subject, msg, encoder, max_attempts, 1)
}

fn do_send_with_retry(
  subject: process.Subject(BitArray),
  msg: a,
  encoder: Encoder(a),
  max_attempts: Int,
  attempt: Int,
) -> Result(Nil, SendError) {
  case messaging.send_typed(subject, msg, encoder) {
    Ok(_) -> Ok(Nil)
    Error(_err) if attempt < max_attempts -> {
      process.sleep(100 * attempt)
      do_send_with_retry(subject, msg, encoder, max_attempts, attempt + 1)
    }
    Error(err) -> Error(err)
  }
}

/// Receive a typed message with timeout and automatic retry.
pub fn receive_with_retry(
  subject: process.Subject(BitArray),
  decoder: Decoder(a),
  timeout_ms: Int,
  max_attempts: Int,
) -> Result(a, ReceiveError) {
  do_receive_with_retry(subject, decoder, timeout_ms, max_attempts, 1)
}

fn do_receive_with_retry(
  subject: process.Subject(BitArray),
  decoder: Decoder(a),
  timeout_ms: Int,
  max_attempts: Int,
  attempt: Int,
) -> Result(a, ReceiveError) {
  case receiver.receive_typed(subject, decoder, timeout_ms) {
    Ok(msg) -> Ok(msg)
    Error(_err) if attempt < max_attempts ->
      do_receive_with_retry(
        subject,
        decoder,
        timeout_ms,
        max_attempts,
        attempt + 1,
      )
    Error(err) -> Error(err)
  }
}

/// Create a request-response pattern: send and wait for reply.
pub fn request(
  target: String,
  request: req,
  request_encoder: Encoder(req),
  response_decoder: Decoder(resp),
  timeout_ms: Int,
) -> Result(resp, RequestError) {
  // Create temporary subject for response
  let reply_subject =
    global.new(
      fn(_) { Ok(<<>>) },
      // Dummy encoder, won't be used
      response_decoder,
    )

  // Register temporarily
  let temp_name = "request_" <> string.inspect(process.self())
  use _ <- result.try(
    register_typed(temp_name, global.subject(reply_subject))
    |> result.map_error(fn(_) { RegistrationFailed }),
  )

  // Send request
  use _ <- result.try(
    messaging.send_global_typed(target, request, request_encoder)
    |> result.map_error(SendFailed),
  )

  // Wait for response
  use response <- result.try(
    global.receive(reply_subject, timeout_ms)
    |> result.map_error(fn(_) {
      ReceiveFailed(receiver.DecodeError(codec.DecodeFailed("timeout")))
    }),
  )

  // Cleanup
  let _ = unregister(temp_name)

  Ok(response)
}

pub type RequestError {
  RegistrationFailed
  SendFailed(SendError)
  ReceiveFailed(ReceiveError)
}
