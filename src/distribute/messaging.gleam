/// Cross-node messaging utilities.
///
/// This module provides functions to send messages to processes (local or remote)
/// and to globally registered names.
///
/// It handles the complexity of looking up global names and routing messages
/// across the cluster.
import distribute/codec
import distribute/log
import distribute/typed_process
import gleam/list
import gleam/string

pub type SendError {
  /// The globally registered name was not found.
  NameNotFound(String)
  /// The target process is not alive.
  ProcessNotAlive
  /// Network connectivity issue.
  NetworkError(String)
  /// Message is too large or invalid.
  InvalidMessage(String)
  /// Message encoding failed.
  EncodeFailed(codec.EncodeError)
  /// Sending the message failed for another reason.
  SendFailed(String)
}

type Dynamic

pub type Pid =
  typed_process.Pid

@external(erlang, "erlang", "send")
fn send_ffi(pid: Pid, msg: a) -> a

@external(erlang, "messaging_ffi", "send_global")
fn send_global_ffi(name: String, msg: a) -> Dynamic

@external(erlang, "messaging_ffi", "send_binary_global")
fn send_binary_global_ffi(name: String, binary_msg: BitArray) -> Dynamic

@external(erlang, "messaging_ffi", "is_ok_atom")
fn is_ok_atom(value: Dynamic) -> Bool

@external(erlang, "messaging_ffi", "is_not_found")
fn is_not_found(value: Dynamic) -> Bool

@external(erlang, "messaging_ffi", "get_error_reason")
fn get_error_reason(value: Dynamic) -> String

/// Classify error reason into structured SendError
pub fn classify_send_error(reason: String, name: String) -> SendError {
  case reason {
    "not_found" -> NameNotFound(name)
    "process_not_alive" -> ProcessNotAlive
    "invalid_message" -> InvalidMessage(name)
    _ ->
      case is_network_error(reason) {
        True -> NetworkError(name)
        False -> SendFailed(reason)
      }
  }
}

/// Convert encode error to send error
pub fn encode_error_to_send_error(error: codec.EncodeError) -> SendError {
  EncodeFailed(error)
}

/// Convert SendError to string for logging
pub fn classify_send_error_to_string(error: SendError) -> String {
  case error {
    NameNotFound(name) -> "name_not_found: " <> name
    ProcessNotAlive -> "process_not_alive"
    NetworkError(name) -> "network_error: " <> name
    InvalidMessage(name) -> "invalid_message: " <> name
    EncodeFailed(_) -> "encode_failed"
    SendFailed(reason) -> "send_failed: " <> reason
  }
}

/// Check if error is network-related
pub fn is_network_error(reason: String) -> Bool {
  string.contains(reason, "network")
  || string.contains(reason, "connection")
  || string.contains(reason, "partition")
}

/// Send a message to a process (local or remote).
pub fn send(pid: Pid, msg: a) -> Nil {
  send_ffi(pid, msg)
  Nil
}

// ============================================================================
// Typed messaging API
// ============================================================================

/// Send a typed message to a subject. The message is encoded using the
/// provided encoder and sent as binary data to ensure type safety.
pub fn send_typed(
  subject: typed_process.Subject(a),
  msg: a,
  encoder: codec.Encoder(a),
) -> Result(Nil, SendError) {
  case codec.encode(encoder, msg) {
    Ok(binary_msg) -> {
      send_ffi(typed_process.to_pid(subject), binary_msg)
      Ok(Nil)
    }
    Error(encode_error) -> Error(EncodeFailed(encode_error))
  }
}

/// Send a message to a globally registered name.
/// Returns Ok(Nil) if successful, Error if name not found or send failed.
pub fn send_global(name: String, msg: a) -> Result(Nil, SendError) {
  log.debug("Sending message to global name", [#("name", name)])
  let res = send_global_ffi(name, msg)
  case is_ok_atom(res) {
    True -> {
      log.debug("Message sent successfully", [#("name", name)])
      Ok(Nil)
    }
    False ->
      case is_not_found(res) {
        True -> {
          let error = NameNotFound(name)
          log.warn("Global name not found", [
            #("name", name),
            #("error", classify_send_error_to_string(error)),
          ])
          Error(error)
        }
        False -> {
          let error = classify_send_error(get_error_reason(res), name)
          log.error("Failed to send message", [
            #("name", name),
            #("error", classify_send_error_to_string(error)),
          ])
          Error(error)
        }
      }
  }
}

/// Batch send result with error aggregation
pub type BatchSendResult {
  BatchSendResult(
    total: Int,
    successful: Int,
    failed: Int,
    errors: List(SendError),
  )
}

/// Send multiple messages in batch with error aggregation
/// Returns detailed results including all errors encountered
pub fn send_batch(messages: List(#(String, a))) -> BatchSendResult {
  log.debug("Sending batch of messages", [
    #("count", string.inspect(list.length(messages))),
  ])

  let results =
    list.map(messages, fn(msg_pair) {
      let #(name, msg) = msg_pair
      send_global(name, msg)
    })

  let successes =
    list.filter(results, fn(r) {
      case r {
        Ok(_) -> True
        Error(_) -> False
      }
    })
    |> list.length

  let errors =
    list.fold(results, [], fn(acc, r) {
      case r {
        Error(e) -> [e, ..acc]
        Ok(_) -> acc
      }
    })

  let total = list.length(messages)
  log.debug("Batch send completed", [
    #("total", string.inspect(total)),
    #("successful", string.inspect(successes)),
    #("failed", string.inspect(list.length(errors))),
  ])

  BatchSendResult(
    total: total,
    successful: successes,
    failed: list.length(errors),
    errors: errors,
  )
}

/// Send batch and return Ok only if all succeeded, Error with first failure otherwise
pub fn send_batch_strict(messages: List(#(String, a))) -> Result(Nil, SendError) {
  let result = send_batch(messages)
  case result.failed {
    0 -> Ok(Nil)
    _ ->
      case list.first(result.errors) {
        Ok(e) -> Error(e)
        Error(_) -> Error(SendFailed("Unknown batch error"))
      }
  }
}

/// Send a typed message to a globally registered name. The message is
/// encoded using the provided encoder before sending.
pub fn send_global_typed(
  name: String,
  msg: a,
  encoder: codec.Encoder(a),
) -> Result(Nil, SendError) {
  log.debug("Sending typed message to global name", [#("name", name)])
  case codec.encode(encoder, msg) {
    Ok(binary_msg) -> {
      let res = send_binary_global_ffi(name, binary_msg)
      case is_ok_atom(res) {
        True -> {
          log.debug("Typed message sent successfully", [#("name", name)])
          Ok(Nil)
        }
        False ->
          case is_not_found(res) {
            True -> {
              let error = NameNotFound(name)
              log.warn("Global name not found", [
                #("name", name),
                #("error", classify_send_error_to_string(error)),
              ])
              Error(error)
            }
            False -> {
              let error = classify_send_error(get_error_reason(res), name)
              log.error("Failed to send typed message", [
                #("name", name),
                #("error", classify_send_error_to_string(error)),
              ])
              Error(error)
            }
          }
      }
    }
    Error(encode_error) -> Error(EncodeFailed(encode_error))
  }
}

/// Send multiple typed messages in batch with error aggregation
pub fn send_batch_typed(
  messages: List(#(String, a)),
  encoder: codec.Encoder(a),
) -> BatchSendResult {
  log.debug("Sending batch of typed messages", [
    #("count", string.inspect(list.length(messages))),
  ])

  let results =
    list.map(messages, fn(msg_pair) {
      let #(name, msg) = msg_pair
      send_global_typed(name, msg, encoder)
    })

  let successes =
    list.filter(results, fn(r) {
      case r {
        Ok(_) -> True
        Error(_) -> False
      }
    })
    |> list.length

  let errors =
    list.fold(results, [], fn(acc, r) {
      case r {
        Error(e) -> [e, ..acc]
        Ok(_) -> acc
      }
    })

  let total = list.length(messages)
  log.debug("Batch typed send completed", [
    #("total", string.inspect(total)),
    #("successful", string.inspect(successes)),
    #("failed", string.inspect(list.length(errors))),
  ])

  BatchSendResult(
    total: total,
    successful: successes,
    failed: list.length(errors),
    errors: errors,
  )
}
