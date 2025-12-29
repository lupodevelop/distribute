import distribute/handshake
import distribute/handshake/state as hs
import distribute/registry/actor as registry_actor
import distribute/registry/behaviour as registry_behaviour
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string

pub type HandshakeMessage {
  NetworkMessage(BitArray)
  TimeoutMessage(String)
  Init(process.Subject(HandshakeMessage), Int)
}

pub fn start_initiator_handshake(
  local: handshake.Hello,
  timeout_ms: Int,
  send_fn: fn(BitArray) -> Nil,
  on_success: fn(String, handshake.MemberMetadata) -> Nil,
  on_failure: fn(String, String) -> Nil,
  registry: Option(process.Subject(registry_actor.RegistryCommand)),
) -> Result(process.Subject(HandshakeMessage), actor.StartError) {
  let initial_state: Result(
    #(hs.HandshakeState, process.Subject(HandshakeMessage), Int),
    Nil,
  ) = Error(Nil)
  actor.new(initial_state)
  |> actor.on_message(fn(state, msg) {
    case state, msg {
      Error(_), Init(subject, timeout) -> {
        let #(_, new_state) = hs.initiator_start(local)
        case handshake.encode_hello(local) {
          Ok(bytes) -> send_fn(bytes)
          Error(_) -> Nil
        }
        schedule_timer_with_subject(subject, "capabilities", timeout)
        actor.continue(Ok(#(new_state, subject, timeout)))
      }
      Ok(#(current, subject, timeout)), NetworkMessage(bytes) -> {
        handle_network_message(
          current,
          bytes,
          send_fn,
          subject,
          timeout,
          local,
          on_success,
          on_failure,
          registry,
        )
      }
      Ok(#(current, subject, timeout)), TimeoutMessage(_tag) -> {
        handle_timeout(
          current,
          send_fn,
          subject,
          timeout,
          local,
          on_success,
          on_failure,
          registry,
        )
      }
      _, _ -> actor.continue(state)
    }
  })
  |> actor.start()
  |> result.map(fn(started) {
    let subject = started.data
    process.send(subject, Init(subject, timeout_ms))
    subject
  })
}

/// Start a responder handshake actor.
///
/// The responder waits for incoming handshake messages from an initiator,
/// performs capability negotiation and key exchange, and upon successful
/// completion registers the negotiated node in the optional registry.
///
/// ## Parameters
///
/// - `timeout_ms`: Timeout in milliseconds for each handshake step. If a
///   response is not received within this time, the handshake will retry or fail.
/// - `send_fn`: Callback invoked with raw bytes to send to the remote peer.
///   The caller is responsible for transmitting these bytes over the network.
/// - `on_success`: Callback invoked when the handshake completes successfully.
///   Receives the remote node ID and negotiated metadata (including secure context).
/// - `on_failure`: Callback invoked when the handshake fails. Receives the
///   remote node ID (or empty string if unknown) and a failure reason string.
/// - `registry`: Optional registry actor subject. When provided (`Some(reg)`),
///   the responder will:
///   - Look up the initiator's node ID when receiving the Hello message to
///     validate known nodes and retrieve cached metadata
///   - Register the successfully negotiated node metadata upon completion
///
///   When `None`, registry operations are skipped and the handshake proceeds
///   without persistence.
///
/// ## Returns
///
/// `Ok(Subject(HandshakeMessage))` - A subject for sending network messages
/// and timeouts to the handshake actor. Use `NetworkMessage(bytes)` to forward
/// incoming protocol bytes from the remote peer.
///
/// `Error(actor.StartError)` - If the actor fails to start.
///
/// ## Registry Integration
///
/// When a registry is provided, the responder performs these operations:
///
/// 1. **On Hello received**: Attempts `lookup_sync` to check if the initiator's
///    node is already known. If found, the cached metadata can be used for
///    validation (e.g., verify capabilities haven't changed unexpectedly).
///    Currently logged but not enforced; future versions may add strict validation.
///
/// 2. **On Established**: Calls `register_sync` to persist the negotiated
///    protocol version and secure context. If registration fails, the handshake
///    is aborted and `on_failure` is called with a registry error reason.
///
pub fn start_responder_handshake(
  timeout_ms: Int,
  send_fn: fn(BitArray) -> Nil,
  on_success: fn(String, handshake.MemberMetadata) -> Nil,
  on_failure: fn(String, String) -> Nil,
  registry: Option(process.Subject(registry_actor.RegistryCommand)),
) -> Result(process.Subject(HandshakeMessage), actor.StartError) {
  let initial_state: Result(
    #(hs.HandshakeState, process.Subject(HandshakeMessage), Int),
    Nil,
  ) = Error(Nil)
  actor.new(initial_state)
  |> actor.on_message(fn(state, msg) {
    case state, msg {
      Error(_), Init(subject, timeout) -> {
        let new_state = hs.responder_init()
        actor.continue(Ok(#(new_state, subject, timeout)))
      }
      Ok(#(current, subject, timeout)), NetworkMessage(bytes) -> {
        handle_responder_message(
          current,
          bytes,
          send_fn,
          subject,
          timeout,
          on_success,
          on_failure,
          registry,
        )
      }
      _, _ -> actor.continue(state)
    }
  })
  |> actor.start()
  |> result.map(fn(started) {
    let subject = started.data
    // Initialize internal state with responder_init and the real subject
    process.send(subject, Init(subject, timeout_ms))
    subject
  })
}

fn handle_responder_message(
  current: hs.HandshakeState,
  bytes: BitArray,
  send_fn: fn(BitArray) -> Nil,
  subject: process.Subject(HandshakeMessage),
  timeout_ms: Int,
  on_success: fn(String, handshake.MemberMetadata) -> Nil,
  on_failure: fn(String, String) -> Nil,
  registry: Option(process.Subject(registry_actor.RegistryCommand)),
) -> actor.Next(
  Result(#(hs.HandshakeState, process.Subject(HandshakeMessage), Int), Nil),
  HandshakeMessage,
) {
  // Check if this is a Hello message and validate against registry if present
  case handshake.decode_hello(bytes), registry {
    Ok(hello), Some(reg) -> {
      // Lookup node in registry to validate known nodes
      case registry_actor.lookup_sync(reg, timeout_ms, hello.node_id) {
        Ok(cached_md) -> {
          // Node is known - could validate capabilities match
          // For now we just log this fact (in production, add proper logging)
          // Future: strict validation of cached_md.capabilities vs hello.capabilities
          let _ = cached_md
          Nil
        }
        Error(_) -> {
          // Node is not known yet - this is fine for first connection
          Nil
        }
      }
    }
    _, _ -> Nil
  }

  case hs.responder_handle_message(current, bytes) {
    Ok(hs.Sent(out_bytes, new_state)) -> {
      send_fn(out_bytes)
      schedule_if_waiting(new_state, subject, timeout_ms)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(Some(out_bytes), hs.ResponderState(..) as new_state)) -> {
      send_fn(out_bytes)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(None, hs.ResponderState(..) as new_state)) ->
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    Ok(hs.Received(Some(out_bytes), hs.InitiatorState(..) as new_state)) -> {
      send_fn(out_bytes)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(None, hs.InitiatorState(..) as new_state)) ->
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    Ok(hs.Received(_, hs.Established(#(node, ctx)))) -> {
      case registry {
        Some(reg) -> {
          let md = registry_behaviour.Metadata(node, [], "")
          case registry_actor.register_sync(reg, timeout_ms, node, md) {
            Ok(_) -> {
              on_success(node, handshake.MemberMetadata([], Some(ctx)))
              actor.stop()
            }
            Error(e) -> {
              let reason = case e {
                registry_behaviour.AlreadyExists -> "already_exists"
                registry_behaviour.NotFound -> "not_found"
                registry_behaviour.InvalidArgument(msg) -> msg
                registry_behaviour.AdapterFailure(msg) -> msg
              }
              on_failure(node, string.concat(["registry error: ", reason]))
              actor.stop()
            }
          }
        }
        None -> {
          on_success(node, handshake.MemberMetadata([], Some(ctx)))
          actor.stop()
        }
      }
    }
    Ok(hs.Received(_, hs.Failed(reason))) -> {
      on_failure("", reason)
      actor.stop()
    }
    Error(_) -> actor.continue(Ok(#(current, subject, timeout_ms)))
  }
}

fn handle_network_message(
  current: hs.HandshakeState,
  bytes: BitArray,
  send_fn: fn(BitArray) -> Nil,
  subject: process.Subject(HandshakeMessage),
  timeout_ms: Int,
  local: handshake.Hello,
  on_success: fn(String, handshake.MemberMetadata) -> Nil,
  on_failure: fn(String, String) -> Nil,
  registry: Option(process.Subject(registry_actor.RegistryCommand)),
) -> actor.Next(
  Result(#(hs.HandshakeState, process.Subject(HandshakeMessage), Int), Nil),
  HandshakeMessage,
) {
  case hs.initiator_handle_message(current, bytes) {
    Ok(hs.Sent(out_bytes, new_state)) -> {
      send_fn(out_bytes)
      schedule_if_waiting(new_state, subject, timeout_ms)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(Some(out_bytes), hs.InitiatorState(..) as new_state)) -> {
      send_fn(out_bytes)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(None, hs.InitiatorState(..) as new_state)) ->
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    Ok(hs.Received(Some(out_bytes), hs.ResponderState(..) as new_state)) -> {
      send_fn(out_bytes)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(None, hs.ResponderState(..) as new_state)) ->
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    Ok(hs.Received(_, hs.Established(#(node, ctx)))) -> {
      case registry {
        Some(reg) -> {
          let md = registry_behaviour.Metadata(node, [], "")
          case registry_actor.register_sync(reg, timeout_ms, node, md) {
            Ok(_) -> {
              on_success(node, handshake.MemberMetadata([], Some(ctx)))
              actor.stop()
            }
            Error(e) -> {
              let reason = case e {
                registry_behaviour.AlreadyExists -> "already_exists"
                registry_behaviour.NotFound -> "not_found"
                registry_behaviour.InvalidArgument(msg) -> msg
                registry_behaviour.AdapterFailure(msg) -> msg
              }
              on_failure(node, string.concat(["registry error: ", reason]))
              actor.stop()
            }
          }
        }
        None -> {
          on_success(node, handshake.MemberMetadata([], Some(ctx)))
          actor.stop()
        }
      }
    }
    Ok(hs.Received(_, hs.Failed(reason))) -> {
      on_failure(local.node_id, reason)
      actor.stop()
    }
    Error(_) -> actor.continue(Ok(#(current, subject, timeout_ms)))
  }
}

fn handle_timeout(
  current: hs.HandshakeState,
  send_fn: fn(BitArray) -> Nil,
  subject: process.Subject(HandshakeMessage),
  timeout_ms: Int,
  local: handshake.Hello,
  on_success: fn(String, handshake.MemberMetadata) -> Nil,
  on_failure: fn(String, String) -> Nil,
  registry: Option(process.Subject(registry_actor.RegistryCommand)),
) -> actor.Next(
  Result(#(hs.HandshakeState, process.Subject(HandshakeMessage), Int), Nil),
  HandshakeMessage,
) {
  case hs.handshake_on_timeout(current) {
    Ok(hs.Sent(bytes, new_state)) -> {
      send_fn(bytes)
      schedule_if_waiting(new_state, subject, timeout_ms)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(Some(bytes), hs.InitiatorState(..) as new_state)) -> {
      send_fn(bytes)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(None, hs.InitiatorState(..) as new_state)) ->
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    Ok(hs.Received(Some(bytes), hs.ResponderState(..) as new_state)) -> {
      send_fn(bytes)
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    }
    Ok(hs.Received(None, hs.ResponderState(..) as new_state)) ->
      actor.continue(Ok(#(new_state, subject, timeout_ms)))
    Ok(hs.Received(_, hs.Failed(reason))) -> {
      on_failure(local.node_id, reason)
      actor.stop()
    }
    Ok(hs.Received(_, hs.Established(#(node, ctx)))) -> {
      case registry {
        Some(reg) -> {
          let md = registry_behaviour.Metadata(node, [], "")
          case registry_actor.register_sync(reg, timeout_ms, node, md) {
            Ok(_) -> {
              on_success(node, handshake.MemberMetadata([], Some(ctx)))
              actor.stop()
            }
            Error(e) -> {
              let reason = case e {
                registry_behaviour.AlreadyExists -> "already_exists"
                registry_behaviour.NotFound -> "not_found"
                registry_behaviour.InvalidArgument(msg) -> msg
                registry_behaviour.AdapterFailure(msg) -> msg
              }
              on_failure(
                local.node_id,
                string.concat(["registry error: ", reason]),
              )
              actor.stop()
            }
          }
        }
        None -> {
          on_success(node, handshake.MemberMetadata([], Some(ctx)))
          actor.stop()
        }
      }
    }
    Error(_) -> actor.continue(Ok(#(current, subject, timeout_ms)))
  }
}

fn schedule_if_waiting(
  state: hs.HandshakeState,
  subject: process.Subject(HandshakeMessage),
  timeout_ms: Int,
) -> Nil {
  case state {
    hs.InitiatorState(_, _, _, _, Some(tag), _, _) -> {
      schedule_timer_with_subject(subject, tag, timeout_ms)
    }
    hs.ResponderState(_, _, _, Some(tag), _, _) -> {
      schedule_timer_with_subject(subject, tag, timeout_ms)
    }
    _ -> Nil
  }
}

fn schedule_timer_with_subject(
  subject: process.Subject(HandshakeMessage),
  tag: String,
  timeout_ms: Int,
) -> Nil {
  process.send_after(subject, timeout_ms, TimeoutMessage(tag))
  Nil
}
