import distribute/handshake
import distribute/handshake/state as hs
import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/result

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

fn handle_network_message(
  current: hs.HandshakeState,
  bytes: BitArray,
  send_fn: fn(BitArray) -> Nil,
  subject: process.Subject(HandshakeMessage),
  timeout_ms: Int,
  local: handshake.Hello,
  on_success: fn(String, handshake.MemberMetadata) -> Nil,
  on_failure: fn(String, String) -> Nil,
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
      on_success(node, handshake.MemberMetadata([], Some(ctx)))
      actor.stop()
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
      on_success(node, handshake.MemberMetadata([], Some(ctx)))
      actor.stop()
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
