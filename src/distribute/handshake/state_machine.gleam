pub type HandshakeState {
  Idle
  InitiatorHelloSent
  AwaitingCapabilities
  Negotiated(protocol: String, version: Int)
  Failed(reason: String)
}

pub type Event {
  Start
  ReceivedHello
  ReceivedCapabilities
  ReceivedAccept(protocol: String, version: Int)
  ReceivedReject(reason: String)
  Timeout
}

pub fn handle(state: HandshakeState, event: Event) -> HandshakeState {
  case state {
    Idle ->
      case event {
        Start -> InitiatorHelloSent
        _ -> Idle
      }
    InitiatorHelloSent ->
      case event {
        ReceivedCapabilities -> AwaitingCapabilities
        Timeout -> Failed("timeout")
        _ -> InitiatorHelloSent
      }
    AwaitingCapabilities ->
      case event {
        ReceivedAccept(p, v) -> Negotiated(p, v)
        ReceivedReject(r) -> Failed(r)
        Timeout -> Failed("timeout")
        _ -> AwaitingCapabilities
      }
    Negotiated(_, _) -> state
    Failed(_) -> state
  }
}
