import distribute/handshake.{Capability, Hello}
import distribute/handshake_state.{
  Failed, Received, Sent, handshake_on_timeout, initiator_start,
}
import gleeunit
import gleeunit/should

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn initiator_retries_and_timeout_test() {
  let local_hello =
    Hello(node_id: "node-a", node_info: [#("k", "v")], capabilities: [
      Capability("proto", 1, 1, []),
    ])

  let #(_hello_b, istate) = initiator_start(local_hello)

  case handshake_on_timeout(istate) {
    Ok(Sent(_b1, state1)) ->
      case handshake_on_timeout(state1) {
        Ok(Sent(_b2, state2)) ->
          case handshake_on_timeout(state2) {
            Ok(Sent(_b3, state3)) ->
              case handshake_on_timeout(state3) {
                Ok(Received(_, Failed(msg))) ->
                  should.equal(msg, "capabilities timeout")
                _ -> should.equal(True, False)
              }
            _ -> should.equal(True, False)
          }
        _ -> should.equal(True, False)
      }
    _ -> should.equal(True, False)
  }
}
