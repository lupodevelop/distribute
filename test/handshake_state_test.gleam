import distribute/handshake.{Capability, Hello}
import distribute/handshake/state.{
  Established, Received, Sent, initiator_handle_message, initiator_start,
  responder_handle_message, responder_init,
}
import gleeunit
import gleeunit/should

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn initiator_responder_full_flow_test() {
  let local_hello =
    Hello(node_id: "node-a", node_info: [#("k", "v")], capabilities: [
      Capability("proto", 1, 1, []),
    ])

  let #(hello_b, istate) = initiator_start(local_hello)

  // Responder receives hello
  case responder_handle_message(responder_init(), hello_b) {
    Ok(Sent(caps_b, rstate)) -> {
      // Initiator receives capabilities
      case initiator_handle_message(istate, caps_b) {
        Ok(Sent(acc_ke_b, istate2)) -> {
          // Responder receives Accept+KE
          case responder_handle_message(rstate, acc_ke_b) {
            Ok(Sent(ke_b2, _rstate2)) -> {
              // Initiator receives responder's KE
              case initiator_handle_message(istate2, ke_b2) {
                Ok(Received(_, Established(#(_, _)))) ->
                  should.equal(True, True)
                _ -> should.equal(True, False)
              }
            }
            Ok(Received(_, Established(#(_, _)))) -> should.equal(True, True)
            _ -> should.equal(True, False)
          }
        }
        _ -> should.equal(True, False)
      }
    }
    _ -> should.equal(True, False)
  }
}
