import distribute/handshake.{Capability, Hello}
import distribute/handshake/actor.{start_initiator_handshake}
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() -> Nil {
  gleeunit.main()
}

pub type TestMessage {
  Failed(String)
  Unexpected(String)
}

fn wait_for_failed(subject: process.Subject(TestMessage), attempts: Int) -> Bool {
  case attempts {
    0 -> False
    _ ->
      case process.receive(subject, 500) {
        Ok(Failed(_)) -> True
        Ok(Unexpected(_)) -> wait_for_failed(subject, attempts - 1)
        Error(_) -> wait_for_failed(subject, attempts - 1)
      }
  }
}

pub fn initiator_actor_timeout_triggers_failure_test() {
  let local_hello =
    Hello(node_id: "node-a", node_info: [#("k", "v")], capabilities: [
      Capability("proto", 1, 1, []),
    ])

  let test_subject: process.Subject(TestMessage) = process.new_subject()

  let on_success = fn(_node, _meta) {
    process.send(test_subject, Unexpected("success"))
  }
  let on_failure = fn(_node, reason) {
    process.send(test_subject, Failed(reason))
  }
  let send_fn = fn(_bytes) { Nil }

  case
    start_initiator_handshake(local_hello, 50, send_fn, on_success, on_failure)
  {
    Ok(_subject) -> {
      let found = wait_for_failed(test_subject, 40)
      should.be_true(found)
    }
    Error(_) -> should.fail()
  }
}
