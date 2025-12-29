import distribute/receiver
import distribute/registry
import gleam/dynamic
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub type Msg {
  Ping
}

pub fn global_registry_test() {
  let decoder = fn(_x) { Ok(Ping) }

  // Start a global receiver
  let subject =
    receiver.start_global_receiver(Nil, decoder, fn(msg, state) {
      case msg {
        Ping -> receiver.Continue(state)
      }
    })

  // Register it
  let name = "global_test_service"
  registry.register_typed(name, subject) |> should.be_ok

  // Look it up using whereis_with_tag (preferred over deprecated whereis_typed)
  let lookup = registry.whereis_with_tag(name, dynamic.nil())
  lookup |> should.be_ok
  let assert Ok(found_subject) = lookup

  // Verify owners match
  process.subject_owner(found_subject)
  |> should.equal(process.subject_owner(subject))

  // Verify we can send to it (using process.send directly to test the subject structure)
  // Note: In a real app we would use messaging.send_typed, but here we want to verify
  // the subject works locally too.
  process.send(found_subject, <<>>)

  // Cleanup
  registry.unregister(name)
}
