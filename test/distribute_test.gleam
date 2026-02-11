import distribute
import distribute/codec
import distribute/receiver
import distribute/registry
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn version_test() {
  should.equal(distribute.version(), "3.0.0")
}

pub fn facade_new_subject_send_receive_test() {
  // new_subject uses unique tags — no message mixing
  let subj =
    distribute.new_subject(codec.string_encoder(), codec.string_decoder())
  let assert Ok(Nil) = distribute.send(subj, "facade-msg")
  let assert Ok(msg) = distribute.receive(subj, 1000)
  should.equal(msg, "facade-msg")
}

pub fn facade_multiple_subjects_isolated_test() {
  // Two new_subject calls on same process should NOT interfere
  let s1 = distribute.new_subject(codec.int_encoder(), codec.int_decoder())
  let s2 = distribute.new_subject(codec.int_encoder(), codec.int_decoder())

  let assert Ok(Nil) = distribute.send(s1, 111)
  let assert Ok(Nil) = distribute.send(s2, 222)

  // Each receives its own message, not the other's
  let assert Ok(msg1) = distribute.receive(s1, 1000)
  let assert Ok(msg2) = distribute.receive(s2, 1000)
  should.equal(msg1, 111)
  should.equal(msg2, 222)
}

pub fn facade_self_node_test() {
  let name = distribute.self_node()
  should.be_true(name != "")
}

pub fn facade_nodes_test() {
  let connected = distribute.nodes()
  should.equal(connected, [])
}

pub fn facade_start_actor_test() {
  let tn =
    registry.typed_name(
      "facade_actor_" <> unique_id(),
      codec.string_encoder(),
      codec.string_decoder(),
    )

  // start_actor creates a named OTP actor, returns Result
  let assert Ok(gs) =
    distribute.start_actor(tn, Nil, fn(_msg, state) { receiver.Continue(state) })

  // Can send to the actor
  let assert Ok(Nil) = distribute.send(gs, "hello-actor")
  should.be_true(True)
}

pub fn facade_register_lookup_test() {
  let tn =
    registry.typed_name(
      "facade_test_" <> unique_id(),
      codec.int_encoder(),
      codec.int_decoder(),
    )

  // Use a named actor for registry (deterministic tag)
  let assert Ok(gs) =
    distribute.start_actor(tn, 0, fn(_msg, state) { receiver.Continue(state) })

  let assert Ok(Nil) = distribute.register(tn, gs)

  // Lookup returns a remote handle with matching deterministic tag
  let assert Ok(found) = distribute.lookup(tn)

  // Send through the found handle — tag matches the actor!
  let assert Ok(Nil) = distribute.send(found, 42)

  // Cleanup
  let assert Ok(Nil) = distribute.unregister(registry.typed_name_to_string(tn))
}

// Simple unique ID generator
@external(erlang, "erlang", "unique_integer")
fn erlang_unique_int() -> Int

fn unique_id() -> String {
  let n = erlang_unique_int()
  int_to_string(case n < 0 {
    True -> 0 - n
    False -> n
  })
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String
