import distribute/actor
import distribute/codec
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn actor_start_test() {
  let tn =
    registry.typed_name(
      "actor_start_" <> unique_id(),
      codec.string_encoder(),
      codec.string_decoder(),
    )

  let assert Ok(gs) =
    actor.start(tn, Nil, fn(_msg, state) { receiver.Continue(state) })

  // Should send without error
  let assert Ok(Nil) = global.send(gs, "ping")
  should.be_true(True)
}

pub fn actor_start_registered_test() {
  let tn =
    registry.typed_name(
      "actor_reg_" <> unique_id(),
      codec.int_encoder(),
      codec.int_decoder(),
    )

  let assert Ok(_gs) =
    actor.start_registered(tn, 0, fn(_msg, state) { receiver.Continue(state) })

  // Should be findable
  should.be_true(registry.is_registered(registry.typed_name_to_string(tn)))

  // Cleanup
  let assert Ok(Nil) = registry.unregister(registry.typed_name_to_string(tn))
}

pub fn actor_child_spec_test() {
  let tn =
    registry.typed_name(
      "child_" <> unique_id(),
      codec.int_encoder(),
      codec.int_decoder(),
    )

  // Just verify the child spec can be created without error
  let _spec =
    actor.child_spec(tn, 0, fn(_msg, state) { receiver.Continue(state) })
  should.be_true(True)
}

pub fn actor_start_supervised_test() {
  let name = "sup_actor_" <> unique_id()
  let tn =
    registry.typed_name(name, codec.string_encoder(), codec.string_decoder())

  let assert Ok(sup_pid) =
    actor.start_supervised(tn, Nil, fn(_msg, state) { receiver.Continue(state) })

  // Supervisor should be alive
  should.be_true(process.is_alive(sup_pid))

  // Actor should be discoverable by name
  process.sleep(100)
  should.be_true(registry.is_registered(name))

  // Cleanup
  let assert Ok(Nil) = registry.unregister(name)
}

pub fn actor_pool_test() {
  let prefix = "pool_" <> unique_id()
  let tn = registry.typed_name(prefix, codec.int_encoder(), codec.int_decoder())

  let assert Ok(sup_pid) =
    actor.pool(tn, 3, 0, fn(_msg, state) { receiver.Continue(state) })

  // Supervisor alive
  should.be_true(process.is_alive(sup_pid))

  // Workers should be discoverable by name
  process.sleep(100)
  should.be_true(registry.is_registered(prefix <> "_1"))
  should.be_true(registry.is_registered(prefix <> "_2"))
  should.be_true(registry.is_registered(prefix <> "_3"))

  // Send to a specific worker by name using pool_member
  let assert Ok(gs) = registry.lookup(registry.pool_member(tn, 1))
  let assert Ok(Nil) = global.send(gs, 42)

  // Cleanup
  let assert Ok(Nil) = registry.unregister(prefix <> "_1")
  let assert Ok(Nil) = registry.unregister(prefix <> "_2")
  let assert Ok(Nil) = registry.unregister(prefix <> "_3")
}

/// Verifies that the deterministic name-based tag allows messages sent via
/// a `registry.lookup` to actually reach the OTP actor's handler.
pub fn actor_registry_roundtrip_test() {
  let tn =
    registry.typed_name(
      "roundtrip_" <> unique_id(),
      codec.int_encoder(),
      codec.int_decoder(),
    )

  // A subject the actor will write its accumulated state to
  let result_subj = process.new_subject()

  let assert Ok(_gs) =
    actor.start_registered(tn, 0, fn(msg, state) {
      let new_state = state + msg
      case new_state >= 10 {
        True -> {
          process.send(result_subj, new_state)
          receiver.Stop
        }
        False -> receiver.Continue(new_state)
      }
    })

  // Look up the actor by typed name — same TypedName = same msg type
  let assert Ok(found) = registry.lookup(tn)

  // Send through the reconstructed GlobalSubject
  let assert Ok(Nil) = global.send(found, 3)
  let assert Ok(Nil) = global.send(found, 7)

  // The actor sums 3+7=10, signals done
  let assert Ok(total) = process.receive(result_subj, 2000)
  should.equal(total, 10)

  // Cleanup
  let assert Ok(Nil) = registry.unregister(registry.typed_name_to_string(tn))
}

// Simple unique ID generator for test isolation
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
