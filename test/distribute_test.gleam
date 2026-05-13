import distribute
import distribute/actor
import distribute/codec
import distribute/receiver
import distribute/registry
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

pub fn version_test() {
  should.equal(distribute.version(), "4.0.0")
}

/// Regression: `distribute.version()` is hardcoded in
/// `src/distribute.gleam` (Gleam has no compile-time API to read
/// `gleam.toml`). A release that bumps the manifest without bumping
/// the constant. or vice versa. ships a mislabelled package.
/// This test parses `gleam.toml` and asserts the two strings match.
pub fn version_matches_gleam_toml_test() {
  let manifest_version = read_gleam_toml_version()
  should.equal(distribute.version(), manifest_version)
}

@external(erlang, "distribute_test_ffi", "read_gleam_toml_version")
fn read_gleam_toml_version() -> String

pub fn facade_new_subject_send_receive_test() {
  // new_subject uses unique tags. no message mixing
  let subj = distribute.new_subject(codec.string())
  let assert Ok(Nil) = distribute.send(subj, "facade-msg")
  let assert Ok(msg) = distribute.receive_with_timeout(subj, 1000)
  should.equal(msg, "facade-msg")
}

pub fn facade_multiple_subjects_isolated_test() {
  // Two new_subject calls on same process should NOT interfere
  let s1 = distribute.new_subject(codec.int())
  let s2 = distribute.new_subject(codec.int())

  let assert Ok(Nil) = distribute.send(s1, 111)
  let assert Ok(Nil) = distribute.send(s2, 222)

  // Each receives its own message, not the other's
  let assert Ok(msg1) = distribute.receive_with_timeout(s1, 1000)
  let assert Ok(msg2) = distribute.receive_with_timeout(s2, 1000)
  should.equal(msg1, 111)
  should.equal(msg2, 222)
}

pub fn facade_start_node_invalid_name_rejected_test() {
  // start_node validates `@` presence. guarantees the function exists
  // (was renamed from the ambiguous `start` in v4.0.0) and forwards
  // input validation to the cluster module.
  let result = distribute.start_node("invalid_no_at", "cookie")
  should.be_error(result)
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
      "facade_actor_" <> test_helpers.unique_id(),
      codec.string_encoder(),
      codec.string_decoder(),
    )

  // start_actor creates a named OTP actor, returns Result
  let assert Ok(gs) =
    distribute.start_actor_with_timeout(
      tn,
      Nil,
      fn(_msg, state) { receiver.Continue(state) },
      5000,
    )

  // Can send to the actor
  let assert Ok(Nil) = distribute.send(gs, "hello-actor")
  should.be_true(True)
}

pub fn facade_register_lookup_test() {
  let tn =
    registry.typed_name(
      "facade_test_" <> test_helpers.unique_id(),
      codec.int_encoder(),
      codec.int_decoder(),
    )

  // Use a named actor for registry (deterministic tag)
  let assert Ok(gs) =
    distribute.start_actor_with_timeout(
      tn,
      0,
      fn(_msg, state) { receiver.Continue(state) },
      5000,
    )

  let assert Ok(Nil) = distribute.register(tn, gs)

  // Lookup returns a remote handle with matching deterministic tag
  let assert Ok(found) = distribute.lookup(tn)

  // Send through the found handle. tag matches the actor!
  let assert Ok(Nil) = distribute.send(found, 42)

  // Cleanup
  distribute.unregister(registry.typed_name_to_string(tn))
}

// ---------------------------------------------------------------------------
// distribute.named. facade shortcut for registry.named
// ---------------------------------------------------------------------------

pub fn facade_named_creates_typed_name_test() {
  // distribute.named should work identically to registry.named
  let tn =
    distribute.named(
      "facade_named_" <> test_helpers.unique_id(),
      codec.string(),
    )

  let assert Ok(gs) =
    distribute.start_actor(tn, Nil, fn(_msg, state) { receiver.Continue(state) })

  let assert Ok(Nil) = distribute.send(gs, "ping")
  should.be_true(True)
}

// ---------------------------------------------------------------------------
// distribute.start_registered / start_registered_default
// ---------------------------------------------------------------------------

pub fn facade_start_registered_with_timeout_test() {
  let tn =
    distribute.named("facade_reg_" <> test_helpers.unique_id(), codec.int())

  let assert Ok(_gs) =
    distribute.start_registered_with_timeout(
      tn,
      0,
      fn(_msg, state) { receiver.Continue(state) },
      5000,
    )

  should.be_true(registry.is_registered(registry.typed_name_to_string(tn)))
  distribute.unregister(registry.typed_name_to_string(tn))
}

pub fn facade_start_registered_default_test() {
  let tn =
    distribute.named(
      "facade_reg_def_" <> test_helpers.unique_id(),
      codec.string(),
    )

  let assert Ok(_gs) =
    distribute.start_registered(tn, Nil, fn(_msg, state) {
      receiver.Continue(state)
    })

  should.be_true(registry.is_registered(registry.typed_name_to_string(tn)))
  distribute.unregister(registry.typed_name_to_string(tn))
}

pub fn facade_start_registered_error_test() {
  // Double registration should surface GlobalRegisterFailed.
  let tn =
    distribute.named("facade_reg_err_" <> test_helpers.unique_id(), codec.int())

  let assert Ok(_gs) =
    distribute.start_registered(tn, 0, fn(_msg, state) {
      receiver.Continue(state)
    })

  let result =
    distribute.start_registered(tn, 0, fn(_msg, state) {
      receiver.Continue(state)
    })

  let assert Error(actor.GlobalRegisterFailed(_)) = result
  distribute.unregister(registry.typed_name_to_string(tn))
}
