/// Tests for whereis_global - type-safe global registry lookup
import distribute/codec
import distribute/global
import distribute/registry
import distribute/settings
import gleam/dynamic
import gleam/erlang/process
import gleam/int
import gleeunit
import gleeunit/should

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

/// Generate a unique name for registry entries to avoid conflicts between test runs
fn unique_name(prefix: String) -> String {
  prefix <> "_" <> int.to_string(unique_integer())
}

pub fn main() {
  // Allow atom creation for global registry names in tests
  settings.set_allow_atom_creation(True)
  gleeunit.main()
}

// ============================================================================
// whereis_global tests
// ============================================================================

pub fn whereis_global_not_found_test() {
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()

  let result =
    registry.whereis_global("nonexistent_service_xyz", encoder, decoder)

  result
  |> should.be_error()
}

pub fn whereis_global_roundtrip_test() {
  settings.set_allow_atom_creation(True)

  let name = unique_name("test_global_service")

  // Spawn in separate process to avoid PID registration conflicts
  let _pid =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let global_subject = global.new(encoder, decoder)

      let assert Ok(_) =
        registry.register_typed(name, global.subject(global_subject))

      // Look it up using whereis_global
      let assert Ok(looked_up) = registry.whereis_global(name, encoder, decoder)

      // Verify owner
      let assert Ok(original_owner) = global.owner(global_subject)
      let assert Ok(looked_up_owner) = global.owner(looked_up)

      original_owner |> should.equal(looked_up_owner)

      // Cleanup
      let assert Ok(_) = registry.unregister(name)

      // Keep alive briefly
      process.sleep(100)
    })

  // Wait for spawned process
  process.sleep(200)
  should.be_true(True)
}

pub fn whereis_global_type_safe_send_receive_test() {
  settings.set_allow_atom_creation(True)

  let name = unique_name("echo_service")

  // Spawn in separate process
  let _pid =
    process.spawn(fn() {
      let encoder = codec.string_encoder()
      let decoder = codec.string_decoder()
      let sender_global = global.new(encoder, decoder)

      let assert Ok(_) =
        registry.register_typed(name, global.subject(sender_global))

      // Lookup from "another node" (simulated)
      let assert Ok(receiver_global) =
        registry.whereis_global(name, encoder, decoder)

      // Send a message
      let assert Ok(_) = global.send(receiver_global, "Hello, type-safe world!")

      // Receive it
      let assert Ok(msg) = global.receive(sender_global, 1000)

      msg |> should.equal("Hello, type-safe world!")

      // Cleanup
      let assert Ok(_) = registry.unregister(name)

      process.sleep(100)
    })

  process.sleep(200)
  should.be_true(True)
}

pub fn whereis_global_different_types_test() {
  settings.set_allow_atom_creation(True)

  let name = unique_name("int_service")

  // Spawn in separate process
  let _pid =
    process.spawn(fn() {
      let encoder = codec.int_encoder()
      let decoder = codec.int_decoder()
      let global_subject = global.new(encoder, decoder)

      let assert Ok(_) =
        registry.register_typed(name, global.subject(global_subject))

      let assert Ok(looked_up) = registry.whereis_global(name, encoder, decoder)

      // Send Int
      let assert Ok(_) = global.send(looked_up, 42)

      // Receive Int
      let assert Ok(value) = global.receive(global_subject, 1000)

      value |> should.equal(42)

      // Cleanup
      let assert Ok(_) = registry.unregister(name)

      process.sleep(100)
    })

  process.sleep(200)
  should.be_true(True)
}

// ============================================================================
// whereis_with_tag tests
// ============================================================================

pub fn whereis_with_tag_nil_test() {
  settings.set_allow_atom_creation(True)

  let name = unique_name("nil_tag_service")

  // Spawn in separate process
  let _pid =
    process.spawn(fn() {
      let subject = process.new_subject()

      let assert Ok(_) = registry.register_typed(name, subject)

      // Lookup with explicit Nil tag
      let assert Ok(looked_up) = registry.whereis_with_tag(name, dynamic.nil())

      // Verify owner is the same
      let assert Ok(original_owner) = process.subject_owner(subject)
      let assert Ok(looked_up_owner) = process.subject_owner(looked_up)

      original_owner |> should.equal(looked_up_owner)

      // Cleanup
      let assert Ok(_) = registry.unregister(name)

      process.sleep(100)
    })

  process.sleep(200)
  should.be_true(True)
}
// ============================================================================
// Integration test: Multiple GlobalSubjects with different types
// ============================================================================
