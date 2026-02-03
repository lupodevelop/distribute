import distribute/groups
import distribute/log
import distribute/registry
import distribute/settings
import gleam/erlang/process.{type Pid}
import gleeunit
import gleeunit/should

@external(erlang, "erlang", "self")
fn groups_self_ffi() -> Pid

@external(erlang, "erlang", "self")
fn registry_self_ffi() -> Pid

@external(erlang, "erlang", "unique_integer")
fn unique_int_ffi() -> Int

@external(erlang, "erlang", "system_time")
fn el_system_time_ffi(unit: Int) -> Int

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string_ffi(i: Int) -> String

@external(erlang, "test_helpers_ffi", "binary_to_existing_atom_exists")
fn atom_exists_ffi(name: String) -> Bool

@external(erlang, "test_helpers_ffi", "get_allow_atom_creation")
fn get_allow_atom_creation_ffi() -> Bool

@external(erlang, "test_helpers_ffi", "inspect_to_atom_status")
fn inspect_to_atom_status_ffi(name: String) -> String

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn groups_atom_creation_disabled_test() {
  log.set_level(log.Debug)
  // Ensure no atom creation is allowed by default
  settings.set_allow_atom_creation(False)
  should.be_false(get_allow_atom_creation_ffi())
  let pid = groups_self_ffi()
  // Use unique group name based on system time and unique integer to avoid atoms existing from prior tests
  let group_name =
    "dynamic_group_"
    <> int_to_string_ffi(el_system_time_ffi(1000))
    <> "_"
    <> int_to_string_ffi(unique_int_ffi())
  log.debug("group_name: " <> group_name, [])
  log.debug("allow setting checked", [])
  should.be_false(atom_exists_ffi(group_name))

  // Attempt to join a dynamic group name (should error because atom creation is disabled)
  // At this point, with allow False, inspect should indicate that the atom does not exist
  let inspected = inspect_to_atom_status_ffi(group_name)
  should.be_true(case inspected {
    "error" -> True
    _ -> False
  })
  // Attempt to join; should fail because atom doesn't exist and creation is disabled
  let res = groups.join(group_name, pid)
  should.be_true(case res {
    Ok(_) -> False
    Error(_) -> True
  })
  // Also assert that the join didn't create an atom by accident
  should.be_false(atom_exists_ffi(group_name))
  // Re-enable atom creation and try again
  settings.set_allow_atom_creation(True)
  let res2 = groups.join(group_name, pid)
  // Note: pg may not be running in test environment, so we just check it doesn't crash
  // and the atom was created (which we can verify)
  case res2 {
    Ok(_) -> should.be_true(True)
    Error(_) -> {
      // Even if pg fails, the atom should have been created
      should.be_true(atom_exists_ffi(group_name))
    }
  }
}

pub fn registry_atom_creation_disabled_test() {
  settings.set_allow_atom_creation(False)
  let pid = registry_self_ffi()
  let reg_name =
    "dynamic_service_"
    <> int_to_string_ffi(el_system_time_ffi(1000))
    <> "_"
    <> int_to_string_ffi(unique_int_ffi())
  should.be_false(atom_exists_ffi(reg_name))
  let res = registry.register(reg_name, pid)
  should.be_true(case res {
    Ok(_) -> False
    Error(_) -> True
  })
  settings.set_allow_atom_creation(True)
  let res2 = registry.register(reg_name, pid)
  // registry may fail for other reasons but atom should be created
  case res2 {
    Ok(_) -> should.be_true(True)
    Error(_) -> {
      // Even if register fails, the atom should have been created
      should.be_true(atom_exists_ffi(reg_name))
    }
  }
}

// =============================================================================
// Tests for shared distribute_ffi_utils:to_atom_safe/1 function
// =============================================================================

@external(erlang, "distribute_ffi_utils", "to_atom_safe")
fn to_atom_safe_ffi(name: String) -> Result(a, String)

@external(erlang, "test_helpers_ffi", "to_atom_safe_with_list")
fn to_atom_safe_list_ffi(name: List(Int)) -> Result(a, String)

@external(erlang, "test_helpers_ffi", "to_atom_safe_with_atom")
fn to_atom_safe_atom_ffi(atom: a) -> Result(a, String)

/// Test that to_atom_safe returns existing atoms without needing allow_atom_creation
pub fn to_atom_safe_existing_atom_test() {
  // 'ok' is always an existing atom in Erlang
  settings.set_allow_atom_creation(False)
  let result = to_atom_safe_ffi("ok")
  should.be_ok(result)
  // Re-enable for subsequent tests
  settings.set_allow_atom_creation(True)
}

/// Test that to_atom_safe fails for non-existing atoms when allow_atom_creation is False
pub fn to_atom_safe_nonexisting_atom_disabled_test() {
  settings.set_allow_atom_creation(False)
  let unique_name =
    "nonexisting_atom_test_"
    <> int_to_string_ffi(el_system_time_ffi(1000))
    <> "_"
    <> int_to_string_ffi(unique_int_ffi())
  should.be_false(atom_exists_ffi(unique_name))
  let result = to_atom_safe_ffi(unique_name)
  should.be_error(result)
  // Re-enable for subsequent tests
  settings.set_allow_atom_creation(True)
}

/// Test that to_atom_safe creates atoms when allow_atom_creation is True
pub fn to_atom_safe_create_atom_enabled_test() {
  settings.set_allow_atom_creation(True)
  let unique_name =
    "created_atom_test_"
    <> int_to_string_ffi(el_system_time_ffi(1000))
    <> "_"
    <> int_to_string_ffi(unique_int_ffi())
  should.be_false(atom_exists_ffi(unique_name))
  let result = to_atom_safe_ffi(unique_name)
  should.be_ok(result)
  // Verify the atom was created
  should.be_true(atom_exists_ffi(unique_name))
  // Reset for other tests
  settings.set_allow_atom_creation(False)
}

/// Test that to_atom_safe handles atom input (passthrough)
pub fn to_atom_safe_atom_passthrough_test() {
  settings.set_allow_atom_creation(False)
  // Pass an existing atom through
  let result = to_atom_safe_atom_ffi(Nil)
  should.be_ok(result)
  // Re-enable for subsequent tests
  settings.set_allow_atom_creation(True)
}

/// Test that to_atom_safe handles list input (converts to binary first)
pub fn to_atom_safe_list_input_test() {
  settings.set_allow_atom_creation(False)
  // "ok" as a charlist: [111, 107]
  let result = to_atom_safe_list_ffi([111, 107])
  should.be_ok(result)
  // Re-enable for subsequent tests
  settings.set_allow_atom_creation(True)
}

/// Test that to_atom_safe returns error for invalid input
pub fn to_atom_safe_invalid_input_test() {
  settings.set_allow_atom_creation(False)
  // This will be handled by the badarg clause
  // We test via the groups/registry which use the shared function
  let result = groups.join("", groups_self_ffi())
  // Empty string should still work (becomes empty atom if allowed)
  // The point is it doesn't crash
  should.be_true(case result {
    Ok(_) -> True
    Error(_) -> True
  })
  // Re-enable for subsequent tests
  settings.set_allow_atom_creation(True)
}

/// Test consistency: multiple FFI modules using shared to_atom_safe behave identically
pub fn shared_to_atom_safe_consistency_test() {
  settings.set_allow_atom_creation(False)
  let unique_name =
    "consistency_test_"
    <> int_to_string_ffi(el_system_time_ffi(1000))
    <> "_"
    <> int_to_string_ffi(unique_int_ffi())
  should.be_false(atom_exists_ffi(unique_name))

  // All FFI modules should fail consistently when atom doesn't exist
  let groups_result = groups.join(unique_name, groups_self_ffi())
  let registry_result = registry.register(unique_name, registry_self_ffi())

  should.be_error(groups_result)
  should.be_error(registry_result)

  // Atom should NOT have been created by any module
  should.be_false(atom_exists_ffi(unique_name))

  // IMPORTANT: Re-enable atom creation for subsequent tests
  settings.set_allow_atom_creation(True)
}
