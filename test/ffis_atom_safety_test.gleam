import gleeunit
import gleeunit/should
import distribute/groups
import distribute/log
import distribute/registry
import distribute/settings
import gleam/erlang/process.{type Pid}

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
