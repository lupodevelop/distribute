import gleeunit/should

// Each scenario starts a fresh VM: config.reset() would initialise the
// counter in the test process and hide the first-caller lifetime bug.
@external(erlang, "atom_budget_test_ffi", "normal_exit")
fn normal_exit() -> Result(Nil, String)

@external(erlang, "atom_budget_test_ffi", "killed_caller")
fn killed_caller() -> Result(Nil, String)

@external(erlang, "atom_budget_test_ffi", "concurrent_first_callers")
fn concurrent_first_callers() -> Result(Nil, String)

pub fn atom_budget_survives_first_caller_normal_exit_test() {
  normal_exit() |> should.equal(Ok(Nil))
}

pub fn atom_budget_survives_first_caller_kill_test() {
  killed_caller() |> should.equal(Ok(Nil))
}

pub fn atom_budget_is_shared_by_concurrent_first_callers_test() {
  concurrent_first_callers() |> should.equal(Ok(Nil))
}
