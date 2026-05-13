import gleam/erlang/process
import gleeunit/should

@external(erlang, "subject_layout_test_ffi", "check_layout")
fn check_layout_ffi(subject: process.Subject(msg)) -> Bool

/// This test prevents catastrophic runtime failure across the cluster
/// if `gleam_erlang` decides to change the internal layout of `process.Subject`
/// in a future version. Our FFI code relies heavily on the undocumented
/// `{subject, Pid, Tag}` tuple layout.
pub fn subject_layout_is_stable_test() {
  let subj: process.Subject(String) = process.new_subject()
  
  check_layout_ffi(subj)
  |> should.be_true
}
