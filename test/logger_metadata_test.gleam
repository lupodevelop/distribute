import distribute/log
import gleeunit
import gleeunit/should

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn logger_console_metadata_test() {
  // Ensure backend doesn't crash and metadata is handled
  log.set_backend("console")
  let id = log.generate_correlation_id()
  log.info_with_correlation("Test with meta", [#("k", "v")], id)
  // Nothing to assert besides no crash and the functions exist
  should.be_true(True)
}

pub fn logger_erlang_logger_metadata_test() {
  // If OTP logger exists, should not crash when using metadata
  log.set_backend("erlang_logger")
  let id = log.generate_correlation_id()
  log.error_with_correlation("Err with meta", [#("k1", "v1")], id)
  // revert to console
  log.set_backend("console")
  should.be_true(True)
}
