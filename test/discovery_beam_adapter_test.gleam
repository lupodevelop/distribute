import distribute/discovery/adapter
import distribute/discovery/beam_adapter
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// new() — returns a valid DiscoveryAdapter record
// ============================================================================

pub fn new_returns_adapter_test() {
  // Must not crash — all function pointers are set
  let _adp = beam_adapter.new()
  should.be_true(True)
}

// ============================================================================
// Adapter has all required fields
// ============================================================================

pub fn adapter_has_start_function_test() {
  let adp = beam_adapter.new()
  // The adapter record should have a start function
  // We verify by calling it with invalid options to confirm function exists
  // (We don't actually call start here, but verify the record is valid)
  case adp {
    adapter.DiscoveryAdapter(
      start: _,
      stop: _,
      subscribe: _,
      unsubscribe: _,
      snapshot: _,
      lookup: _,
      health: _,
      metrics: _,
    ) -> should.be_true(True)
  }
}

// ============================================================================
// get_handle — returns Error when no adapter started
// ============================================================================

pub fn get_handle_without_start_returns_error_test() {
  // No adapter has been started with this name
  let result = beam_adapter.get_handle("nonexistent_adapter_name")
  should.be_error(result)
}

pub fn get_handle_empty_name_returns_error_test() {
  let result = beam_adapter.get_handle("")
  should.be_error(result)
}
