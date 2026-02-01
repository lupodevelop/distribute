import distribute/settings
import gleeunit
import gleeunit/should

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn toggle_allow_atom_creation_test() {
  settings.set_allow_atom_creation(True)
  should.be_true(settings.is_allow_atom_creation())
  settings.set_allow_atom_creation(False)
  should.be_false(settings.is_allow_atom_creation())
}

pub fn toggle_use_crypto_ids_test() {
  settings.set_use_crypto_ids(True)
  should.be_true(settings.is_use_crypto_ids())
  settings.set_use_crypto_ids(False)
  should.be_false(settings.is_use_crypto_ids())
}

pub fn max_message_size_default_test() {
  // Default should be 10MB
  settings.get_max_message_size()
  |> should.equal(settings.default_max_message_size)
}

pub fn max_message_size_configurable_test() {
  // Save original
  let original = settings.get_max_message_size()

  // Set custom size (50MB)
  let custom_size = 50 * 1024 * 1024
  settings.set_max_message_size(custom_size)
  settings.get_max_message_size()
  |> should.equal(custom_size)

  // Set to zero (disable checking)
  settings.set_max_message_size(0)
  settings.get_max_message_size()
  |> should.equal(0)

  // Restore original
  settings.set_max_message_size(original)
  settings.get_max_message_size()
  |> should.equal(original)
}

pub fn default_max_message_size_constant_test() {
  // Verify constant value is 10MB
  settings.default_max_message_size
  |> should.equal(10_485_760)
}
