import gleeunit
import gleeunit/should
import settings

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
