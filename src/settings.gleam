/// Settings API for the distribute library
@external(erlang, "settings_ffi", "set_allow_atom_creation")
fn set_allow_atom_creation_ffi(b: Bool) -> a

@external(erlang, "settings_ffi", "get_allow_atom_creation")
fn get_allow_atom_creation_ffi() -> Bool

@external(erlang, "settings_ffi", "set_use_crypto_ids")
fn set_use_crypto_ids_ffi(b: Bool) -> a

@external(erlang, "settings_ffi", "get_use_crypto_ids")
fn get_use_crypto_ids_ffi() -> Bool

pub fn set_allow_atom_creation(allow: Bool) -> Nil {
  let _ = set_allow_atom_creation_ffi(allow)
  Nil
}

pub fn is_allow_atom_creation() -> Bool {
  get_allow_atom_creation_ffi()
}

pub fn set_use_crypto_ids(use_crypto: Bool) -> Nil {
  let _ = set_use_crypto_ids_ffi(use_crypto)
  Nil
}

pub fn is_use_crypto_ids() -> Bool {
  get_use_crypto_ids_ffi()
}
