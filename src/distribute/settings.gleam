/// Settings API for the distribute library.
/// 
/// These settings control global behavior of the library, particularly
/// around security-sensitive operations like atom creation.

@external(erlang, "settings_ffi", "set_allow_atom_creation")
fn set_allow_atom_creation_ffi(b: Bool) -> a

@external(erlang, "settings_ffi", "get_allow_atom_creation")
fn get_allow_atom_creation_ffi() -> Bool

@external(erlang, "settings_ffi", "set_use_crypto_ids")
fn set_use_crypto_ids_ffi(b: Bool) -> a

@external(erlang, "settings_ffi", "get_use_crypto_ids")
fn get_use_crypto_ids_ffi() -> Bool

/// Enable or disable dynamic atom creation from untrusted input.
/// 
/// When disabled (default), functions that would create atoms from
/// external data will fail safely instead. This prevents atom table
/// exhaustion attacks.
pub fn set_allow_atom_creation(allow: Bool) -> Nil {
  let _ = set_allow_atom_creation_ffi(allow)
  Nil
}

/// Check if dynamic atom creation is currently allowed.
pub fn is_allow_atom_creation() -> Bool {
  get_allow_atom_creation_ffi()
}

/// Enable or disable cryptographic identifiers for registry names.
/// 
/// When enabled, registry names are hashed using a secure algorithm
/// to prevent name collision attacks.
pub fn set_use_crypto_ids(use_crypto: Bool) -> Nil {
  let _ = set_use_crypto_ids_ffi(use_crypto)
  Nil
}

/// Check if cryptographic identifiers are currently enabled.
pub fn is_use_crypto_ids() -> Bool {
  get_use_crypto_ids_ffi()
}
