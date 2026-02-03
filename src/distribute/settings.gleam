/// Settings API for the distribute library.
/// 
/// These settings control global behavior of the library, particularly
/// around security-sensitive operations like atom creation and message
/// size limits.
@external(erlang, "settings_ffi", "set_allow_atom_creation")
fn set_allow_atom_creation_ffi(b: Bool) -> a

@external(erlang, "settings_ffi", "get_allow_atom_creation")
fn get_allow_atom_creation_ffi() -> Bool

@external(erlang, "settings_ffi", "set_use_crypto_ids")
fn set_use_crypto_ids_ffi(b: Bool) -> a

@external(erlang, "settings_ffi", "get_use_crypto_ids")
fn get_use_crypto_ids_ffi() -> Bool

@external(erlang, "settings_ffi", "set_max_message_size")
fn set_max_message_size_ffi(size: Int) -> a

@external(erlang, "settings_ffi", "get_max_message_size")
fn get_max_message_size_ffi() -> Int

/// Default maximum message size: 10MB (10485760 bytes)
pub const default_max_message_size = 10_485_760

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

/// Set the maximum allowed message size in bytes.
///
/// Messages larger than this limit will be rejected by `messaging.send_binary`
/// and `groups.broadcast_binary`. This prevents memory exhaustion attacks
/// from malicious or misconfigured peers.
///
/// ## Parameters
///
/// - `size`: Maximum message size in bytes. Must be >= 0.
///   Use 0 to disable size checking (not recommended for production).
///
/// ## Default
///
/// The default is 10MB (10,485,760 bytes), which is suitable for most
/// use cases. Increase if you need to send larger payloads.
///
/// ## Example
///
/// ```gleam
/// // Allow messages up to 50MB
/// settings.set_max_message_size(50 * 1024 * 1024)
///
/// // Restore default
/// settings.set_max_message_size(settings.default_max_message_size)
/// ```
pub fn set_max_message_size(size: Int) -> Nil {
  let _ = set_max_message_size_ffi(size)
  Nil
}

/// Get the current maximum allowed message size in bytes.
///
/// Returns the configured limit, or 10MB if not explicitly set.
pub fn get_max_message_size() -> Int {
  get_max_message_size_ffi()
}
