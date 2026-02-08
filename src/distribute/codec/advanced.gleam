/// Advanced codecs using Erlang term serialization.
///
/// ⚠️ **WARNING**: These codecs use Erlang's `term_to_binary`/`binary_to_term`
/// which bypasses Gleam's type system. They are provided as an **escape hatch**
/// for advanced use cases but should be avoided when possible.
///
/// **Risks:**
/// - No compile-time type checking
/// - Binary format may change between Erlang/OTP versions
/// - Type changes in your code will cause silent runtime failures
/// - Not suitable for long-term storage or cross-language communication
///
/// **When to use:**
/// - Temporary serialization within a single running cluster
/// - Pid/Subject transmission (no other option on BEAM)
/// - Prototyping before implementing proper codecs
///
/// **Prefer instead:**
/// - Built-in typed codecs (string, int, bool, etc.)
/// - Custom codecs for your domain types
/// - Schema-based encoding for versioned protocols

import distribute/codec.{type Decoder, type Encoder, InvalidBinary}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}

// ============================================================================
// FFI
// ============================================================================

@external(erlang, "erlang", "term_to_binary")
fn term_to_binary(x: a) -> BitArray

@external(erlang, "codec_ffi", "safe_binary_to_term")
fn safe_binary_to_term(b: BitArray) -> Result(Dynamic, Dynamic)

@external(erlang, "codec_ffi", "unsafe_coerce")
fn unsafe_coerce(x: Dynamic) -> a

// ============================================================================
// Dynamic codecs
// ============================================================================

/// Encoder for any Gleam/Erlang term using `term_to_binary`.
///
/// ⚠️ **ESCAPE HATCH** — This encoder bypasses Gleam's type system.
/// The binary format is opaque and may not be portable across:
/// - Different Erlang/OTP versions
/// - Different Gleam versions
/// - Type definition changes in your code
///
/// Use typed codecs (string_encoder, int_encoder, etc.) whenever possible.
/// This is suitable only for temporary in-cluster communication or prototyping.
pub fn dynamic_encoder() -> Encoder(Dynamic) {
  fn(d) { Ok(term_to_binary(d)) }
}

/// Decoder for any Gleam/Erlang term using `binary_to_term`.
///
/// ⚠️ **ESCAPE HATCH** — This decoder bypasses Gleam's type system.
/// The returned `Dynamic` must be validated using `gleam/dynamic` decoders.
/// Binary data from untrusted sources should be treated with extreme caution.
///
/// Uses `binary_to_term([safe])` to prevent atom table attacks, but the
/// decoded value still requires runtime type checking.
pub fn dynamic_decoder() -> Decoder(Dynamic) {
  fn(b) {
    case safe_binary_to_term(b) {
      Ok(d) -> Ok(d)
      Error(_) -> Error(InvalidBinary("invalid erlang term"))
    }
  }
}

/// Encode any Gleam value to binary using Erlang term serialization.
///
/// ⚠️ **ESCAPE HATCH** — Bypasses type safety. See `dynamic_encoder` warnings.
/// This is a convenience function for quick prototyping.
pub fn any_encoder() -> Encoder(a) {
  fn(value) { Ok(term_to_binary(value)) }
}

// ============================================================================
// Pid codecs
// ============================================================================

/// Encoder for Pids.
///
/// Note: Pids are inherently untyped in Erlang. This encoder uses
/// `term_to_binary` which is safe for same-cluster communication.
pub fn pid_encoder() -> Encoder(Pid) {
  fn(p) { Ok(term_to_binary(p)) }
}

/// Decoder for Pids.
///
/// Note: The decoded Pid is validated as a proper Erlang pid internally.
pub fn pid_decoder() -> Decoder(Pid) {
  fn(b) {
    case safe_binary_to_term(b) {
      Ok(d) -> Ok(unsafe_coerce(d))
      Error(_) -> Error(InvalidBinary("invalid pid binary"))
    }
  }
}

// ============================================================================
// Subject codecs
// ============================================================================

/// Encoder for Subjects.
///
/// Note: Subject serialization preserves the Pid but the type parameter
/// is erased. The receiving side must know the expected message type.
pub fn subject_encoder() -> Encoder(Subject(a)) {
  fn(s) { Ok(term_to_binary(s)) }
}

/// Decoder for Subjects.
///
/// ⚠️ **Type parameter is not validated** — The returned Subject(a) will
/// accept any type parameter at compile time. Ensure the type matches
/// what was encoded, or use Schema-based messaging for safety.
pub fn subject_decoder() -> Decoder(Subject(a)) {
  fn(b) {
    case safe_binary_to_term(b) {
      Ok(d) -> Ok(unsafe_coerce(d))
      Error(_) -> Error(InvalidBinary("invalid subject binary"))
    }
  }
}
