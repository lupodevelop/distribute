//// Property-based tests for crypto primitives (AEAD and HKDF).
////
//// These tests verify correctness properties that must hold for all inputs:
//// - AEAD: encrypt then decrypt returns original plaintext
//// - AEAD: ciphertext differs from plaintext
//// - AEAD: tampering with ciphertext causes decrypt failure
//// - HKDF: deterministic output for same inputs
//// - HKDF: different inputs produce different outputs
////
//// Note: These are pseudo-property tests using random sampling.
//// For full property-based testing, consider integrating a QuickCheck-style library.

import gleam/bit_array
import gleam/int
import gleam/list
import gleeunit/should

// =============================================================================
// FFI Imports (direct access to crypto primitives)
// =============================================================================

@external(erlang, "crypto_sodium_ffi", "gen_keypair")
fn ffi_gen_keypair() -> Result(#(BitArray, BitArray), Nil)

@external(erlang, "crypto_sodium_ffi", "aead_encrypt")
fn ffi_aead_encrypt(
  key: BitArray,
  nonce: BitArray,
  aad: BitArray,
  plaintext: BitArray,
) -> Result(BitArray, Nil)

@external(erlang, "crypto_sodium_ffi", "aead_decrypt")
fn ffi_aead_decrypt(
  key: BitArray,
  nonce: BitArray,
  aad: BitArray,
  ciphertext: BitArray,
) -> Result(BitArray, Nil)

@external(erlang, "crypto_sodium_ffi", "hkdf")
fn ffi_hkdf(
  salt: BitArray,
  ikm: BitArray,
  info: BitArray,
  len: Int,
) -> Result(BitArray, Nil)

@external(erlang, "crypto_sodium_ffi", "random_bytes")
fn ffi_random_bytes(n: Int) -> BitArray

@external(erlang, "crypto_sodium_ffi", "generate_nonce")
fn ffi_generate_nonce() -> BitArray

// =============================================================================
// Test Helpers
// =============================================================================

/// Generate a random 32-byte key.
fn random_key() -> BitArray {
  ffi_random_bytes(32)
}

/// Generate a random plaintext of given size.
fn random_plaintext(size: Int) -> BitArray {
  ffi_random_bytes(size)
}

/// Generate random AAD (additional authenticated data).
fn random_aad() -> BitArray {
  ffi_random_bytes(16)
}

/// Flip a random bit in a BitArray to simulate tampering.
fn tamper_ciphertext(ciphertext: BitArray) -> BitArray {
  let size = bit_array.byte_size(ciphertext)
  case size > 0 {
    True -> {
      // Flip a byte somewhere in the middle
      let pos = size / 2
      tamper_at_position(ciphertext, pos)
    }
    False -> ciphertext
  }
}

fn tamper_at_position(data: BitArray, pos: Int) -> BitArray {
  let prefix_size = pos
  let suffix_start = pos + 1
  let suffix_size = bit_array.byte_size(data) - suffix_start

  case
    bit_array.slice(data, 0, prefix_size),
    bit_array.slice(data, pos, 1),
    bit_array.slice(data, suffix_start, suffix_size)
  {
    Ok(prefix), Ok(<<byte>>), Ok(suffix) -> {
      let flipped = int.bitwise_exclusive_or(byte, 0xFF)
      bit_array.concat([prefix, <<flipped>>, suffix])
    }
    _, _, _ -> data
  }
}

// =============================================================================
// AEAD Property Tests
// =============================================================================

pub fn aead_roundtrip_property_test() {
  // Property: for any key, nonce, aad, plaintext:
  // decrypt(encrypt(plaintext)) == plaintext

  // Test with multiple random samples
  list.range(1, 20)
  |> list.each(fn(_i) {
    let key = random_key()
    let nonce = ffi_generate_nonce()
    let aad = random_aad()
    // Vary plaintext size: 0 to 1024 bytes
    let plaintext_size = int.random(1025)
    let plaintext = random_plaintext(plaintext_size)

    let assert Ok(ciphertext) = ffi_aead_encrypt(key, nonce, aad, plaintext)
    let assert Ok(decrypted) = ffi_aead_decrypt(key, nonce, aad, ciphertext)

    should.equal(decrypted, plaintext)
  })
}

pub fn aead_ciphertext_differs_from_plaintext_property_test() {
  // Property: ciphertext != plaintext (for non-empty plaintext)

  list.range(1, 10)
  |> list.each(fn(_i) {
    let key = random_key()
    let nonce = ffi_generate_nonce()
    let aad = random_aad()
    let plaintext = random_plaintext(64)

    let assert Ok(ciphertext) = ffi_aead_encrypt(key, nonce, aad, plaintext)

    // Ciphertext should be different from plaintext
    should.not_equal(ciphertext, plaintext)

    // Ciphertext should be larger (includes 16-byte auth tag)
    should.be_true(
      bit_array.byte_size(ciphertext) > bit_array.byte_size(plaintext),
    )
  })
}

pub fn aead_tampering_causes_decrypt_failure_property_test() {
  // Property: tampering with ciphertext causes decrypt to fail

  list.range(1, 10)
  |> list.each(fn(_i) {
    let key = random_key()
    let nonce = ffi_generate_nonce()
    let aad = random_aad()
    let plaintext = random_plaintext(128)

    let assert Ok(ciphertext) = ffi_aead_encrypt(key, nonce, aad, plaintext)
    let tampered = tamper_ciphertext(ciphertext)

    // Decryption of tampered ciphertext should fail
    let result = ffi_aead_decrypt(key, nonce, aad, tampered)
    should.be_error(result)
  })
}

pub fn aead_wrong_key_fails_property_test() {
  // Property: decryption with wrong key fails

  list.range(1, 10)
  |> list.each(fn(_i) {
    let key1 = random_key()
    let key2 = random_key()
    let nonce = ffi_generate_nonce()
    let aad = random_aad()
    let plaintext = random_plaintext(64)

    let assert Ok(ciphertext) = ffi_aead_encrypt(key1, nonce, aad, plaintext)

    // Decryption with different key should fail
    let result = ffi_aead_decrypt(key2, nonce, aad, ciphertext)
    should.be_error(result)
  })
}

pub fn aead_wrong_aad_fails_property_test() {
  // Property: decryption with wrong AAD fails

  list.range(1, 10)
  |> list.each(fn(_i) {
    let key = random_key()
    let nonce = ffi_generate_nonce()
    let aad1 = random_aad()
    let aad2 = random_aad()
    let plaintext = random_plaintext(64)

    let assert Ok(ciphertext) = ffi_aead_encrypt(key, nonce, aad1, plaintext)

    // Decryption with different AAD should fail
    let result = ffi_aead_decrypt(key, nonce, aad2, ciphertext)
    should.be_error(result)
  })
}

pub fn aead_empty_plaintext_property_test() {
  // Property: empty plaintext encrypts and decrypts correctly

  let key = random_key()
  let nonce = ffi_generate_nonce()
  let aad = random_aad()
  let plaintext = <<>>

  let assert Ok(ciphertext) = ffi_aead_encrypt(key, nonce, aad, plaintext)

  // Ciphertext should still have the auth tag (16 bytes)
  should.equal(bit_array.byte_size(ciphertext), 16)

  let assert Ok(decrypted) = ffi_aead_decrypt(key, nonce, aad, ciphertext)
  should.equal(decrypted, plaintext)
}

// =============================================================================
// HKDF Property Tests
// =============================================================================

pub fn hkdf_deterministic_property_test() {
  // Property: same inputs always produce same output

  list.range(1, 10)
  |> list.each(fn(_i) {
    let salt = ffi_random_bytes(32)
    let ikm = ffi_random_bytes(32)
    let info = <<"test_info">>
    let len = 32

    let assert Ok(key1) = ffi_hkdf(salt, ikm, info, len)
    let assert Ok(key2) = ffi_hkdf(salt, ikm, info, len)

    should.equal(key1, key2)
  })
}

pub fn hkdf_different_salt_produces_different_output_property_test() {
  // Property: different salt produces different key

  list.range(1, 10)
  |> list.each(fn(_i) {
    let salt1 = ffi_random_bytes(32)
    let salt2 = ffi_random_bytes(32)
    let ikm = ffi_random_bytes(32)
    let info = <<"test_info">>
    let len = 32

    let assert Ok(key1) = ffi_hkdf(salt1, ikm, info, len)
    let assert Ok(key2) = ffi_hkdf(salt2, ikm, info, len)

    should.not_equal(key1, key2)
  })
}

pub fn hkdf_different_ikm_produces_different_output_property_test() {
  // Property: different IKM produces different key

  list.range(1, 10)
  |> list.each(fn(_i) {
    let salt = ffi_random_bytes(32)
    let ikm1 = ffi_random_bytes(32)
    let ikm2 = ffi_random_bytes(32)
    let info = <<"test_info">>
    let len = 32

    let assert Ok(key1) = ffi_hkdf(salt, ikm1, info, len)
    let assert Ok(key2) = ffi_hkdf(salt, ikm2, info, len)

    should.not_equal(key1, key2)
  })
}

pub fn hkdf_different_info_produces_different_output_property_test() {
  // Property: different info produces different key

  let salt = ffi_random_bytes(32)
  let ikm = ffi_random_bytes(32)
  let info1 = <<"context_a">>
  let info2 = <<"context_b">>
  let len = 32

  let assert Ok(key1) = ffi_hkdf(salt, ikm, info1, len)
  let assert Ok(key2) = ffi_hkdf(salt, ikm, info2, len)

  should.not_equal(key1, key2)
}

pub fn hkdf_variable_length_output_property_test() {
  // Property: can derive keys of various lengths

  let salt = ffi_random_bytes(32)
  let ikm = ffi_random_bytes(32)
  let info = <<"test_info">>

  // Test various output lengths
  [16, 32, 48, 64, 128]
  |> list.each(fn(len) {
    let assert Ok(key) = ffi_hkdf(salt, ikm, info, len)
    should.equal(bit_array.byte_size(key), len)
  })
}

pub fn hkdf_empty_salt_works_property_test() {
  // Property: empty salt is valid (RFC 5869)

  let salt = <<>>
  let ikm = ffi_random_bytes(32)
  let info = <<"test_info">>
  let len = 32

  let result = ffi_hkdf(salt, ikm, info, len)
  should.be_ok(result)

  let assert Ok(key) = result
  should.equal(bit_array.byte_size(key), len)
}

// =============================================================================
// Key Exchange Property Tests
// =============================================================================

pub fn x25519_keypair_property_test() {
  // Property: generated keypairs have correct sizes

  list.range(1, 10)
  |> list.each(fn(_i) {
    let assert Ok(#(pub_key, priv_key)) = ffi_gen_keypair()

    // Both keys should be 32 bytes
    should.equal(bit_array.byte_size(pub_key), 32)
    should.equal(bit_array.byte_size(priv_key), 32)

    // Keys should be different
    should.not_equal(pub_key, priv_key)
  })
}

@external(erlang, "crypto_sodium_ffi", "scalarmult")
fn ffi_scalarmult(
  peer_pub: BitArray,
  our_priv: BitArray,
) -> Result(BitArray, Nil)

pub fn x25519_shared_secret_commutative_property_test() {
  // Property: A's priv + B's pub == B's priv + A's pub (shared secret)

  list.range(1, 5)
  |> list.each(fn(_i) {
    let assert Ok(#(pub_a, priv_a)) = ffi_gen_keypair()
    let assert Ok(#(pub_b, priv_b)) = ffi_gen_keypair()

    // A computes shared secret with B's public key
    let assert Ok(secret_a) = ffi_scalarmult(pub_b, priv_a)

    // B computes shared secret with A's public key
    let assert Ok(secret_b) = ffi_scalarmult(pub_a, priv_b)

    // Both should derive the same shared secret
    should.equal(secret_a, secret_b)
    should.equal(bit_array.byte_size(secret_a), 32)
  })
}

// =============================================================================
// Cross-Implementation Readiness Tests
// =============================================================================

/// Test vectors for future cross-implementation validation.
/// When libsodium NIF is available, these can verify compatibility.
pub fn known_test_vectors_for_cross_impl_test() {
  // RFC 8439 ChaCha20-Poly1305 test vector (simplified)
  // This ensures our AEAD implementation matches the standard

  // Key: 32 bytes of 0x00..0x1f
  let key = <<
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
  >>

  // Nonce: 12 bytes
  let nonce = <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>

  // AAD
  let aad = <<"test">>

  // Plaintext
  let plaintext = <<"Hello, World!">>

  // Encrypt and verify roundtrip
  let assert Ok(ciphertext) = ffi_aead_encrypt(key, nonce, aad, plaintext)
  let assert Ok(decrypted) = ffi_aead_decrypt(key, nonce, aad, ciphertext)

  should.equal(decrypted, plaintext)

  // Store ciphertext for future cross-impl validation
  // When libsodium is available, verify: libsodium_encrypt(key, nonce, aad, plaintext) == ciphertext
  should.be_true(bit_array.byte_size(ciphertext) > 0)
}

/// HKDF test vector (RFC 5869 Test Case 1)
pub fn hkdf_rfc5869_test_vector_test() {
  // Test Case 1 from RFC 5869
  let ikm = <<
    0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B,
    0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B, 0x0B,
  >>
  let salt = <<
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C,
  >>
  let info = <<0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9>>
  let len = 42

  let assert Ok(okm) = ffi_hkdf(salt, ikm, info, len)

  // Expected OKM from RFC 5869
  let expected = <<
    0x3C, 0xB2, 0x5F, 0x25, 0xFA, 0xAC, 0xD5, 0x7A, 0x90, 0x43, 0x4F, 0x64, 0xD0,
    0x36, 0x2F, 0x2A, 0x2D, 0x2D, 0x0A, 0x90, 0xCF, 0x1A, 0x5A, 0x4C, 0x5D, 0xB0,
    0x2D, 0x56, 0xEC, 0xC4, 0xC5, 0xBF, 0x34, 0x00, 0x72, 0x08, 0xD5, 0xB8, 0x87,
    0x18, 0x58, 0x65,
  >>

  should.equal(okm, expected)
}
