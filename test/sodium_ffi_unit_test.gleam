//// Unit tests for the Erlang crypto FFI (`crypto_sodium_ffi`).
////
//// These tests exercise edge cases and fixed vectors (RFC 5869 HKDF vector)
//// and validate basic properties for X25519 and AEAD operations.

import gleam/bit_array
import gleeunit/should

// External bindings (mirror functions in `crypto_sodium_ffi.erl`)
@external(erlang, "crypto_sodium_ffi", "gen_keypair")
fn ffi_gen_keypair() -> Result(#(BitArray, BitArray), Nil)

@external(erlang, "crypto_sodium_ffi", "scalarmult")
fn ffi_scalarmult(
  peer_pub: BitArray,
  our_priv: BitArray,
) -> Result(BitArray, Nil)

@external(erlang, "crypto_sodium_ffi", "hkdf")
fn ffi_hkdf(
  salt: BitArray,
  ikm: BitArray,
  info: BitArray,
  len: Int,
) -> Result(BitArray, Nil)

@external(erlang, "crypto_sodium_ffi", "generate_nonce")
fn ffi_generate_nonce() -> BitArray

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

// -----------------------------------------------------------------------------
// HKDF fixed vector test (RFC 5869 - Test case 1)
// IKM: 22 bytes of 0x0b
// Salt: 0x000102030405060708090a0b0c
// Info: 0xf0f1f2f3f4f5f6f7f8f9
// L = 42
// Expected OKM (hex):
// 3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865

pub fn hkdf_rfc5869_vector1_test() {
  // Build inputs
  let ikm = <<
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
    11,
  >>
  let salt = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12>>
  let info = <<0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9>>
  let expected = <<
    0x3c,
    0xb2,
    0x5f,
    0x25,
    0xfa,
    0xac,
    0xd5,
    0x7a,
    0x90,
    0x43,
    0x4f,
    0x64,
    0xd0,
    0x36,
    0x2f,
    0x2a,
    0x2d,
    0x2d,
    0x0a,
    0x90,
    0xcf,
    0x1a,
    0x5a,
    0x4c,
    0x5d,
    0xb0,
    0x2d,
    0x56,
    0xec,
    0xc4,
    0xc5,
    0xbf,
    0x34,
    0x00,
    0x72,
    0x08,
    0xd5,
    0xb8,
    0x87,
    0x18,
    0x58,
    0x65,
  >>

  case ffi_hkdf(salt, ikm, info, 42) {
    Ok(okm) -> should.equal(okm, expected)
    Error(_) -> panic as "HKDF returned error"
  }
}

// -----------------------------------------------------------------------------
// X25519 scalarmult property: shared secrets equal

pub fn scalarmult_shared_secret_equality_test() {
  case ffi_gen_keypair() {
    Ok(#(pub_key_a, priv_key_a)) -> {
      case ffi_gen_keypair() {
        Ok(#(pub_key_b, priv_key_b)) -> {
          case ffi_scalarmult(pub_key_b, priv_key_a) {
            Ok(shared1) -> {
              case ffi_scalarmult(pub_key_a, priv_key_b) {
                Ok(shared2) -> should.equal(shared1, shared2)
                Error(_) -> panic as "scalarmult failed (shared2)"
              }
            }
            Error(_) -> panic as "scalarmult failed (shared1)"
          }
        }
        Error(_) -> panic as "gen_keypair failed for B"
      }
    }
    Error(_) -> panic as "gen_keypair failed for A"
  }
}

// -----------------------------------------------------------------------------
// AEAD encrypt/decrypt and wrong-key/tamper checks

pub fn aead_encrypt_decrypt_and_failure_modes_test() {
  let ikm = <<"unit-test-ikm">>
  let salt = <<>>
  let info = <<"aead-test">>

  // Derive a 32-byte AEAD key
  case ffi_hkdf(salt, ikm, info, 32) {
    Ok(aead_key) -> {
      let nonce = ffi_generate_nonce()
      // nonce length sanity check (expect 24 for XChaCha or >=12)
      should.be_true(bit_array.byte_size(nonce) >= 12)
      let aad = <<>>
      let plaintext = <<"hello ffi aead">>

      case ffi_aead_encrypt(aead_key, nonce, aad, plaintext) {
        Ok(ciphertext) -> {
          // Successful decrypt with same key
          case ffi_aead_decrypt(aead_key, nonce, aad, ciphertext) {
            Ok(pt) -> should.equal(pt, plaintext)
            Error(_) -> panic as "decrypt failed (expected success)"
          }

          // Derive a different key and expect decrypt to fail
          case ffi_hkdf(salt, <<"other-ikm">>, info, 32) {
            Ok(wrong_key) -> {
              case ffi_aead_decrypt(wrong_key, nonce, aad, ciphertext) {
                Ok(_) -> panic as "decrypt succeeded with wrong key"
                Error(_) -> Nil
              }
            }
            Error(_) -> panic as "hkdf failed for wrong key"
          }
        }
        Error(_) -> panic as "aead_encrypt failed"
      }
    }
    Error(_) -> panic as "hkdf failed"
  }
}

// -----------------------------------------------------------------------------
// Small additional checks: key pair size, nonce size

pub fn gen_keypair_sizes_test() {
  case ffi_gen_keypair() {
    Ok(#(pub_key, priv_key)) -> {
      should.equal(bit_array.byte_size(pub_key), 32)
      should.equal(bit_array.byte_size(priv_key), 32)
    }
    Error(_) -> panic as "gen_keypair failed"
  }
}

pub fn nonce_size_test() {
  let n = ffi_generate_nonce()
  should.be_true(bit_array.byte_size(n) >= 12)
}

// -----------------------------------------------------------------------------
// Invalid input cases

pub fn aead_decrypt_invalid_key_size_test() {
  // Prepare invalid key
  let invalid_key = <<1, 2, 3, 4>>
  let nonce = ffi_generate_nonce()
  let aad = <<>>
  // ciphertext arbitrary (too short will also be error)
  let ciphertext = <<1, 2, 3, 4, 5, 6>>

  case ffi_aead_decrypt(invalid_key, nonce, aad, ciphertext) {
    Ok(_) -> panic as "decrypt unexpectedly succeeded with invalid key size"
    Error(_) -> Nil
  }
}

// -----------------------------------------------------------------------------
// Additional HKDF and AEAD edge-case tests

pub fn hkdf_length_and_info_variation_test() {
  let ikm = <<1, 2, 3, 4, 5, 6>>
  let salt = <<>>

  case ffi_hkdf(salt, ikm, <<"info1">>, 16) {
    Ok(a) -> {
      case ffi_hkdf(salt, ikm, <<"info2">>, 32) {
        Ok(b) -> {
          should.equal(bit_array.byte_size(a), 16)
          should.equal(bit_array.byte_size(b), 32)
          should.not_equal(a, b)
        }
        Error(_) -> panic as "hkdf failed for len 32"
      }
    }
    Error(_) -> panic as "hkdf failed for len 16"
  }
}

pub fn aead_zero_length_plaintext_test() {
  let ikm = <<"zero-ikm">>
  let salt = <<>>
  let info = <<"zero">>

  case ffi_hkdf(salt, ikm, info, 32) {
    Ok(aead_key) -> {
      let nonce = ffi_generate_nonce()
      let aad = <<>>
      let plaintext = <<>>

      case ffi_aead_encrypt(aead_key, nonce, aad, plaintext) {
        Ok(ciphertext) -> {
          case ffi_aead_decrypt(aead_key, nonce, aad, ciphertext) {
            Ok(pt) -> should.equal(pt, plaintext)
            Error(_) -> panic as "decrypt failed for zero-length plaintext"
          }
        }
        Error(_) -> panic as "aead_encrypt failed for zero-length plaintext"
      }
    }
    Error(_) -> panic as "hkdf failed"
  }
}

pub fn aead_tamper_and_short_ciphertext_test() {
  let ikm = <<"tamper-ikm">>
  let salt = <<>>
  let info = <<"tamper">>

  case ffi_hkdf(salt, ikm, info, 32) {
    Ok(aead_key) -> {
      let nonce = ffi_generate_nonce()
      let aad = <<>>
      let plaintext = <<"sensitive">>

      case ffi_aead_encrypt(aead_key, nonce, aad, plaintext) {
        Ok(ciphertext) -> {
          // Tamper: remove last byte (short ciphertext)
          let len = bit_array.byte_size(ciphertext)
          case bit_array.slice(ciphertext, 0, len - 1) {
            Ok(short_ct) -> {
              case ffi_aead_decrypt(aead_key, nonce, aad, short_ct) {
                Ok(_) -> panic as "decrypt succeeded on short ciphertext"
                Error(_) -> Nil
              }
            }
            Error(_) -> panic as "failed to slice ciphertext"
          }

          // Tamper: append extra byte (invalid tag)
          let tampered = bit_array.concat([ciphertext, <<1>>])
          case ffi_aead_decrypt(aead_key, nonce, aad, tampered) {
            Ok(_) -> panic as "decrypt succeeded on tampered ciphertext"
            Error(_) -> Nil
          }
        }
        Error(_) -> panic as "aead_encrypt failed"
      }
    }
    Error(_) -> panic as "hkdf failed"
  }
}
