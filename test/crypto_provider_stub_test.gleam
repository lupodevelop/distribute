import distribute/crypto/provider
import distribute/crypto/provider_stub
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Provider stub initialization
// ============================================================================

pub fn stub_init_test() {
  let state = provider_stub.init()
  should.equal(state, provider.ProviderState)
}

// ============================================================================
// Key exchange stub
// ============================================================================

pub fn stub_start_key_exchange_echos_params_test() {
  let state = provider_stub.init()
  let params = <<"local_params":utf8>>
  let #(outgoing, new_state) = provider_stub.start_key_exchange(state, params)
  // Stub echoes params as outgoing message
  should.equal(outgoing, params)
  should.equal(new_state, provider.ProviderState)
}

pub fn stub_handle_key_exchange_produces_context_test() {
  let state = provider_stub.init()
  let #(response, _new_state, context) =
    provider_stub.handle_key_exchange(state, <<"remote_msg":utf8>>)
  // No further message needed
  should.equal(response, None)
  // Context produced immediately
  should.equal(context, Some(provider.SecureContext))
}

// ============================================================================
// Encrypt/Decrypt (identity stubs)
// ============================================================================

pub fn stub_encrypt_returns_plaintext_test() {
  let plain = <<"secret data":utf8>>
  let result = provider_stub.encrypt(provider.SecureContext, plain)
  should.equal(result, Ok(plain))
}

pub fn stub_decrypt_returns_ciphertext_test() {
  let cipher = <<"encrypted data":utf8>>
  let result = provider_stub.decrypt(provider.SecureContext, cipher)
  should.equal(result, Ok(cipher))
}

pub fn stub_encrypt_decrypt_roundtrip_test() {
  let plain = <<"roundtrip data":utf8>>
  let assert Ok(encrypted) =
    provider_stub.encrypt(provider.SecureContext, plain)
  let assert Ok(decrypted) =
    provider_stub.decrypt(provider.SecureContext, encrypted)
  should.equal(decrypted, plain)
}

pub fn stub_encrypt_empty_data_test() {
  let result = provider_stub.encrypt(provider.SecureContext, <<>>)
  should.equal(result, Ok(<<>>))
}

// ============================================================================
// Rekey stub
// ============================================================================

pub fn stub_rekey_returns_same_context_test() {
  let result = provider_stub.rekey(provider.SecureContext)
  should.equal(result, Ok(provider.SecureContext))
}

// ============================================================================
// Close stub
// ============================================================================

pub fn stub_close_succeeds_test() {
  let state = provider_stub.init()
  let result = provider_stub.close(state)
  should.equal(result, Nil)
}
