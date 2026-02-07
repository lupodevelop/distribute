import distribute/crypto/provider
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// init — returns ProviderState
// ============================================================================

pub fn init_returns_provider_state_test() {
  let state = provider.init()
  should.equal(state, provider.ProviderState)
}

// ============================================================================
// start_key_exchange — echoes local params
// ============================================================================

pub fn start_key_exchange_echoes_params_test() {
  let state = provider.init()
  let params = <<"my_local_params":utf8>>
  let #(outgoing, new_state) = provider.start_key_exchange(state, params)
  should.equal(outgoing, params)
  should.equal(new_state, provider.ProviderState)
}

pub fn start_key_exchange_empty_params_test() {
  let state = provider.init()
  let #(outgoing, new_state) = provider.start_key_exchange(state, <<>>)
  should.equal(outgoing, <<>>)
  should.equal(new_state, provider.ProviderState)
}

pub fn start_key_exchange_large_params_test() {
  let state = provider.init()
  let large = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa":utf8>>
  let #(outgoing, _) = provider.start_key_exchange(state, large)
  should.equal(outgoing, large)
}

// ============================================================================
// handle_key_exchange — returns (None, state, None)
// ============================================================================

pub fn handle_key_exchange_noop_test() {
  let state = provider.init()
  let #(response, new_state, context) =
    provider.handle_key_exchange(state, <<"incoming":utf8>>)
  should.equal(response, None)
  should.equal(new_state, provider.ProviderState)
  should.equal(context, None)
}

pub fn handle_key_exchange_empty_incoming_test() {
  let state = provider.init()
  let #(response, _, context) = provider.handle_key_exchange(state, <<>>)
  should.equal(response, None)
  should.equal(context, None)
}

// ============================================================================
// encrypt — stub (Error)
// ============================================================================

pub fn encrypt_returns_not_implemented_test() {
  let result = provider.encrypt(provider.SecureContext, <<"data":utf8>>)
  should.be_error(result)
}

pub fn encrypt_error_message_test() {
  let assert Error(msg) =
    provider.encrypt(provider.SecureContext, <<"data":utf8>>)
  should.equal(msg, "not implemented")
}

pub fn encrypt_empty_data_error_test() {
  let result = provider.encrypt(provider.SecureContext, <<>>)
  should.be_error(result)
}

// ============================================================================
// decrypt — stub (Error)
// ============================================================================

pub fn decrypt_returns_not_implemented_test() {
  let result = provider.decrypt(provider.SecureContext, <<"cipher":utf8>>)
  should.be_error(result)
}

pub fn decrypt_error_message_test() {
  let assert Error(msg) =
    provider.decrypt(provider.SecureContext, <<"cipher":utf8>>)
  should.equal(msg, "not implemented")
}

// ============================================================================
// rekey — stub (Error)
// ============================================================================

pub fn rekey_returns_not_implemented_test() {
  let result = provider.rekey(provider.SecureContext)
  should.be_error(result)
}

pub fn rekey_error_message_test() {
  let assert Error(msg) = provider.rekey(provider.SecureContext)
  should.equal(msg, "not implemented")
}

// ============================================================================
// close — no crash
// ============================================================================

pub fn close_returns_nil_test() {
  let result = provider.close(provider.ProviderState)
  should.equal(result, Nil)
}

// ============================================================================
// Full lifecycle sequence
// ============================================================================

pub fn full_lifecycle_no_crash_test() {
  // Init → key exchange → handle → encrypt attempt → close
  let state = provider.init()
  let #(_, state2) = provider.start_key_exchange(state, <<"init":utf8>>)
  let #(_, state3, _) = provider.handle_key_exchange(state2, <<"resp":utf8>>)
  // encrypt/decrypt are stubs — should not crash
  let _ = provider.encrypt(provider.SecureContext, <<"test":utf8>>)
  let _ = provider.decrypt(provider.SecureContext, <<"test":utf8>>)
  let _ = provider.rekey(provider.SecureContext)
  provider.close(state3)
  |> should.equal(Nil)
}
