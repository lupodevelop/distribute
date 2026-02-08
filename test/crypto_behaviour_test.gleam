import distribute/crypto/behaviour.{
  type CryptoError, CryptoMetrics, DecryptionFailed, Degraded, Down,
  EncryptionFailed, Failed, HandshakeFailed, HandshakeMessage, InitFailed,
  InvalidSignature, KeyExchangeInProgress, KeyMismatch, NoSecureContext, Plain,
  ProviderFailure, ProviderOptions, RekeyFailed, Rekeying, SecureEstablished,
  ShutdownFailed, Timeout, TransientNetwork, Up,
}
import gleam/dict
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// SecureContext — opaque constructors + getters
// ============================================================================

pub fn new_secure_context_node_id_test() {
  let ctx =
    behaviour.new_secure_context(
      "node@host",
      SecureEstablished,
      1_000_000,
      "k1",
    )
  behaviour.context_node_id(ctx)
  |> should.equal("node@host")
}

pub fn new_secure_context_stage_test() {
  let ctx = behaviour.new_secure_context("n1", Plain, 0, "k0")
  behaviour.context_stage(ctx)
  |> should.equal(Plain)
}

pub fn new_secure_context_key_id_test() {
  let ctx =
    behaviour.new_secure_context("n1", SecureEstablished, 100, "key-abc-123")
  behaviour.context_key_id(ctx)
  |> should.equal("key-abc-123")
}

pub fn secure_context_with_failed_stage_test() {
  let ctx = behaviour.new_secure_context("n1", Failed("timeout"), 0, "k0")
  behaviour.context_stage(ctx)
  |> should.equal(Failed("timeout"))
}

pub fn secure_context_with_rekeying_stage_test() {
  let ctx = behaviour.new_secure_context("n1", Rekeying, 999, "k2")
  behaviour.context_stage(ctx)
  |> should.equal(Rekeying)
}

pub fn secure_context_with_key_exchange_stage_test() {
  let ctx = behaviour.new_secure_context("n1", KeyExchangeInProgress, 0, "k0")
  behaviour.context_stage(ctx)
  |> should.equal(KeyExchangeInProgress)
}

// ============================================================================
// HandshakeState — opaque constructors + getters
// ============================================================================

pub fn new_handshake_state_stage_test() {
  let state = behaviour.new_handshake_state("local", "remote", Plain)
  behaviour.handshake_stage(state)
  |> should.equal(Plain)
}

pub fn new_handshake_state_remote_node_test() {
  let state =
    behaviour.new_handshake_state("local@a", "remote@b", KeyExchangeInProgress)
  behaviour.handshake_remote_node(state)
  |> should.equal("remote@b")
}

pub fn handshake_state_established_test() {
  let state = behaviour.new_handshake_state("a", "b", SecureEstablished)
  behaviour.handshake_stage(state)
  |> should.equal(SecureEstablished)
}

pub fn handshake_state_failed_test() {
  let state = behaviour.new_handshake_state("a", "b", Failed("bad key"))
  behaviour.handshake_stage(state)
  |> should.equal(Failed("bad key"))
}

// ============================================================================
// HealthStatus — variant construction
// ============================================================================

pub fn health_up_test() {
  let h: behaviour.HealthStatus = Up
  should.equal(h, Up)
}

pub fn health_degraded_test() {
  let h: behaviour.HealthStatus = Degraded("slow")
  should.equal(h, Degraded("slow"))
}

pub fn health_down_test() {
  let h: behaviour.HealthStatus = Down("crashed")
  should.equal(h, Down("crashed"))
}

// ============================================================================
// CryptoMetrics — construction
// ============================================================================

pub fn crypto_metrics_zero_test() {
  let m =
    CryptoMetrics(
      handshakes_initiated: 0,
      handshakes_completed: 0,
      handshakes_failed: 0,
      encrypt_count: 0,
      decrypt_count: 0,
      rekey_count: 0,
      active_contexts: 0,
    )
  should.equal(m.handshakes_initiated, 0)
  should.equal(m.active_contexts, 0)
}

pub fn crypto_metrics_with_values_test() {
  let m =
    CryptoMetrics(
      handshakes_initiated: 10,
      handshakes_completed: 8,
      handshakes_failed: 2,
      encrypt_count: 1000,
      decrypt_count: 999,
      rekey_count: 3,
      active_contexts: 5,
    )
  should.equal(m.handshakes_completed, 8)
  should.equal(m.encrypt_count, 1000)
  should.equal(m.rekey_count, 3)
}

// ============================================================================
// Error classification — is_transient_error
// ============================================================================

pub fn transient_network_is_transient_test() {
  behaviour.is_transient_error(TransientNetwork("conn reset"))
  |> should.be_true()
}

pub fn timeout_is_transient_test() {
  behaviour.is_transient_error(Timeout(5000))
  |> should.be_true()
}

pub fn init_failed_is_not_transient_test() {
  behaviour.is_transient_error(InitFailed("bad config"))
  |> should.be_false()
}

pub fn invalid_signature_is_not_transient_test() {
  behaviour.is_transient_error(InvalidSignature)
  |> should.be_false()
}

pub fn key_mismatch_is_not_transient_test() {
  behaviour.is_transient_error(KeyMismatch)
  |> should.be_false()
}

pub fn decryption_failed_is_not_transient_test() {
  behaviour.is_transient_error(DecryptionFailed("bad padding"))
  |> should.be_false()
}

pub fn encryption_failed_is_not_transient_test() {
  behaviour.is_transient_error(EncryptionFailed("key too short"))
  |> should.be_false()
}

pub fn no_secure_context_is_not_transient_test() {
  behaviour.is_transient_error(NoSecureContext("node@x"))
  |> should.be_false()
}

pub fn handshake_failed_is_not_transient_test() {
  behaviour.is_transient_error(HandshakeFailed("rejected"))
  |> should.be_false()
}

pub fn rekey_failed_is_not_transient_test() {
  behaviour.is_transient_error(RekeyFailed("stale context"))
  |> should.be_false()
}

pub fn provider_failure_is_not_transient_test() {
  behaviour.is_transient_error(ProviderFailure("crash"))
  |> should.be_false()
}

pub fn shutdown_failed_is_not_transient_test() {
  behaviour.is_transient_error(ShutdownFailed("timeout"))
  |> should.be_false()
}

// ============================================================================
// Error classification — is_permanent_error
// ============================================================================

pub fn invalid_signature_is_permanent_test() {
  behaviour.is_permanent_error(InvalidSignature)
  |> should.be_true()
}

pub fn key_mismatch_is_permanent_test() {
  behaviour.is_permanent_error(KeyMismatch)
  |> should.be_true()
}

pub fn decryption_failed_is_permanent_test() {
  behaviour.is_permanent_error(DecryptionFailed("corrupt"))
  |> should.be_true()
}

pub fn init_failed_is_permanent_test() {
  behaviour.is_permanent_error(InitFailed("missing lib"))
  |> should.be_true()
}

pub fn transient_network_is_not_permanent_test() {
  behaviour.is_permanent_error(TransientNetwork("retry"))
  |> should.be_false()
}

pub fn timeout_is_not_permanent_test() {
  behaviour.is_permanent_error(Timeout(3000))
  |> should.be_false()
}

pub fn shutdown_failed_is_not_permanent_test() {
  behaviour.is_permanent_error(ShutdownFailed("stuck"))
  |> should.be_false()
}

pub fn provider_failure_is_not_permanent_test() {
  behaviour.is_permanent_error(ProviderFailure("internal"))
  |> should.be_false()
}

pub fn no_secure_context_is_not_permanent_test() {
  behaviour.is_permanent_error(NoSecureContext("n"))
  |> should.be_false()
}

pub fn handshake_failed_is_not_permanent_test() {
  behaviour.is_permanent_error(HandshakeFailed("err"))
  |> should.be_false()
}

pub fn rekey_failed_is_not_permanent_test() {
  behaviour.is_permanent_error(RekeyFailed("err"))
  |> should.be_false()
}

pub fn encryption_failed_is_not_permanent_test() {
  behaviour.is_permanent_error(EncryptionFailed("err"))
  |> should.be_false()
}

// ============================================================================
// Error mutually exclusive: no error should be both transient AND permanent
// ============================================================================

pub fn transient_and_permanent_mutually_exclusive_test() {
  let errors: List(CryptoError) = [
    InitFailed(""),
    ShutdownFailed(""),
    TransientNetwork(""),
    InvalidSignature,
    KeyMismatch,
    DecryptionFailed(""),
    EncryptionFailed(""),
    NoSecureContext(""),
    HandshakeFailed(""),
    RekeyFailed(""),
    ProviderFailure(""),
    Timeout(0),
  ]
  list_check_exclusive(errors)
}

fn list_check_exclusive(errors: List(CryptoError)) -> Nil {
  case errors {
    [] -> Nil
    [err, ..rest] -> {
      let t = behaviour.is_transient_error(err)
      let p = behaviour.is_permanent_error(err)
      // Cannot be both transient AND permanent
      case t, p {
        True, True -> should.fail()
        _, _ -> Nil
      }
      list_check_exclusive(rest)
    }
  }
}

// ============================================================================
// default_options
// ============================================================================

pub fn default_options_name_test() {
  let opts = behaviour.default_options("my_provider")
  should.equal(opts.name, "my_provider")
}

pub fn default_options_is_not_development_test() {
  let opts = behaviour.default_options("p")
  should.equal(opts.is_development, False)
}

pub fn default_options_key_rotation_disabled_test() {
  let opts = behaviour.default_options("p")
  should.equal(opts.key_rotation_interval_ms, 0)
}

pub fn default_options_handshake_timeout_test() {
  let opts = behaviour.default_options("p")
  should.equal(opts.handshake_timeout_ms, 30_000)
}

pub fn default_options_custom_empty_test() {
  let opts = behaviour.default_options("p")
  should.equal(dict.size(opts.custom), 0)
}

// ============================================================================
// HandshakeMessage — construction
// ============================================================================

pub fn handshake_message_construction_test() {
  let msg =
    HandshakeMessage(
      message_type: "key_exchange_init",
      payload: <<"pubkey":utf8>>,
      metadata: None,
    )
  should.equal(msg.message_type, "key_exchange_init")
  should.equal(msg.payload, <<"pubkey":utf8>>)
  should.equal(msg.metadata, None)
}

pub fn handshake_message_with_metadata_test() {
  let meta = dict.from_list([#("version", "1.0")])
  let msg =
    HandshakeMessage(
      message_type: "key_exchange_resp",
      payload: <<>>,
      metadata: Some(meta),
    )
  should.equal(msg.metadata, Some(meta))
}

// ============================================================================
// ProviderOptions — construction
// ============================================================================

pub fn provider_options_custom_construction_test() {
  let opts =
    ProviderOptions(
      name: "custom",
      is_development: True,
      key_rotation_interval_ms: 60_000,
      handshake_timeout_ms: 10_000,
      custom: dict.from_list([#("cipher", "aes-256-gcm")]),
    )
  should.equal(opts.is_development, True)
  should.equal(opts.key_rotation_interval_ms, 60_000)
  should.equal(dict.get(opts.custom, "cipher"), Ok("aes-256-gcm"))
}
