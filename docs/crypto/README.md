# Crypto Documentation

## Overview

The crypto system in distribute provides **encrypted communication** between nodes. Every message crossing the cluster can be encrypted using ChaCha20-Poly1305 AEAD.

### Key Features

- ✅ X25519 key exchange (32-byte Curve25519)
- ✅ ChaCha20-Poly1305 AEAD encryption (RFC 8439, **12-byte nonces**)
- ✅ HKDF-SHA256 key derivation
- ✅ Rekey support for forward secrecy
- ✅ No external dependencies (uses OTP `:crypto`)
- ✅ 13 comprehensive tests

### ⚠️ Nonce Policy

This implementation uses **12-byte nonces** as specified in RFC 8439 (standard ChaCha20-Poly1305).

**XChaCha20-Poly1305 (24-byte extended nonces) is NOT supported.**

Random nonces are generated per encryption, which is safe for typical message volumes. The birthday bound for 12-byte nonces is approximately 2^48 messages per key, which is sufficient for most use cases.

---

## Quick Start

### Enable Crypto in Your Application

```gleam
import distribute/crypto/otp_crypto_adapter
import distribute/registry

// Create the crypto adapter (uses OTP :crypto, no external deps)
let crypto_adapter = otp_crypto_adapter.new()

// Initialize it
case crypto_adapter.init(
  adapter.development_options("my-node-crypto")
) {
  Ok(handle) -> {
    logger.info("Crypto enabled")
    // Use the handle for encryption/decryption
    Ok(handle)
  }
  Error(e) -> {
    logger.error("Crypto init failed: " <> string.inspect(e))
    Error(e)
  }
}
```

### Encrypt a Message

```gleam
import distribute/crypto/types

// Establish a context with a peer
let context = types.new_secure_context(
  "remote_node@host",
  types.SecureEstablished,
  erlang.system_time_nanoseconds() / 1_000_000,
  key_id,
  wrapped_key_material,
)

// Encrypt
let plaintext = <<"secret data">>
case crypto_adapter.encrypt(handle, context, plaintext) {
  Ok(ciphertext) -> {
    logger.info("Encrypted: " <> bit_array.byte_size(ciphertext) <> " bytes")
    Ok(ciphertext)
  }
  Error(e) -> {
    logger.error("Encryption failed: " <> string.inspect(e))
    Error(e)
  }
}
```

### Decrypt a Message

```gleam
// Decrypt
case crypto_adapter.decrypt(handle, context, ciphertext) {
  Ok(plaintext) -> {
    logger.info("Decrypted: " <> bit_array.to_string(plaintext))
    Ok(plaintext)
  }
  Error(e) -> {
    logger.error("Decryption failed: " <> string.inspect(e))
    Error(e)
  }
}
```

---

## Architecture

### Adapter Pattern

Crypto is pluggable via adapters. Each adapter implements the `CryptoAdapter` behaviour:

```gleam
pub type CryptoAdapter {
  CryptoAdapter(
    // Initialize adapter with options
    init: fn(CryptoOptions) -> Result(ProviderHandle, CryptoError),
    
    // Start key exchange (initiator or responder)
    handshake_start: fn(
      ProviderHandle,
      NodeId,
      NodeId,
      Option(HandshakeMessage),
    ) -> Result(HandshakeResult, CryptoError),
    
    // Continue key exchange with peer message
    handshake_continue: fn(
      ProviderHandle,
      HandshakeState,
      HandshakeMessage,
    ) -> Result(HandshakeResult, CryptoError),
    
    // Get established context for a peer
    secure_context: fn(
      ProviderHandle,
      NodeId,
    ) -> Option(SecureContext),
    
    // Encrypt a message
    encrypt: fn(
      ProviderHandle,
      SecureContext,
      BitArray,
    ) -> Result(BitArray, CryptoError),
    
    // Decrypt a message
    decrypt: fn(
      ProviderHandle,
      SecureContext,
      BitArray,
    ) -> Result(BitArray, CryptoError),
    
    // Rekey for forward secrecy
    rekey: fn(
      ProviderHandle,
      NodeId,
    ) -> Result(Nil, CryptoError),
    
    // Health check
    health: fn(ProviderHandle) -> HealthStatus,
    
    // Metrics and observability
    metrics: fn(ProviderHandle) -> CryptoMetrics,
    
    // Shutdown
    shutdown: fn(ProviderHandle) -> Result(Nil, CryptoError),
  )
}
```

### Available Adapters

| Adapter | Algorithm | Status | FFI | Notes |
|---------|-----------|--------|-----|-------|
| `noop_adapter` | None (no encryption) | ✅ Complete | No | Testing only |
| `otp_crypto_adapter` | ChaCha20-Poly1305 + X25519 | ✅ Complete | OTP `:crypto` | **Recommended** |
| `sodium_adapter` | ChaCha20-Poly1305 + X25519 | ⚠️ Stub | libsodium | Future (v3.x) |

---

## OTP Crypto Adapter

### Overview

The **otp_crypto_adapter** is the production-ready encryption backend using Erlang's built-in `:crypto` module.

**Why this choice?**

| Aspect | OTP `:crypto` | libsodium NIF |
|--------|---------------|---------------|
| **Availability** | Built-in OTP 22+ | External dependency |
| **Security** | ✅ OpenSSL/LibreSSL | ✅ High (audited) |
| **Performance** | ✅ Fast | ✅ Very fast |
| **Compilation** | ✅ Zero setup | ⚠️ CI/CD complexity |
| **Cross-platform** | ✅ Yes | ⚠️ Requires builds |
| **Maintenance** | ✅ Easy | ⚠️ Binary management |

**Decision:** Use OTP for v2.x (zero friction). Evaluate libsodium for v3.x if performance is critical.

### Algorithms

**Key Exchange:** X25519 (Curve25519 ECDH)
```
- 32-byte public key
- 32-byte secret key
- Creates 32-byte shared secret
- Forward compatible
```

**Encryption:** ChaCha20-Poly1305 (RFC 8439)
```
- 256-bit key
- 12-byte nonce (randomly generated per RFC 8439)
- 16-byte authentication tag
- AEAD (Authenticated Encryption with Associated Data)
```

**Key Derivation:** HKDF-SHA256 (RFC 5869)
```
- Derives encryption key from shared secret
- Deterministic
- Domain-separated per node pair
```

### Handshake Flow

```
Node A                          Node B
  |                               |
  | 1. Start handshake            |
  |    (generate ephemeral key)   |
  |                               |
  | 2. Send pubkey_A -----------> |
  |                               |
  |    3. Start handshake         |
  |       (generate ephemeral key)|
  |                               |
  |    4. Compute shared_secret   |
  |       (ECDH with pubkey_A)    |
  |                               |
  | <---------- Send pubkey_B     |
  |             + derive keys     |
  |                               |
  | 5. Compute shared_secret      |
  |    (ECDH with pubkey_B)       |
  |                               |
  | 6. Derive keys                |
  |                               |
  | 7. Ready to encrypt/decrypt   | 8. Ready to encrypt/decrypt
```

### Testing

The adapter is fully tested (13 tests):

```gleam
test/otp_crypto_adapter_test.gleam

- Lifecycle (init/shutdown)
- Handshake (initiator/responder/full flow)
- Encrypt/decrypt roundtrip
- Empty and large messages (64KB)
- Tamper detection
- Rekey functionality
- Metrics tracking
```

Run with:
```bash
gleam test --target erlang
```

---

## Handshake State Machine

### Initiator

```
None
  ↓ (HandshakeStart with None)
KeyExchangeInProgress (send pubkey_A)
  ↓ (HandshakeContinue with pubkey_B)
SecureEstablished (context ready)
```

### Responder

```
None
  ↓ (HandshakeStart with pubkey_A)
KeyExchangeInProgress (send pubkey_B)
  ↓ (optional additional exchange)
SecureEstablished (context ready)
```

---

## Patterns

### Pattern 1: Enable Crypto at Startup

```gleam
import distribute/crypto/otp_crypto_adapter
import distribute/registry

pub fn start_with_crypto() -> Result(crypto.ProviderHandle, String) {
  let adapter = otp_crypto_adapter.new()
  let options = crypto.development_options("my-node")
  
  case adapter.init(options) {
    Ok(handle) -> {
      logger.info("Crypto enabled")
      Ok(handle)
    }
    Error(e) -> {
      logger.error("Crypto init failed: " <> string.inspect(e))
      Error("Crypto initialization failed")
    }
  }
}
```

### Pattern 2: Rekey on Interval

```gleam
import distribute/crypto/otp_crypto_adapter

pub fn rekey_all_peers(handle: crypto.ProviderHandle, peers: List(String)) {
  list.each(peers, fn(peer) {
    case otp_crypto_adapter.rekey(handle, peer) {
      Ok(Nil) -> logger.info("Rekeyed: " <> peer)
      Error(e) -> logger.warn("Rekey failed: " <> string.inspect(e))
    }
  })
}
```

### Pattern 3: Monitor Crypto Health

```gleam
import distribute/crypto/otp_crypto_adapter

pub fn check_crypto_health(handle: crypto.ProviderHandle) -> Bool {
  let health = otp_crypto_adapter.health(handle)
  case health {
    crypto.Up -> True
    crypto.Down(reason) -> {
      logger.error("Crypto down: " <> reason)
      False
    }
  }
}

pub fn log_crypto_metrics(handle: crypto.ProviderHandle) {
  let metrics = otp_crypto_adapter.metrics(handle)
  
  logger.info("Crypto metrics:")
  logger.info("  Handshakes: " <> int.to_string(metrics.handshakes_completed))
  logger.info("  Encrypt ops: " <> int.to_string(metrics.encrypt_count))
  logger.info("  Decrypt ops: " <> int.to_string(metrics.decrypt_count))
  logger.info("  Active contexts: " <> int.to_string(metrics.active_contexts))
}
```

### Pattern 4: Error Recovery

```gleam
import distribute/crypto/otp_crypto_adapter

pub fn encrypt_with_fallback(
  handle: crypto.ProviderHandle,
  context: crypto.SecureContext,
  plaintext: BitArray,
) -> Result(BitArray, String) {
  case otp_crypto_adapter.encrypt(handle, context, plaintext) {
    Ok(ciphertext) -> Ok(ciphertext)
    Error(crypto.InvalidKeySize) -> {
      logger.error("Invalid key size, rekeying...")
      // Try to rekey and retry
      case otp_crypto_adapter.rekey(handle, crypto.context_node_id(context)) {
        Ok(Nil) -> {
          // Try again after rekey
          otp_crypto_adapter.encrypt(handle, context, plaintext)
          |> result.map_error(fn(e) { "Encrypt after rekey failed: " <> string.inspect(e) })
        }
        Error(e) -> Error("Rekey failed: " <> string.inspect(e))
      }
    }
    Error(e) -> Error("Encryption failed: " <> string.inspect(e))
  }
}
```

---

## Error Handling

### CryptoError Types

```gleam
pub type CryptoError {
  InvalidKeySize
  InvalidNonce
  DecryptFailed
  HandshakeFailed(String)
  KeyDerivationFailed
  NoActiveContext(String)
  ContextExpired
  RekeyFailed(String)
  InitializationFailed(String)
  Timeout(Int)
}
```

### Recovery Strategies

| Error | Meaning | Recovery |
|-------|---------|----------|
| `InvalidKeySize` | Key material corrupted | Rekey |
| `DecryptFailed` | Tampered ciphertext | Log, reject message |
| `HandshakeFailed` | Key exchange failed | Retry handshake |
| `NoActiveContext` | Context not established | Initiate handshake |
| `ContextExpired` | Context too old | Rekey |
| `Timeout` | Operation timed out | Retry with backoff |

---

## Security Considerations

### Key Material

- **Never logged:** Keys are opaque, not shown in debug output
- **Cleared on shutdown:** Memory is reclaimed by GC (best effort)
- **Per-peer contexts:** Each peer has independent key material
- **Key IDs:** Opaque identifiers rotate on rekey

### Nonce Handling

- **Random generation:** 12-byte nonces from `crypto:strong_rand_bytes` (RFC 8439)
- **Never reused:** Each encryption gets a fresh nonce
- **Birthday bound:** ~2^48 messages per key before collision risk (sufficient for typical use)

### Forward Secrecy

- **Rekey on interval:** Rotate keys every N messages or time period
- **Invalidates old ciphertexts:** Can't decrypt old messages with new keys
- **Mitigates compromise:** If a key is leaked, only current messages are at risk

---

## Troubleshooting

### Crypto Init Fails

```gleam
case adapter.init(options) {
  Error(crypto.InitializationFailed(msg)) -> {
    // Likely: OTP :crypto module not available
    // Check: erlang:crypto:supports() in erl shell
  }
  Error(e) -> {
    logger.error("Init failed: " <> string.inspect(e))
  }
}
```

### Decrypt Fails After Rekey

```gleam
// Old context still cached?
case adapter.decrypt(handle, old_context, ciphertext) {
  Error(crypto.DecryptFailed) -> {
    logger.info("Old context failed, trying current...")
    case adapter.secure_context(handle, node_id) {
      Some(new_context) -> adapter.decrypt(handle, new_context, ciphertext)
      None -> Error("No context available")
    }
  }
}
```

### Handshake Timeout

```gleam
case adapter.handshake_start(handle, local, remote, msg) {
  Error(crypto.Timeout(ms)) -> {
    logger.warn("Handshake timeout after " <> int.to_string(ms) <> "ms")
    // Retry with backoff
    import distribute/retry
    retry.execute_with_strategy(retry.aggressive(), fn() {
      adapter.handshake_start(handle, local, remote, msg)
      |> result.map_error(fn(_) { Nil })
    })
  }
}
```

---

## Performance

### Throughput

- **Encryption:** ~1 Gbps (hardware-dependent)
- **Handshake:** <100ms (network-dependent)
- **Rekey:** <10ms (CPU-bound)

### Memory

- **Per-context:** ~1KB (keys + nonce state)
- **Adapter state:** ~100KB (minimal)
- **Total overhead:** <10MB for 10K active peers

### Best Practices

1. **Batch encrypt/decrypt** — Amortize FFI call overhead
2. **Rekey periodically** — Not on every message
3. **Monitor metrics** — Detect degradation early

---

## Documentation Structure

- **[OTP vs libsodium](otp-vs-libsodium.md)** — Detailed comparison of crypto providers
  - Performance benchmarks, operational friction, dependency management
  
- **[Security Checklist](security.md)** — Pre-deploy and operational checks
  - Pre-deployment verification, incident response, compliance
  
- **[Implementation Checklist](implementation-checklist.md)** — Building a new adapter
  - CI/CD requirements, testing, binary distribution

---

## Next Steps

- Start with [OTP vs libsodium](otp-vs-libsodium.md) to understand provider choices
- Read [Security Checklist](security.md) before deploying to production
- Reference [Implementation Checklist](implementation-checklist.md) if building a custom adapter

---

## References

- [RFC 7748: Elliptic Curves for Security](https://tools.ietf.org/html/rfc7748)
- [RFC 8439: ChaCha20 and Poly1305](https://tools.ietf.org/html/rfc8439)
- [RFC 5869: HKDF](https://tools.ietf.org/html/rfc5869)
- [OTP Crypto Module](https://erlang.org/doc/man/crypto.html)