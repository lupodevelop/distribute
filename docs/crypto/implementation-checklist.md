# Implementation checklist: libsodium-backed adapter (NIF or port)

This checklist provides concrete steps, example commands and CI snippets for implementing a libsodium-backed adapter compatible with the `distribute/crypto/adapter.gleam` contract.

1. RFC and approval
   - [ ] Create an RFC documenting: the functional gap, performance measurements, security assessment, CI/packaging impact, and rollout plan.

2. FFI specification
   - [ ] Define the Erlang-facing native interface (module name example: `crypto_sodium_native` or `crypto_sodium_nif`).
   - Minimum required functions (error contract: return `{ok, ...}` or `{error, Reason}`):
     - `gen_keypair()`
     - `scalarmult(pub, priv)`
     - `aead_encrypt(key, nonce, aad, plaintext)`
     - `aead_decrypt(key, nonce, aad, ciphertext)`
     - `random_bytes(n)`
     - `hkdf(salt, ikm, info, len)`
     - `generate_key_id()`

3. Erlang wrapper
   - [ ] Implement an Erlang wrapper that validates inputs and normalizes errors. Keep cryptographic primitives in native code and business logic in Erlang.
   - Example: `crypto_sodium_native:gen_keypair()` returns `{ok, {Pub, Priv}}`.

4. Gleam adapter
   - [ ] Implement the adapter in Gleam and ensure it fully implements `CryptoAdapter` (init, handshake_start/continue, secure_context, encrypt/decrypt, rekey, health, metrics).

5. Local build & test commands (examples)
   - Install libsodium (Ubuntu): `sudo apt-get update && sudo apt-get install -y libsodium-dev`
   - macOS: `brew install libsodium`
   - Build native code (example for a small C wrapper):
     - `make` or `rebar3 compile` (depending on project structure)
   - Run tests: `gleam test` and `rebar3 ct` or `erl -sname test -s ...` as project requires.

6. CI example (GitHub Actions snippet)

```yaml
jobs:
  test-libsodium:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install libsodium
        run: sudo apt-get update && sudo apt-get install -y libsodium-dev
      - name: Build & test
        run: |
          rebar3 compile || true
          gleam test
```

7. Testing matrix
   - Unit tests: Erlang wrapper input validation, fixed vectors.
   - Integration tests: handshake paths, encrypt/decrypt roundtrips, tamper cases.
   - Fuzz tests: apply fuzzing to decrypt and handshake entrypoints.
   - Cross-platform: run on Ubuntu and macOS; Windows optional.

8. Security & review
   - [ ] Threat model and memory-safety analysis.
   - [ ] Dedicated review for native code and optional third-party audit.
   - [ ] Plan for process crash recovery and fallback to OTP builtin.

9. Packaging and release
   - Document dependencies and build steps in README and `docs/crypto/how-to-enable-libsodium.md`.
   - Make the libsodium adapter optional at packaging time and document how to enable/disable it.

10. Rollout
   - Canary deploy on a small percentage of nodes.
   - Monitor handshakes_failed, decrypt errors, and crash rates; rollback if error rates spike.

Notes

- Prefer using existing, maintained libraries (e.g., `erl_sodium`) where practical. Implementing a NIF from scratch is acceptable only with sufficient review resources.