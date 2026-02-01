# Decision guide: OTP builtin vs libsodium-backed adapter

Executive summary

Choose the Erlang/OTP builtin crypto primitives (the current `sodium_adapter` implementation) by default if the target runtime supports the required algorithms (X25519, ChaCha20-Poly1305 / XChaCha20-Poly1305 and HKDF). Only introduce a libsodium-backed native adapter (NIF or port) if a documented technical requirement cannot be satisfied with the OTP primitives or if measured performance benefits justify the additional complexity and risk.

Compatibility table (reference)

- OTP 22+: X25519 keypair generation (`crypto:generate_key(ecdh, x25519)`), ECDH (`crypto:compute_key(ecdh, ...)`) and ChaCha20-Poly1305 AEAD (`crypto_one_time_aead(chacha20_poly1305, ...)`) are supported on most modern OpenSSL/LibreSSL-backed OTP builds.
- If runtime lacks X25519 or AEAD cipher support, a libsodium-backed adapter is a viable option.

Decision rubric (measurable)

Use the rubric below to decide whether to implement a libsodium adapter:

- Functional gap: If required primitive is unavailable (e.g., XChaCha20-Poly1305) → libsodium justified.
- Performance threshold: If OTP path shows a sustained throughput deficit > 30% for the target workload (measured under representative load), consider libsodium.
- Operational cost: If the added packaging/CI cost and maintenance burden are acceptable (budget/time allocated), proceed.

Decision example:
- If runtime supports X25519 and XChaCha20-Poly1305 and measured performance is within 30% of target, prefer OTP builtin. Otherwise prepare an RFC.

Runtime configuration (example)

Provide a simple runtime switch so operators can choose the adapter without code changes. Example environment-based selection:

```bash
# Select adapter at runtime: 'otp' or 'libsodium'
export DISTRIBUTE_CRYPTO_ADAPTER=otp
# or
export DISTRIBUTE_CRYPTO_ADAPTER=libsodium
```

In application boot, read the variable and configure the adapter accordingly.

RFC template (minimal)

When libsodium is considered, open an RFC containing:

- Motivation: missing primitive or measured performance gap (include benchmarks)
- Security assessment: memory-safety, audit plan
- CI impact: build, package, and cross-platform matrix
- Rollout plan: canary strategy, metrics to monitor

Operational checklist (before implementing libsodium)

- Confirm runtime primitives are truly insufficient (document tests and results).
- Create an RFC and obtain team approval.
- Ensure CI and packaging plans are in place to build native components.

CI example: guard that runtime supports required primitives

Add a verification step in CI to ensure the runner's OTP supports required crypto primitives. Example (GitHub Actions shell step):

```yaml
- name: Verify OTP crypto support
  run: |
    erl -noshell -eval '
      Curves = crypto:supports(curves),
      Ciphers = crypto:supports(ciphers),
      case {lists:member(x25519, Curves), lists:member(chacha20_poly1305, Ciphers)} of
        {true, true} -> io:format("Crypto OK~n"), halt(0);
        _ -> io:format("Missing crypto support~n"), halt(1)
      end
    '
```

References

- RFC 7748 (Curve25519)
- XChaCha20-Poly1305 and ChaCha20-Poly1305 AEAD specifications
- libsodium documentation

See `implementation-checklist.md` for a step-by-step plan to implement a libsodium-backed adapter, including CI and testing requirements.
