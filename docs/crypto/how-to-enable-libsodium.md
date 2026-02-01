# How to enable a libsodium-backed adapter

This guide explains how to build, enable, and safely roll out a libsodium-backed crypto adapter.

Build and runtime requirements

- Build-time: libsodium development headers (`libsodium-dev` on Debian/Ubuntu, `libsodium` via Homebrew on macOS).
- Runtime: libc and libsodium available on production hosts if the adapter uses a NIF.

Example local build steps (Linux)

```bash
sudo apt-get update && sudo apt-get install -y libsodium-dev build-essential
# build native part (example)
make
# build project
gleam build && rebar3 compile
```

Enabling at runtime (recommended)

- Use an environment variable or application config to choose the adapter.

```bash
export DISTRIBUTE_CRYPTO_ADAPTER=libsodium
# start your system normally
```

- The application should log the chosen adapter at startup and fail fast if the adapter cannot initialize.

CI example (install libsodium and run tests)

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

Rollout checklist

- Start with a canary environment (small percentage of traffic) and monitor decrypt/handshake errors and crash counts.
- Retain a rollback plan to switch back to the OTP builtin adapter and have a test to validate the fallback.

Security notes

- Ensure the NIF or native wrapper follows memory-safety best practices and has a signed, reviewed release.
- Keep release branches with reproducible build artifacts for the native code.