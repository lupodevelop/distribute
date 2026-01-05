//// Test helpers used by integration tests

import distribute/crypto/adapter.{type CryptoAdapter}
import distribute/crypto/types.{type ProviderHandle, type CryptoError}
import gleeunit/should

// with_provider: given a `CryptoAdapter` instance and a `name`, initialize
// it, run `f(provider, handle)` and ensure `shutdown` is called and returns Ok.
pub fn with_provider(provider: CryptoAdapter, name: String, f: fn(CryptoAdapter, ProviderHandle) -> a) -> Result(a, CryptoError) {
  let options = adapter.default_options(name)

  let result = { provider.init }(options)
  case result {
    Ok(handle) -> {
      let res = f(provider, handle)

      let shutdown_result = { provider.shutdown }(handle)
      should.be_ok(shutdown_result)

      Ok(res)
    }
    Error(err) -> Error(err)
  }
}

