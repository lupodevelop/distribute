//// Test helpers used by integration tests

import distribute/crypto/adapter.{type CryptoAdapter}
import distribute/crypto/types.{
  type CryptoError, type ProviderHandle, type ProviderOptions,
}
import gleeunit/should

// with_provider: given a `CryptoAdapter` instance and a `name`, initialize
// it, run `f(provider, handle)` and ensure `shutdown` is called and returns Ok.
pub fn with_provider(
  provider: CryptoAdapter,
  name: String,
  f: fn(CryptoAdapter, ProviderHandle) -> a,
) -> Result(a, CryptoError) {
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

// with_provider_module: accept a provider constructor (fn() -> CryptoAdapter)
// so tests can pass the adapter module's `new` function directly.
pub fn with_provider_module(
  provider_ctor: fn() -> CryptoAdapter,
  name: String,
  f: fn(CryptoAdapter, ProviderHandle) -> a,
) -> Result(a, CryptoError) {
  let provider = provider_ctor()
  with_provider(provider, name, f)
}

// like with_provider_module but accept explicit ProviderOptions
pub fn with_provider_module_with_options(
  provider_ctor: fn() -> CryptoAdapter,
  options: ProviderOptions,
  f: fn(CryptoAdapter, ProviderHandle) -> a,
) -> Result(a, CryptoError) {
  let provider = provider_ctor()

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

// with_provider_module_checked: like with_provider_module but also verify the
// adapter-level `get_handle` returns Error for `name` after shutdown.
pub fn with_provider_module_checked(
  provider_ctor: fn() -> CryptoAdapter,
  get_handle: fn(String) -> Result(ProviderHandle, e),
  name: String,
  f: fn(CryptoAdapter, ProviderHandle) -> a,
) -> Result(a, CryptoError) {
  let options = adapter.default_options(name)
  let provider = provider_ctor()

  let result = { provider.init }(options)
  case result {
    Ok(handle) -> {
      let res = f(provider, handle)

      let shutdown_result = { provider.shutdown }(handle)
      should.be_ok(shutdown_result)

      // Sanity: adapter-level get_handle should not return Ok for this name
      let handle_check = get_handle(name)
      should.be_error(handle_check)

      Ok(res)
    }
    Error(err) -> Error(err)
  }
}

// with_provider_module_with_options_checked: same as above with explicit options
pub fn with_provider_module_with_options_checked(
  provider_ctor: fn() -> CryptoAdapter,
  get_handle: fn(String) -> Result(ProviderHandle, e),
  options: ProviderOptions,
  name: String,
  f: fn(CryptoAdapter, ProviderHandle) -> a,
) -> Result(a, CryptoError) {
  let provider = provider_ctor()

  let result = { provider.init }(options)
  case result {
    Ok(handle) -> {
      let res = f(provider, handle)

      let shutdown_result = { provider.shutdown }(handle)
      should.be_ok(shutdown_result)

      let handle_check = get_handle(name)
      should.be_error(handle_check)

      Ok(res)
    }
    Error(err) -> Error(err)
  }
}
