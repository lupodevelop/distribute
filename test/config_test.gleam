import distribute/config
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn default_call_timeout_is_5000ms_test() {
  config.default().default_call_timeout_ms
  |> should.equal(5000)
}

pub fn default_init_timeout_is_5000ms_test() {
  config.default().default_init_timeout_ms
  |> should.equal(5000)
}

pub fn default_max_payload_is_4mb_test() {
  config.default().max_payload_size_bytes
  |> should.equal(4 * 1024 * 1024)
}

pub fn default_max_decoded_list_elements_is_10000_test() {
  config.default().max_decoded_list_elements
  |> should.equal(10_000)
}

pub fn default_isolated_proxy_shutdown_grace_is_5000ms_test() {
  config.default().isolated_proxy_shutdown_grace_ms
  |> should.equal(5000)
}

pub fn default_resource_owner_poll_is_5000ms_test() {
  config.default().resource_owner_poll_ms
  |> should.equal(5000)
}

pub fn default_health_proxy_shutdown_grace_is_1000ms_test() {
  config.default().health_proxy_shutdown_grace_ms
  |> should.equal(1000)
}

pub fn default_conflict_resolver_timeout_is_1000ms_test() {
  // 1 s, not 5 s: `:global`'s singleton serialises every cluster-
  // wide operation behind the resolver. A 5 s deadline (the v4-alpha
  // value) was too long for cluster throughput. Pure resolvers
  // (`lowest_pid_wins`, `keep_local`, `node_priority`,
  // `kill_both`) take microseconds and never approach this.
  config.default().conflict_resolver_timeout_ms
  |> should.equal(1000)
}

pub fn get_always_returns_valid_config_test() {
  let cfg = config.get()
  { cfg.default_call_timeout_ms > 0 } |> should.equal(True)
  { cfg.default_init_timeout_ms > 0 } |> should.equal(True)
  { cfg.max_payload_size_bytes > 0 } |> should.equal(True)
  { cfg.max_distribution_atoms > 0 } |> should.equal(True)
  { cfg.max_decoded_list_elements > 0 } |> should.equal(True)
  { cfg.isolated_proxy_shutdown_grace_ms > 0 } |> should.equal(True)
  { cfg.resource_owner_poll_ms > 0 } |> should.equal(True)
  { cfg.health_proxy_shutdown_grace_ms > 0 } |> should.equal(True)
  { cfg.conflict_resolver_timeout_ms > 0 } |> should.equal(True)
}

pub fn configure_and_get_roundtrip_test() {
  config.reset()
  let custom =
    config.Config(
      default_call_timeout_ms: 99_999,
      default_init_timeout_ms: 88_888,
      max_payload_size_bytes: 512,
      max_distribution_atoms: 10_000,
      max_decoded_list_elements: 12_345,
      isolated_proxy_shutdown_grace_ms: 7000,
      resource_owner_poll_ms: 3000,
      health_proxy_shutdown_grace_ms: 2000,
      conflict_resolver_timeout_ms: 1500,
    )
  let assert Ok(Nil) = config.configure(custom)
  let got = config.get()
  got.default_call_timeout_ms |> should.equal(99_999)
  got.default_init_timeout_ms |> should.equal(88_888)
  got.max_payload_size_bytes |> should.equal(512)
  got.max_decoded_list_elements |> should.equal(12_345)
  got.isolated_proxy_shutdown_grace_ms |> should.equal(7000)
  got.resource_owner_poll_ms |> should.equal(3000)
  got.health_proxy_shutdown_grace_ms |> should.equal(2000)
  got.conflict_resolver_timeout_ms |> should.equal(1500)
  // Reset so other tests see a clean state
  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
}

pub fn configure_rejects_negative_timeout_test() {
  let result =
    config.configure(
      config.Config(..config.default(), default_call_timeout_ms: -1),
    )
  should.be_error(result)
}

pub fn configure_rejects_zero_timeout_test() {
  let result =
    config.configure(
      config.Config(..config.default(), default_init_timeout_ms: 0),
    )
  should.be_error(result)
}

pub fn configure_rejects_zero_payload_size_test() {
  let result =
    config.configure(
      config.Config(..config.default(), max_payload_size_bytes: 0),
    )
  should.be_error(result)
}

pub fn configure_rejects_zero_list_cap_test() {
  let result =
    config.configure(
      config.Config(..config.default(), max_decoded_list_elements: 0),
    )
  let assert Error(config.InvalidListElementCap(0)) = result
}

pub fn configure_cannot_be_widened_at_runtime_test() {
  // Regression guard against the audit claim that a second `configure`
  // call could widen `max_payload_size_bytes` and bypass memory safety.
  // The FFI is single-write: any subsequent call returns AlreadyConfigured
  // and the persistent_term keeps the original limit untouched.
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(..config.default(), max_payload_size_bytes: 1024),
    )
  // Hostile second call attempting to remove the limit:
  let widening =
    config.configure(
      config.Config(..config.default(), max_payload_size_bytes: 999_999_999),
    )
  let active = config.get()
  config.reset()
  let assert Ok(Nil) = config.configure(config.default())

  let assert Error(config.AlreadyConfigured) = widening
  // Active limit is still the original 1024, NOT the hostile 999_999_999.
  active.max_payload_size_bytes |> should.equal(1024)
}

pub fn configure_rejects_negative_payload_size_test() {
  let result =
    config.configure(
      config.Config(..config.default(), max_payload_size_bytes: -100),
    )
  should.be_error(result)
}
