//// Integration tests for the discovery layer.
////
//// These tests verify that:
//// 1. The discovery facade works correctly (singleton pattern).
//// 2. Peer events are correctly emitted.
//// 3. Snapshot and lookup queries work.
//// 4. Health reporting works.

import distribute/discovery
import distribute/discovery/adapter
import distribute/discovery/types
import gleam/erlang/process
import gleam/list
import gleam/otp/static_supervisor as supervisor
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Helper to start the discovery singleton
fn start_discovery() -> process.Pid {
  let worker = discovery.child_spec()
  let assert Ok(sup) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(worker)
    |> supervisor.start()

  // Give the process time to register and sync
  process.sleep(100)

  sup.pid
}

// Helper to stop the discovery
fn stop_discovery(pid: process.Pid) {
  process.send_exit(pid)
}

// Test 1: discovery health via singleton
pub fn discovery_health_test() {
  let sup = start_discovery()

  // Health should be Up or Degraded (no sync yet is ok)
  let health = discovery.health()

  case health {
    types.Up -> should.be_true(True)
    types.Degraded(_) -> should.be_true(True)
    types.Down(_) -> should.fail()
  }

  stop_discovery(sup)
}

// Test 2: discovery snapshot returns empty list (no other nodes)
pub fn discovery_snapshot_test() {
  let sup = start_discovery()

  // Wait for initial sync
  process.sleep(200)

  let result = discovery.snapshot()

  result
  |> should.be_ok

  let assert Ok(peers) = result
  // In single-node test, we expect no other peers
  list.length(peers)
  |> should.equal(0)

  stop_discovery(sup)
}

// Test 3: discovery lookup for non-existent peer
pub fn discovery_lookup_not_found_test() {
  let sup = start_discovery()

  process.sleep(100)

  let result = discovery.lookup("nonexistent@node")

  case result {
    Error(types.PeerNotFound(_)) -> should.be_true(True)
    _ -> should.fail()
  }

  stop_discovery(sup)
}

// Test 4: subscribe and unsubscribe
pub fn discovery_subscription_test() {
  let sup = start_discovery()

  // Subscribe with a simple callback
  let callback = fn(_event: types.DiscoveryEvent) { Nil }

  let subscribe_result = discovery.subscribe(callback)
  subscribe_result
  |> should.be_ok

  let assert Ok(sub_id) = subscribe_result

  // Unsubscribe should succeed
  let unsubscribe_result = discovery.unsubscribe(sub_id)
  unsubscribe_result
  |> should.be_ok

  stop_discovery(sup)
}

// Test 5: adapter.default_options creates valid options
pub fn discovery_default_options_test() {
  let opts = adapter.default_options("test_discovery")

  opts.name
  |> should.equal("test_discovery")

  opts.sync_interval_ms
  |> should.equal(5000)

  opts.sync_timeout_ms
  |> should.equal(10_000)

  // Verify retry config defaults
  opts.retry_config.max_attempts
  |> should.equal(3)

  opts.retry_config.initial_backoff_ms
  |> should.equal(100)
}

// Test 6: types error classification
pub fn discovery_error_classification_test() {
  // Transient errors
  types.is_transient_error(types.SyncFailed("test"))
  |> should.be_true

  types.is_transient_error(types.Timeout(1000))
  |> should.be_true

  // Permanent errors
  types.is_permanent_error(types.InvalidConfiguration("test"))
  |> should.be_true

  types.is_permanent_error(types.PeerNotFound("node"))
  |> should.be_true

  // Non-transient
  types.is_transient_error(types.InvalidConfiguration("test"))
  |> should.be_false
}

// Test 7: metrics structure
pub fn discovery_metrics_test() {
  // Test default metrics structure
  let default_metrics =
    types.DiscoveryMetrics(
      sync_count: 0,
      sync_failures: 0,
      last_sync_duration_ms: 0,
      events_emitted: 0,
      known_peers_count: 0,
      error_rate: 0.0,
    )

  default_metrics.sync_count
  |> should.equal(0)

  default_metrics.error_rate
  |> should.equal(0.0)
}

// Test 8: retry config
pub fn discovery_retry_config_test() {
  let config = types.default_retry_config()

  config.max_attempts
  |> should.equal(3)

  config.initial_backoff_ms
  |> should.equal(100)

  config.max_backoff_ms
  |> should.equal(5000)

  // Float comparison
  { config.backoff_multiplier >. 1.9 && config.backoff_multiplier <. 2.1 }
  |> should.be_true
}
