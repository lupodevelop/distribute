import distribute/discovery/behaviour.{
  type DiscoveryError, Degraded, Down, HealthInfo, InvalidConfiguration,
  PeerDown, PeerInfo, PeerNotFound, PeerUp, PeerUpdate, ShutdownTimeout,
  StartFailed, StopFailed, SubscriptionFailed, SyncFailed, Timeout, Up,
}
import gleam/dict
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Error classification — is_transient_error
// ============================================================================

pub fn sync_failed_is_transient_test() {
  behaviour.is_transient_error(SyncFailed("network"))
  |> should.be_true()
}

pub fn timeout_is_transient_test() {
  behaviour.is_transient_error(Timeout(5000))
  |> should.be_true()
}

pub fn start_failed_is_not_transient_test() {
  behaviour.is_transient_error(StartFailed("config"))
  |> should.be_false()
}

pub fn stop_failed_is_not_transient_test() {
  behaviour.is_transient_error(StopFailed("busy"))
  |> should.be_false()
}

pub fn shutdown_timeout_is_not_transient_test() {
  behaviour.is_transient_error(ShutdownTimeout(10_000))
  |> should.be_false()
}

pub fn peer_not_found_is_not_transient_test() {
  behaviour.is_transient_error(PeerNotFound("node@x"))
  |> should.be_false()
}

pub fn invalid_configuration_is_not_transient_test() {
  behaviour.is_transient_error(InvalidConfiguration("missing name"))
  |> should.be_false()
}

pub fn subscription_failed_is_not_transient_test() {
  behaviour.is_transient_error(SubscriptionFailed("dup"))
  |> should.be_false()
}

// ============================================================================
// Error classification — is_permanent_error
// ============================================================================

pub fn invalid_configuration_is_permanent_test() {
  behaviour.is_permanent_error(InvalidConfiguration("bad"))
  |> should.be_true()
}

pub fn peer_not_found_is_permanent_test() {
  behaviour.is_permanent_error(PeerNotFound("node@gone"))
  |> should.be_true()
}

pub fn start_failed_is_permanent_test() {
  behaviour.is_permanent_error(StartFailed("fatal"))
  |> should.be_true()
}

pub fn sync_failed_is_not_permanent_test() {
  behaviour.is_permanent_error(SyncFailed("retry"))
  |> should.be_false()
}

pub fn timeout_is_not_permanent_test() {
  behaviour.is_permanent_error(Timeout(1000))
  |> should.be_false()
}

pub fn stop_failed_is_not_permanent_test() {
  behaviour.is_permanent_error(StopFailed("stuck"))
  |> should.be_false()
}

pub fn shutdown_timeout_is_not_permanent_test() {
  behaviour.is_permanent_error(ShutdownTimeout(5000))
  |> should.be_false()
}

pub fn subscription_failed_is_not_permanent_test() {
  behaviour.is_permanent_error(SubscriptionFailed("err"))
  |> should.be_false()
}

// ============================================================================
// Mutually exclusive: no error is both transient AND permanent
// ============================================================================

pub fn transient_permanent_mutually_exclusive_test() {
  let errors: List(DiscoveryError) = [
    StartFailed(""), StopFailed(""), ShutdownTimeout(0), SyncFailed(""),
    PeerNotFound(""), InvalidConfiguration(""), Timeout(0),
    SubscriptionFailed(""),
  ]
  check_exclusive(errors)
}

fn check_exclusive(errors: List(DiscoveryError)) -> Nil {
  case errors {
    [] -> Nil
    [err, ..rest] -> {
      let t = behaviour.is_transient_error(err)
      let p = behaviour.is_permanent_error(err)
      case t, p {
        True, True -> should.fail()
        _, _ -> Nil
      }
      check_exclusive(rest)
    }
  }
}

// ============================================================================
// default_options
// ============================================================================

pub fn default_options_name_test() {
  let opts = behaviour.default_options("my_discovery")
  should.equal(opts.name, "my_discovery")
}

pub fn default_options_sync_interval_test() {
  let opts = behaviour.default_options("d")
  should.equal(opts.sync_interval_ms, 5000)
}

pub fn default_options_sync_timeout_test() {
  let opts = behaviour.default_options("d")
  should.equal(opts.sync_timeout_ms, 10_000)
}

pub fn default_options_telemetry_prefix_test() {
  let opts = behaviour.default_options("d")
  should.equal(opts.telemetry_prefix, "discovery")
}

pub fn default_options_seed_peers_empty_test() {
  let opts = behaviour.default_options("d")
  should.equal(opts.seed_peers, [])
}

pub fn default_options_custom_empty_test() {
  let opts = behaviour.default_options("d")
  should.equal(dict.size(opts.custom), 0)
}

// ============================================================================
// SubscriptionId — opaque round-trip
// ============================================================================

pub fn subscription_id_roundtrip_test() {
  let id = behaviour.new_subscription_id("sub-123")
  behaviour.subscription_id_value(id)
  |> should.equal("sub-123")
}

pub fn subscription_id_empty_string_test() {
  let id = behaviour.new_subscription_id("")
  behaviour.subscription_id_value(id)
  |> should.equal("")
}

pub fn subscription_id_unicode_test() {
  let id = behaviour.new_subscription_id("sub-日本語-🔥")
  behaviour.subscription_id_value(id)
  |> should.equal("sub-日本語-🔥")
}

// ============================================================================
// PeerInfo — construction
// ============================================================================

pub fn peer_info_construction_test() {
  let meta = dict.from_list([#("version", "2.1.0"), #("role", "worker")])
  let info = PeerInfo(id: "node@host", metadata: meta)
  should.equal(info.id, "node@host")
  should.equal(dict.get(info.metadata, "version"), Ok("2.1.0"))
}

pub fn peer_info_empty_metadata_test() {
  let info = PeerInfo(id: "n@h", metadata: dict.new())
  should.equal(info.id, "n@h")
  should.equal(dict.size(info.metadata), 0)
}

// ============================================================================
// DiscoveryEvent — variant construction
// ============================================================================

pub fn peer_up_event_test() {
  let meta = dict.from_list([#("ip", "10.0.0.1")])
  let event = PeerUp(peer: "node@a", metadata: meta)
  case event {
    PeerUp(peer, metadata) -> {
      should.equal(peer, "node@a")
      should.equal(dict.get(metadata, "ip"), Ok("10.0.0.1"))
    }
    _ -> should.fail()
  }
}

pub fn peer_update_event_test() {
  let meta = dict.from_list([#("status", "healthy")])
  let event = PeerUpdate(peer: "node@b", metadata: meta)
  case event {
    PeerUpdate(peer, _) -> should.equal(peer, "node@b")
    _ -> should.fail()
  }
}

pub fn peer_down_event_test() {
  let event = PeerDown(peer: "node@c", reason: "timeout")
  case event {
    PeerDown(peer, reason) -> {
      should.equal(peer, "node@c")
      should.equal(reason, "timeout")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// HealthStatus — variant construction
// ============================================================================

pub fn health_up_test() {
  let h: behaviour.HealthStatus = Up
  should.equal(h, Up)
}

pub fn health_degraded_test() {
  let h: behaviour.HealthStatus = Degraded("slow sync")
  should.equal(h, Degraded("slow sync"))
}

pub fn health_down_test() {
  let h: behaviour.HealthStatus = Down("backend unreachable")
  should.equal(h, Down("backend unreachable"))
}

// ============================================================================
// HealthInfo — construction
// ============================================================================

pub fn health_info_construction_test() {
  let info =
    HealthInfo(
      status: Up,
      last_sync_time_ms: Some(1_700_000_000_000),
      last_error: None,
      known_peers_count: 5,
    )
  should.equal(info.status, Up)
  should.equal(info.last_sync_time_ms, Some(1_700_000_000_000))
  should.equal(info.last_error, None)
  should.equal(info.known_peers_count, 5)
}

pub fn health_info_with_error_test() {
  let info =
    HealthInfo(
      status: Degraded("stale"),
      last_sync_time_ms: None,
      last_error: Some("connection refused"),
      known_peers_count: 0,
    )
  should.equal(info.last_error, Some("connection refused"))
  should.equal(info.known_peers_count, 0)
}
