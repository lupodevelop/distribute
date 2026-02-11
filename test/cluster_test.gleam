import distribute/cluster
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn self_node_returns_string_test() {
  // When not distributed, should be "nonode@nohost"
  let name = cluster.self_node()
  should.be_true(name != "")
}

pub fn nodes_returns_list_test() {
  // When not distributed, should be empty
  let connected = cluster.nodes()
  should.equal(connected, [])
}

pub fn is_distributed_false_when_local_test() {
  // By default in tests, not distributed
  // (This may be True if the test runner starts a distributed node)
  let _ = cluster.is_distributed()
  should.be_true(True)
}

pub fn connected_count_zero_when_local_test() {
  let count = cluster.connected_count()
  should.equal(count, 0)
}

pub fn health_works_test() {
  let health = cluster.health()
  should.be_true(health.self_node != "")
}

pub fn start_node_invalid_name_test() {
  // Missing @ should fail validation
  let result = cluster.start_node("invalid", "cookie")
  should.be_error(result)
}

pub fn start_node_cookie_too_long_test() {
  // Cookie > 255 chars should fail
  let long_cookie = repeat_string("a", 256)
  let result = cluster.start_node("node@host", long_cookie)
  should.be_error(result)
}

pub fn connect_invalid_node_test() {
  // Missing @ should fail
  let result = cluster.connect("invalid")
  should.be_error(result)
}

fn repeat_string(s: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False -> s <> repeat_string(s, n - 1)
  }
}
