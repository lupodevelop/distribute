import gleeunit
import gleeunit/should
import gleam/dict
import gleam/result
import distribute/transport/behaviour
import distribute/transport/stub

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// SEND TESTS
// ============================================================================

pub fn send_to_known_node_test() {
  let payload = <<"test message">>
  let result = stub.send_with_retry("node2@host", payload)
  
  result
  |> should.be_ok()
}

pub fn send_to_unknown_node_test() {
  let payload = <<"test message">>
  let result = stub.send_with_retry("unknown@host", payload)
  
  result
  |> should.be_error()
  
  // Should fail with InvalidNode after retries
  case result {
    Error(behaviour.InvalidNode) -> Nil
    Error(_) -> panic as "Expected InvalidNode error"
    Ok(_) -> panic as "Expected error, got Ok"
  }
}

// ============================================================================
// BROADCAST TESTS
// ============================================================================

pub fn broadcast_to_all_known_nodes_test() {
  let payload = <<"broadcast message">>
  let nodes = ["node2@host", "node3@host"]
  let result = stub.broadcast_with_retry(nodes, payload)
  
  result
  |> should.be_ok()
  
  // All nodes should succeed
  case result {
    Ok(outcomes) -> {
      let node2_result = dict.get(outcomes, "node2@host")
      let node3_result = dict.get(outcomes, "node3@host")
      
      node2_result
      |> should.be_ok()
      |> should.equal(Ok(Nil))
      
      node3_result
      |> should.be_ok()
      |> should.equal(Ok(Nil))
    }
    Error(_) -> panic as "Expected Ok, got Error"
  }
}

pub fn broadcast_to_mixed_nodes_test() {
  let payload = <<"broadcast message">>
  let nodes = ["node2@host", "unknown@host"]
  let result = stub.broadcast_with_retry(nodes, payload)
  
  // Should succeed because at least one node succeeded
  result
  |> should.be_ok()
  
  case result {
    Ok(outcomes) -> {
      // node2@host should succeed
      let node2_result = dict.get(outcomes, "node2@host")
      node2_result
      |> should.be_ok()
      |> should.equal(Ok(Nil))
      
      // unknown@host should fail
      let unknown_result = dict.get(outcomes, "unknown@host")
      unknown_result
      |> should.be_ok()
      |> result.is_error()
      |> should.be_true()
    }
    Error(_) -> panic as "Expected Ok, got Error"
  }
}

pub fn broadcast_to_all_unknown_nodes_test() {
  let payload = <<"broadcast message">>
  let nodes = ["unknown1@host", "unknown2@host"]
  let result = stub.broadcast_with_retry(nodes, payload)
  
  // Should fail because all nodes failed
  result
  |> should.be_error()
  
  case result {
    Error(behaviour.AdapterFailure(_)) -> Nil
    Error(_) -> panic as "Expected AdapterFailure error"
    Ok(_) -> panic as "Expected error, got Ok"
  }
}

pub fn broadcast_empty_list_test() {
  let payload = <<"broadcast message">>
  let nodes = []
  let result = stub.broadcast_with_retry(nodes, payload)
  
  // Empty list should fail (all nodes failed vacuously)
  result
  |> should.be_error()
}

// ============================================================================
// HEALTH TESTS
// ============================================================================

pub fn health_check_test() {
  let status = stub.health()
  
  case status {
    behaviour.Up -> Nil
    _ -> panic as "Expected Up status"
  }
}

// ============================================================================
// METRICS TESTS
// ============================================================================

pub fn metrics_test() {
  let metrics = stub.metrics()
  
  let known_count = dict.get(metrics, "known_nodes_count")
  known_count
  |> should.be_ok()
  |> should.equal(2)
}
