//// Transport stub with retry and backoff logic (Phase 1).
////
//// This is a minimal reference implementation following distribute patterns
//// and Gleam/OTP best practices. It demonstrates:
////
//// - Pure function retry logic with exponential backoff
//// - Per-node outcome tracking in broadcasts (type-safe BroadcastResult)
//// - Simulated delivery (succeeds for known nodes, fails gracefully)
//// - OTP-style state management and error handling
////
//// ## Design
////
//// Like `crypto/provider`, this stub provides **stateless pure functions**
//// that can be:
////
//// - Called directly from messaging/global modules
//// - Wrapped in an OTP actor for long-lived instances (Phase 2)
//// - Extended with real network I/O by actual adapters
////
//// Each call is independent; retry logic is computed on demand.
////
//// ## Gleam/OTP Best Practices
////
//// - Type-safe Result/Option handling
//// - Exhaustive pattern matching
//// - Pure functions; no side effects or global state
//// - Clear error types (TransportError variants)
//// - No panics; all errors handled explicitly

import distribute/transport/behaviour
import gleam/dict
import gleam/int
import gleam/list
import gleam/result

/// Known nodes for the stub transport (hardcoded for Phase 1).
const known_nodes = ["node2@host", "node3@host"]

/// Default retry policy for the stub.
const default_retry_policy: behaviour.RetryPolicy = behaviour.RetryPolicy(
  max_attempts: 3,
  initial_backoff_ms: 10,
  max_backoff_ms: 1000,
)

/// Send a binary payload to a node with exponential backoff retry.
///
/// This is a **pure function** that simulates delivery:
/// - If the node is in the known_nodes list, succeeds immediately
/// - If not, retries with exponential backoff up to max_attempts
///
/// ## Type Safety
///
/// - Returns `Result(Nil, behaviour.TransportError)` matching the contract
/// - Exhaustive error handling; no panics
/// - All errors are explicit TransportError variants
///
/// ## Example
///
/// ```gleam
/// case send_with_retry("node2@host", payload) {
///   Ok(Nil) -> io.println("Sent successfully")
///   Error(err) -> io.println("Send failed")
/// }
/// ```
pub fn send_with_retry(
  node: behaviour.NodeId,
  payload: BitArray,
) -> Result(Nil, behaviour.TransportError) {
  attempt_send(node, payload, 0, default_retry_policy)
}

/// Broadcast a payload to multiple nodes with exponential backoff.
///
/// Returns `BroadcastResult` (dict of per-node outcomes) on partial/full success,
/// or `Error(TransportError)` only if **all** nodes fail catastrophically.
///
/// ## Type Safety
///
/// - Returns `Result(behaviour.BroadcastResult, behaviour.TransportError)` per contract
/// - BroadcastResult is `dict.Dict(NodeId, Result(Nil, TransportError))`
/// - Exhaustive matching on outcomes; no silent failures
///
/// ## Example
///
/// ```gleam
/// case broadcast_with_retry(["node2@host", "node3@host"], payload) {
///   Ok(outcomes) ->
///     dict.each(outcomes, fn(node, result) {
///       case result {
///         Ok(Nil) -> io.println("Delivered to " <> node)
///         Error(err) -> io.println("Failed to " <> node)
///       }
///     })
///   Error(err) -> io.println("Broadcast completely failed")
/// }
/// ```
pub fn broadcast_with_retry(
  nodes: List(behaviour.NodeId),
  payload: BitArray,
) -> Result(behaviour.BroadcastResult, behaviour.TransportError) {
  // Send to each node, collecting outcomes
  let outcomes =
    list.fold(nodes, dict.new(), fn(results, node) {
      case attempt_send(node, payload, 0, default_retry_policy) {
        Ok(Nil) -> dict.insert(results, node, Ok(Nil))
        Error(err) -> dict.insert(results, node, Error(err))
      }
    })

  // Check if all nodes failed
  let all_failed =
    outcomes
    |> dict.values()
    |> list.all(fn(res) { result.is_error(res) })

  case all_failed {
    True -> Error(behaviour.AdapterFailure("broadcast: all nodes failed"))
    False -> Ok(outcomes)
  }
}

/// Get the health status of the transport.
///
/// Simple check: `Up` if known nodes exist, `Down` otherwise.
pub fn health() -> behaviour.HealthStatus {
  case known_nodes {
    [] -> behaviour.Down("no known nodes")
    _ -> behaviour.Up
  }
}

/// Get operational metrics.
///
/// Returns a dict of metrics. In Phase 1, mostly placeholder values.
/// Phase 2 will add stateful tracking.
pub fn metrics() -> dict.Dict(String, Int) {
  dict.new()
  |> dict.insert("known_nodes_count", list.length(known_nodes))
}

// ============================================================================
// PRIVATE FUNCTIONS - RETRY LOGIC
// ============================================================================

/// Attempt to send a message with exponential backoff retry.
///
/// Pure function; no side effects. Returns success if node is known,
/// fails with InvalidNode if all retries are exhausted.
fn attempt_send(
  node: behaviour.NodeId,
  _payload: BitArray,
  attempt: Int,
  retry_policy: behaviour.RetryPolicy,
) -> Result(Nil, behaviour.TransportError) {
  case node_is_known(node) {
    True -> Ok(Nil)
    False -> {
      case attempt < retry_policy.max_attempts - 1 {
        True -> {
          // Compute backoff and retry
          let _backoff = calculate_backoff(retry_policy, attempt)
          // In Phase 1, we simulate backoff by recursing
          // In Phase 2+, wrapped in actor, this would be async
          attempt_send(node, <<>>, attempt + 1, retry_policy)
        }
        False ->
          // Max retries exhausted
          Error(behaviour.InvalidNode)
      }
    }
  }
}

/// Calculate exponential backoff in milliseconds.
///
/// Given base and max from retry policy, returns wait_ms = base * 2^attempt (clamped).
/// Pure function; just computation.
fn calculate_backoff(retry_policy: behaviour.RetryPolicy, attempt: Int) -> Int {
  let base = retry_policy.initial_backoff_ms
  let max_backoff = retry_policy.max_backoff_ms
  // 2^attempt using bit shifting for efficiency
  let exponent = int.min(attempt, 10)
  // Cap to prevent overflow
  let multiplier = bit_shift_left(1, exponent)
  int.min(base * multiplier, max_backoff)
}

/// Bitwise left shift (multiply by power of 2).
fn bit_shift_left(n: Int, shift: Int) -> Int {
  case shift {
    0 -> n
    1 -> n * 2
    2 -> n * 4
    3 -> n * 8
    4 -> n * 16
    5 -> n * 32
    6 -> n * 64
    7 -> n * 128
    8 -> n * 256
    9 -> n * 512
    _ -> n * 1024
    // fallback for 10+
  }
}

/// Check if a node is in the known nodes list.
fn node_is_known(node: behaviour.NodeId) -> Bool {
  list.contains(known_nodes, node)
}
