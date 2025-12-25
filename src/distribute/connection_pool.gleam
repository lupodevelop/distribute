/// Connection pooling for efficient RPC communication.
///
/// This module provides a process-based connection pool backed by ETS
/// for atomic operations and efficient connection management.
import gleam/list

// ============================================================================
// Types (Opaque - backed by Erlang FFI)
// ============================================================================

/// Connection pool handle (backed by ETS table)
pub type Pool

/// Connection handle from a pool
pub type Connection

/// Pool statistics
pub type PoolStats {
  PoolStats(
    target_node: String,
    max_connections: Int,
    active_connections: Int,
    available_connections: Int,
  )
}

/// Pool operation errors
pub type PoolError {
  /// Pool has reached maximum connections
  PoolExhausted
  /// Connection establishment failed
  ConnectionFailed(String)
  /// Invalid pool configuration
  InvalidConfig(String)
  /// Pool was destroyed or invalid
  InvalidPool
}

// ============================================================================
// FFI declarations
// ============================================================================

type Dynamic

@external(erlang, "connection_pool_ffi", "new_pool")
fn new_pool_ffi(target_node: String, max_connections: Int) -> Dynamic

@external(erlang, "connection_pool_ffi", "get_connection")
fn get_connection_ffi(pool: Pool) -> Dynamic

@external(erlang, "connection_pool_ffi", "release_connection")
fn release_connection_ffi(connection: Connection) -> Dynamic

@external(erlang, "connection_pool_ffi", "pool_stats")
fn pool_stats_ffi(pool: Pool) -> Dynamic

@external(erlang, "connection_pool_ffi", "destroy_pool")
fn destroy_pool_ffi(pool: Pool) -> Dynamic

@external(erlang, "connection_pool_ffi", "stress_test_pool")
fn stress_test_pool_ffi(pool: Pool, ops: Int, workers: Int) -> Dynamic

@external(erlang, "connection_pool_ffi", "is_ok")
fn is_ok(value: Dynamic) -> Bool

@external(erlang, "connection_pool_ffi", "get_error")
fn get_error(value: Dynamic) -> String

@external(erlang, "erlang", "element")
fn element(index: Int, tuple: Dynamic) -> a

// ============================================================================
// Core functions
// ============================================================================

/// Create a new connection pool backed by ETS for atomic operations
pub fn new(target_node: String, max_connections: Int) -> Result(Pool, PoolError) {
  case max_connections > 0 {
    False -> Error(InvalidConfig("max_connections must be > 0"))
    True -> {
      let result = new_pool_ffi(target_node, max_connections)
      case is_ok(result) {
        True -> Ok(unwrap_pool(result))
        False -> Error(ConnectionFailed(get_error(result)))
      }
    }
  }
}

/// Execute a function with a pooled connection (automatically released)
pub fn with_connection(
  pool: Pool,
  f: fn(Connection) -> a,
) -> Result(a, PoolError) {
  let conn_result = get_connection_ffi(pool)
  case is_ok(conn_result) {
    True -> {
      let connection = unwrap_connection(conn_result)
      let result = f(connection)
      // Always release connection after use
      let _ = release_connection_ffi(connection)
      Ok(result)
    }
    False -> {
      let error_msg = get_error(conn_result)
      case error_msg {
        "pool_exhausted" -> Error(PoolExhausted)
        _ -> Error(ConnectionFailed(error_msg))
      }
    }
  }
}

/// Get pool statistics (atomic read from ETS)
pub fn stats(pool: Pool) -> Result(PoolStats, PoolError) {
  let result = pool_stats_ffi(pool)
  case is_ok(result) {
    True -> {
      let map = unwrap_stats_map(result)
      Ok(PoolStats(
        target_node: get_map_string(map, "target_node"),
        max_connections: get_map_int(map, "max_connections"),
        active_connections: get_map_int(map, "active_connections"),
        available_connections: get_map_int(map, "available_connections"),
      ))
    }
    False -> Error(InvalidPool)
  }
}

/// Destroy the pool and release all resources
pub fn destroy(pool: Pool) -> Result(Nil, PoolError) {
  let result = destroy_pool_ffi(pool)
  case is_ok(result) {
    True -> Ok(Nil)
    False -> Error(InvalidPool)
  }
}

// ============================================================================
// Batch operations
// ============================================================================

/// Send multiple messages in a batch using a single connection
pub fn send_batch(
  pool: Pool,
  messages: List(#(String, a)),
  send_fn: fn(String, a) -> Result(Nil, b),
) -> Result(List(Result(Nil, b)), PoolError) {
  with_connection(pool, fn(_conn) {
    // Process each message and collect results
    list.map(messages, fn(message) {
      let #(name, msg) = message
      send_fn(name, msg)
    })
  })
}

/// Send multiple messages in parallel (up to pool size) with error aggregation
pub fn send_batch_parallel(
  pool: Pool,
  messages: List(#(String, a)),
  send_fn: fn(String, a) -> Result(Nil, b),
) -> Result(BatchResult(b), PoolError) {
  // For now, use sequential sending
  // Future: implement parallel with multiple connections
  case send_batch(pool, messages, send_fn) {
    Ok(results) -> {
      let successes =
        list.filter(results, fn(r) {
          case r {
            Ok(_) -> True
            Error(_) -> False
          }
        })
        |> list.length

      let failures =
        list.fold(results, [], fn(acc, r) {
          case r {
            Error(e) -> [e, ..acc]
            Ok(_) -> acc
          }
        })

      Ok(BatchResult(
        total: list.length(messages),
        successful: successes,
        failed: list.length(failures),
        errors: failures,
      ))
    }
    Error(e) -> Error(e)
  }
}

/// Run a concurrency stress-test against a pool. This spawns multiple worker processes
/// each performing `ops` get/release cycles using the pool; returns final pool stats.
pub fn stress_test(
  pool: Pool,
  ops_per_worker: Int,
  workers: Int,
) -> Result(PoolStats, PoolError) {
  let res = stress_test_pool_ffi(pool, ops_per_worker, workers)
  case is_ok(res) {
    True -> {
      let map = unwrap_stats_map(res)
      Ok(PoolStats(
        target_node: get_map_string(map, "target_node"),
        max_connections: get_map_int(map, "max_connections"),
        active_connections: get_map_int(map, "active_connections"),
        available_connections: get_map_int(map, "available_connections"),
      ))
    }
    False -> Error(InvalidPool)
  }
}

/// Result of batch operations with error aggregation
pub type BatchResult(e) {
  BatchResult(total: Int, successful: Int, failed: Int, errors: List(e))
}

// ============================================================================
// Internal helpers
// ============================================================================

fn unwrap_pool(result: Dynamic) -> Pool {
  element(2, result)
}

fn unwrap_connection(result: Dynamic) -> Connection {
  element(2, result)
}

fn unwrap_stats_map(result: Dynamic) -> Dynamic {
  element(2, result)
}

@external(erlang, "maps", "get")
fn maps_get(key: a, map: Dynamic) -> b

fn get_map_string(map: Dynamic, key: String) -> String {
  maps_get(string_to_atom(key), map)
}

fn get_map_int(map: Dynamic, key: String) -> Int {
  maps_get(string_to_atom(key), map)
}

@external(erlang, "erlang", "binary_to_atom")
fn string_to_atom(s: String) -> a
