# Connection Pool Module

Connection pooling for distributed Gleam/Erlang applications. The connection pool module provides efficient management of connections to remote nodes with automatic resource management and batch operations.

## Overview

The Connection Pool module provides:

- **Pooled connections** - Reuse connections to avoid overhead of establishing new ones
- **Automatic resource management** - Connections are properly returned to the pool after use
- **Batch operations** - Send multiple messages efficiently
- **Parallel processing** - Execute batch operations concurrently
- **Pool statistics** - Monitor pool health and usage
- **Stress testing** - Built-in tools for performance testing

## Quick Start

### Creating a Connection Pool

```gleam
import distribute/connection_pool.{type Pool, type PoolError}

pub fn setup_pool() -> Result(Pool, PoolError) {
  let target_node = "worker@node.example.com"
  let max_connections = 10
  
  case connection_pool.new(target_node, max_connections) {
    Ok(pool) -> {
      io.println("Pool created with 10 connections")
      Ok(pool)
    }
    Error(connection_pool.InvalidConfig) -> {
      io.println("Invalid pool configuration")
      Error(connection_pool.InvalidConfig)
    }
    Error(connection_pool.ConnectionFailed) -> {
      io.println("Failed to connect to target node")
      Error(connection_pool.ConnectionFailed)
    }
    Error(err) -> Error(err)
  }
}
```

### Using Connections

```gleam
import distribute/connection_pool

pub fn send_request(pool: connection_pool.Pool, data: String) {
  connection_pool.with_connection(pool, fn(conn) {
    // Use the connection
    let result = send_via_connection(conn, data)
    result
  })
}
```

### Batch Operations

```gleam
import distribute/connection_pool

pub fn send_many_messages(pool: connection_pool.Pool, messages: List(String)) {
  let send_fn = fn(conn, msg) {
    send_via_connection(conn, msg)
  }
  
  case connection_pool.send_batch(pool, messages, send_fn) {
    Ok(results) -> {
      io.println("Sent " <> int.to_string(list.length(results)) <> " messages")
      Ok(results)
    }
    Error(connection_pool.PoolExhausted) -> {
      io.println("Pool exhausted, try again later")
      Error(connection_pool.PoolExhausted)
    }
    Error(err) -> Error(err)
  }
}
```

## API Reference

### Types

#### Pool

```gleam
pub opaque type Pool
```

An opaque type representing a connection pool. Created with `new()` and must be destroyed with `destroy()` when no longer needed.

#### Connection

```gleam
pub opaque type Connection
```

An opaque type representing a single connection from the pool. Connections are automatically managed and should not be stored outside of `with_connection` callbacks.

#### PoolStats

```gleam
pub type PoolStats {
  PoolStats(
    total_connections: Int,
    available_connections: Int,
    in_use_connections: Int,
    failed_operations: Int,
    successful_operations: Int,
  )
}
```

Statistics about pool usage and health.

#### PoolError

```gleam
pub type PoolError {
  PoolExhausted
  ConnectionFailed
  InvalidConfig
  InvalidPool
}
```

Error types that can occur during pool operations:

- `PoolExhausted` - No connections available in the pool
- `ConnectionFailed` - Failed to establish or use a connection
- `InvalidConfig` - Invalid pool configuration parameters
- `InvalidPool` - The pool reference is invalid or has been destroyed

#### BatchResult

```gleam
pub type BatchResult {
  BatchResult(
    successful: List(Dynamic),
    failed: List(#(Int, PoolError)),
  )
}
```

Result of a parallel batch operation, containing both successful results and failures with their indices.

### Functions

#### new

```gleam
pub fn new(
  target_node: String,
  max_connections: Int,
) -> Result(Pool, PoolError)
```

Creates a new connection pool to the specified node.

**Parameters:**
- `target_node` - The Erlang node name to connect to
- `max_connections` - Maximum number of connections to maintain

**Returns:** A `Result` containing the pool or an error.

**Example:**
```gleam
let assert Ok(pool) = connection_pool.new("app@server.local", 5)
```

#### with_connection

```gleam
pub fn with_connection(
  pool: Pool,
  func: fn(Connection) -> a,
) -> Result(a, PoolError)
```

Executes a function with a connection from the pool. The connection is automatically returned to the pool when the function completes.

**Parameters:**
- `pool` - The connection pool
- `func` - A function that uses the connection

**Returns:** The result of the function wrapped in a `Result`.

**Example:**
```gleam
connection_pool.with_connection(pool, fn(conn) {
  send_message(conn, "Hello")
})
```

#### stats

```gleam
pub fn stats(pool: Pool) -> Result(PoolStats, PoolError)
```

Returns current statistics about the pool.

**Parameters:**
- `pool` - The connection pool

**Returns:** Pool statistics or an error.

#### destroy

```gleam
pub fn destroy(pool: Pool) -> Result(Nil, PoolError)
```

Destroys the pool and closes all connections. After calling this, the pool should not be used.

**Parameters:**
- `pool` - The connection pool to destroy

**Returns:** `Ok(Nil)` on success.

#### send_batch

```gleam
pub fn send_batch(
  pool: Pool,
  messages: List(msg),
  send_fn: fn(Connection, msg) -> result,
) -> Result(List(result), PoolError)
```

Sends multiple messages sequentially using pool connections.

**Parameters:**
- `pool` - The connection pool
- `messages` - List of messages to send
- `send_fn` - Function that sends a single message using a connection

**Returns:** A list of results for each message, or an error.

#### send_batch_parallel

```gleam
pub fn send_batch_parallel(
  pool: Pool,
  messages: List(msg),
  send_fn: fn(Connection, msg) -> result,
) -> Result(BatchResult, PoolError)
```

Sends multiple messages in parallel using available pool connections.

**Parameters:**
- `pool` - The connection pool
- `messages` - List of messages to send
- `send_fn` - Function that sends a single message using a connection

**Returns:** A `BatchResult` containing successful results and any failures.

#### stress_test

```gleam
pub fn stress_test(
  pool: Pool,
  ops_per_worker: Int,
  workers: Int,
) -> Result(PoolStats, PoolError)
```

Runs a stress test on the pool to verify performance under load.

**Parameters:**
- `pool` - The connection pool to test
- `ops_per_worker` - Number of operations each worker should perform
- `workers` - Number of concurrent workers

**Returns:** Pool statistics after the test completes.

## Usage Patterns

### Pattern 1: Request Handler with Pooled Connections

Use a pool for handling multiple concurrent requests:

```gleam
import distribute/connection_pool

pub fn start_request_handler(target_node: String) {
  let assert Ok(pool) = connection_pool.new(target_node, 20)
  
  // Handler function for incoming requests
  fn(request: Request) -> Response {
    case connection_pool.with_connection(pool, fn(conn) {
      forward_request(conn, request)
    }) {
      Ok(response) -> response
      Error(connection_pool.PoolExhausted) -> {
        Response(status: 503, body: "Service busy")
      }
      Error(_) -> {
        Response(status: 500, body: "Connection error")
      }
    }
  }
}
```

### Pattern 2: Bulk Data Synchronization

Efficiently sync data across nodes:

```gleam
import distribute/connection_pool

pub fn sync_records(pool: connection_pool.Pool, records: List(Record)) {
  let send_fn = fn(conn, record) {
    serialize_and_send(conn, record)
  }
  
  case connection_pool.send_batch_parallel(pool, records, send_fn) {
    Ok(batch_result) -> {
      let success_count = list.length(batch_result.successful)
      let failure_count = list.length(batch_result.failed)
      
      io.println(
        "Synced " <> int.to_string(success_count) <> 
        " records, " <> int.to_string(failure_count) <> " failed"
      )
      
      // Retry failed records
      case batch_result.failed {
        [] -> Ok(Nil)
        failures -> retry_failed(pool, records, failures)
      }
    }
    Error(err) -> Error(err)
  }
}
```

### Pattern 3: Pool Health Monitoring

Monitor pool health and scale as needed:

```gleam
import distribute/connection_pool

pub fn monitor_pool_health(pool: connection_pool.Pool) {
  case connection_pool.stats(pool) {
    Ok(stats) -> {
      let utilization = 
        int.to_float(stats.in_use_connections) /. 
        int.to_float(stats.total_connections)
      
      case utilization >. 0.8 {
        True -> {
          io.println("Warning: Pool utilization above 80%")
          // Consider scaling or alerting
        }
        False -> Nil
      }
      
      case stats.failed_operations > 100 {
        True -> {
          io.println("Warning: High failure rate")
          // Investigate connection issues
        }
        False -> Nil
      }
    }
    Error(connection_pool.InvalidPool) -> {
      io.println("Pool has been destroyed")
    }
    Error(_) -> {
      io.println("Failed to get pool stats")
    }
  }
}
```

## Best Practices

1. **Size pools appropriately** - Start with a pool size equal to expected concurrent operations, then tune based on `stats()` output.

2. **Always destroy pools** - Call `destroy()` when your application shuts down to clean up resources properly.

3. **Handle PoolExhausted gracefully** - This error means all connections are busy. Implement backpressure, queuing, or return a "service busy" response.

4. **Use batch operations for bulk work** - When sending many messages, `send_batch` or `send_batch_parallel` is more efficient than multiple `with_connection` calls.

5. **Don't hold connections** - Keep operations inside `with_connection` short. Long-running operations block connections from being reused.

6. **Monitor pool statistics** - Regularly check `stats()` to detect pool exhaustion or connection failures before they become critical.

7. **Use parallel batch for independent operations** - `send_batch_parallel` is faster when messages don't depend on each other.

8. **Stress test before production** - Use `stress_test()` to validate your pool configuration handles expected load.

9. **Create pools at startup** - Creating pools has overhead. Create them once at application startup, not per-request.

10. **One pool per target node** - Create separate pools for different target nodes rather than sharing.

## Performance Considerations

| Pool Size | Use Case |
|-----------|----------|
| 1-5 | Low-volume, latency-tolerant workloads |
| 5-20 | Standard request handling |
| 20-50 | High-throughput batch processing |
| 50+ | Extreme throughput (benchmark first) |

Memory usage scales linearly with pool size. Each connection maintains buffers and state.

## Related Modules

- [Messaging](../messaging/README.md) - Higher-level message passing
- [Discovery](../discovery/README.md) - Finding nodes to connect to
- [Cluster](../cluster/README.md) - Cluster-wide connection management
