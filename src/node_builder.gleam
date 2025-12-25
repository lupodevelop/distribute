/// Fluent API builder for distributed node configuration.
///
/// This module provides a builder pattern for configuring and starting
/// distributed nodes with a more ergonomic API.
import cluster
import gleam/list
import gleam/option
import gleam/string
import log

// ============================================================================
// Types
// ============================================================================

/// Node configuration builder
pub type NodeBuilder {
  NodeBuilder(
    name: option.Option(String),
    cookie: option.Option(String),
    peers: List(String),
    auto_register_services: Bool,
  )
}

/// Result of node startup operations
pub type NodeResult {
  NodeResult(
    node_started: Bool,
    connections: List(String),
    registered_services: List(String),
    errors: List(String),
  )
}

// ============================================================================
// Builder API
// ============================================================================

/// Create a new node builder with default settings
pub fn new() -> NodeBuilder {
  NodeBuilder(
    name: option.None,
    cookie: option.None,
    peers: [],
    auto_register_services: False,
  )
}

/// Set the node name
pub fn with_name(builder: NodeBuilder, name: String) -> NodeBuilder {
  NodeBuilder(..builder, name: option.Some(name))
}

/// Set the cookie
pub fn with_cookie(builder: NodeBuilder, cookie: String) -> NodeBuilder {
  NodeBuilder(..builder, cookie: option.Some(cookie))
}

/// Add peers to connect to
pub fn connect_to(builder: NodeBuilder, peers: List(String)) -> NodeBuilder {
  NodeBuilder(..builder, peers: list.append(builder.peers, peers))
}

/// Enable auto-registration of common services
pub fn with_auto_register(builder: NodeBuilder) -> NodeBuilder {
  NodeBuilder(..builder, auto_register_services: True)
}

/// Start the node with the configured settings
pub fn start(builder: NodeBuilder) -> Result(NodeResult, cluster.StartError) {
  log.info("Starting distributed node with builder", [
    #("has_name", bool_to_string(option.is_some(builder.name))),
    #("peer_count", int_to_string(list.length(builder.peers))),
    #("auto_register", bool_to_string(builder.auto_register_services)),
  ])

  // Validate configuration
  case validate_config(builder) {
    Ok(valid_config) -> {
      // Start the node
      let start_result = start_node(valid_config)

      case start_result.node_started {
        True -> {
          log.info("Node started successfully", [
            #("node", valid_config.name),
            #(
              "connections",
              int_to_string(list.length(start_result.connections)),
            ),
          ])

          // Connect to peers if specified
          let connect_result = connect_peers(valid_config.peers)

          // Register services if enabled
          let register_result = case valid_config.auto_register_services {
            True -> register_common_services()
            False -> []
          }

          let final_result =
            NodeResult(
              node_started: True,
              connections: connect_result,
              registered_services: register_result,
              errors: start_result.errors,
            )

          Ok(final_result)
        }
        False -> {
          log.error("Failed to start node", [
            #("errors", string.join(start_result.errors, ", ")),
          ])
          Error(cluster.StartFailed(string.join(start_result.errors, ", ")))
        }
      }
    }
    Error(error) -> {
      log.error("Configuration validation failed", [#("error", error)])
      // Map builder validation errors to StartError variants
      // Missing name or cookie -> InvalidNodeName
      Error(cluster.InvalidNodeName(error))
    }
  }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Validate builder configuration
fn validate_config(builder: NodeBuilder) -> Result(ValidConfig, String) {
  case builder.name {
    option.None -> Error("Node name is required")
    option.Some(name) -> {
      case builder.cookie {
        option.None -> Error("Cookie is required")
        option.Some(cookie) -> {
          Ok(ValidConfig(
            name: name,
            cookie: cookie,
            peers: builder.peers,
            auto_register_services: builder.auto_register_services,
          ))
        }
      }
    }
  }
}

/// Validated configuration
type ValidConfig {
  ValidConfig(
    name: String,
    cookie: String,
    peers: List(String),
    auto_register_services: Bool,
  )
}

/// Start the node
fn start_node(config: ValidConfig) -> StartupResult {
  case cluster.start_node(config.name, config.cookie) {
    Ok(Nil) -> StartupResult(node_started: True, errors: [], connections: [])
    Error(error) -> {
      let error_str = case error {
        cluster.InvalidNodeName(msg) -> "Invalid node name: " <> msg
        cluster.AlreadyStarted -> "Node already started"
        cluster.CookieTooLong -> "Cookie too long"
        cluster.NetworkError(msg) -> "Network error: " <> msg
        cluster.SystemError(msg) -> "System error: " <> msg
        cluster.StartFailed(msg) -> "Start failed: " <> msg
      }
      StartupResult(node_started: False, errors: [error_str], connections: [])
    }
  }
}

/// Startup result
type StartupResult {
  StartupResult(
    node_started: Bool,
    errors: List(String),
    connections: List(String),
  )
}

/// Connect to peer nodes
fn connect_peers(peers: List(String)) -> List(String) {
  list.fold(peers, [], fn(acc, peer) {
    case cluster.connect(peer) {
      Ok(_) -> {
        log.debug("Connected to peer", [#("peer", peer)])
        [peer, ..acc]
      }
      Error(_) -> {
        log.warn("Failed to connect to peer", [#("peer", peer)])
        acc
      }
    }
  })
}

/// Register common services
fn register_common_services() -> List(String) {
  let services = [
    #("logger", "log_service"),
    #("monitor", "monitor_service"),
    #("election", "election_service"),
  ]

  list.fold(services, [], fn(acc, service_pair) {
    let #(module, name) = service_pair
    // In a real implementation, this would register actual service processes
    log.debug("Auto-registered service", [#("name", name), #("module", module)])
    [name, ..acc]
  })
}

// ============================================================================
// Utility functions
// ============================================================================

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

fn int_to_string(i: Int) -> String {
  // Simple implementation - in real Gleam this would use the int module
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    _ -> "N"
    // Placeholder for larger numbers
  }
}
// ============================================================================
// FFI and external dependencies
// ============================================================================

// These would be actual FFI calls in a real implementation
// For now, they're placeholders
