# Settings Documentation

## Overview

The settings module provides **runtime configuration management** for distributed systems. It enables dynamic configuration, environment-specific settings, and runtime overrides without code changes.

### Key Features

- ✅ Environment-based configuration
- ✅ Runtime setting updates
- ✅ Type-safe configuration
- ✅ Default value fallback
- ✅ Watch for configuration changes
- ✅ Validation and constraints

---

## Quick Start

### Load Configuration

```gleam
import distribute/settings

pub fn load_config() -> Result(AppConfig, String) {
  case settings.load_from_env("DISTRIBUTE_") {
    Ok(config) -> {
      logger.info("Configuration loaded from environment")
      Ok(config)
    }
    Error(e) -> {
      logger.error("Config load failed: " <> string.inspect(e))
      Error("Failed to load config")
    }
  }
}
```

### Get a Setting

```gleam
import distribute/settings

pub fn get_database_url() -> Result(String, String) {
  case settings.get("DATABASE_URL") {
    Ok(value) -> {
      logger.info("Database URL: " <> value)
      Ok(value)
    }
    Error(_) -> {
      logger.warn("DATABASE_URL not set, using default")
      Ok("postgres://localhost/mydb")
    }
  }
}
```

### Watch for Changes

```gleam
import distribute/settings

pub fn setup_config_watcher() -> Nil {
  settings.watch("MAX_POOL_SIZE", fn(new_value) {
    logger.info("MAX_POOL_SIZE changed to: " <> new_value)
    update_pool_size(new_value |> int.parse() |> result.unwrap(20))
  })
}
```

---

## Core Concepts

### Settings Store

```gleam
pub type Settings {
  Settings(
    values: Dict(String, String),
    defaults: Dict(String, String),
    watchers: List(Watcher),
  )
}
```

### Setting Source

Where settings come from:

```gleam
pub type SettingSource {
  Environment  // Environment variables
  ConfigFile   // JSON/TOML file
  Runtime      // In-memory runtime override
  Default      // Fallback value
}
```

### Setting Types

```gleam
pub type SettingType {
  StringSetting(key: String, default: String)
  IntSetting(key: String, default: Int, min: Int, max: Int)
  BoolSetting(key: String, default: Bool)
  ListSetting(key: String, default: List(String))
}
```

---

## Patterns

### Pattern 1: Environment-Specific Configuration

```gleam
import distribute/settings

pub type Environment {
  Development
  Staging
  Production
}

pub fn load_environment_config(env: Environment) -> Result(AppConfig, String) {
  let prefix = case env {
    Development -> "DEV_"
    Staging -> "STAGING_"
    Production -> "PROD_"
  }
  
  case settings.load_from_env(prefix) {
    Ok(config) -> {
      logger.info("Loaded " <> case env {
        Development -> "development"
        Staging -> "staging"
        Production -> "production"
      } <> " configuration")
      Ok(config)
    }
    Error(e) -> Error(string.inspect(e))
  }
}
```

### Pattern 2: Configuration with Defaults

```gleam
import distribute/settings

pub fn get_setting_with_fallback(key: String, default: String) -> String {
  case settings.get(key) {
    Ok(value) -> {
      logger.info("Using " <> key <> " from config")
      value
    }
    Error(_) -> {
      logger.info("Using default for " <> key)
      default
    }
  }
}

pub fn setup_defaults() -> Nil {
  settings.set_default("PORT", "3000")
  settings.set_default("HOST", "localhost")
  settings.set_default("LOG_LEVEL", "info")
  settings.set_default("MAX_CONNECTIONS", "20")
}
```

### Pattern 3: Dynamic Configuration Update

```gleam
import distribute/settings

pub fn update_cluster_settings(new_settings: Dict(String, String)) -> Result(Nil, String) {
  list.try_each(dict.to_list(new_settings), fn(entry) {
    let #(key, value) = entry
    
    case settings.set(key, value) {
      Ok(_) -> {
        logger.info("Updated setting: " <> key <> " = " <> value)
        Ok(Nil)
      }
      Error(e) -> {
        logger.error("Failed to update " <> key <> ": " <> string.inspect(e))
        Error(string.inspect(e))
      }
    }
  })
}
```

### Pattern 4: Validated Settings

```gleam
import distribute/settings

pub type PoolConfig {
  PoolConfig(
    min_size: Int,
    max_size: Int,
    idle_timeout_ms: Int,
  )
}

pub fn get_pool_config() -> Result(PoolConfig, String) {
  // Get and validate settings
  let min_size = get_int_setting("POOL_MIN_SIZE", 5)
  let max_size = get_int_setting("POOL_MAX_SIZE", 20)
  let idle_timeout = get_int_setting("POOL_IDLE_TIMEOUT", 30000)
  
  // Validate constraints
  case min_size > max_size {
    True -> {
      logger.error("Invalid pool config: min > max")
      Error("Invalid configuration")
    }
    False -> {
      Ok(PoolConfig(
        min_size: min_size,
        max_size: max_size,
        idle_timeout_ms: idle_timeout,
      ))
    }
  }
}

fn get_int_setting(key: String, default: Int) -> Int {
  case settings.get(key) {
    Ok(str_value) -> {
      case int.parse(str_value) {
        Ok(value) -> value
        Error(_) -> {
          logger.warn("Invalid int for " <> key <> ", using default")
          default
        }
      }
    }
    Error(_) -> default
  }
}
```

### Pattern 5: Configuration Reload on Signal

```gleam
import distribute/settings

pub fn setup_config_reload() {
  // Reload config on SIGHUP or external trigger
  settings.on_reload_request(fn() {
    logger.info("Reloading configuration...")
    
    case settings.reload() {
      Ok(new_config) -> {
        logger.info("Configuration reloaded successfully")
        apply_new_settings(new_config)
      }
      Error(e) -> {
        logger.error("Reload failed: " <> string.inspect(e))
      }
    }
  })
}

pub fn apply_new_settings(config: Dict(String, String)) -> Nil {
  // Apply each setting change
  dict.each(config, fn(key, value) {
    case key {
      "LOG_LEVEL" -> {
        logger.info("Changing log level to: " <> value)
        logger.set_level(parse_log_level(value))
      }
      "MAX_POOL_SIZE" -> {
        logger.info("Changing pool size to: " <> value)
        update_pool_size(int.parse(value) |> result.unwrap(20))
      }
      _ -> Nil
    }
  })
}
```

### Pattern 6: Feature Flags via Settings

```gleam
import distribute/settings

pub type FeatureFlags {
  FeatureFlags(
    enable_encryption: Bool,
    enable_compression: Bool,
    enable_metrics: Bool,
  )
}

pub fn get_feature_flags() -> FeatureFlags {
  FeatureFlags(
    enable_encryption: get_bool_setting("FEATURE_ENCRYPTION", True),
    enable_compression: get_bool_setting("FEATURE_COMPRESSION", False),
    enable_metrics: get_bool_setting("FEATURE_METRICS", True),
  )
}

fn get_bool_setting(key: String, default: Bool) -> Bool {
  case settings.get(key) {
    Ok("true") | Ok("1") | Ok("yes") -> True
    Ok("false") | Ok("0") | Ok("no") -> False
    _ -> default
  }
}

pub fn is_feature_enabled(feature: String) -> Bool {
  case settings.get("FEATURE_" <> feature) {
    Ok("true") -> True
    _ -> False
  }
}
```

---

## Error Handling

### SettingsError Types

```gleam
pub type SettingsError {
  KeyNotFound(String)
  InvalidValue(String, String)  // key, expected_type
  ValidationFailed(String)
  ConfigFileNotFound
  ParseError(String)
  PermissionDenied
}
```

### Recovery Strategies

| Error | Meaning | Recovery |
|-------|---------|----------|
| `KeyNotFound` | Setting not configured | Use default value |
| `InvalidValue` | Wrong value type | Parse or use default |
| `ValidationFailed` | Value violates constraint | Log and use default |
| `ConfigFileNotFound` | Config file missing | Use environment vars |
| `ParseError` | Invalid format | Validate input |

---

## Best Practices

### 1. Always Provide Defaults

```gleam
// GOOD: Defaults handle missing settings
fn get_port() -> Int {
  case settings.get("PORT") {
    Ok(str) -> int.parse(str) |> result.unwrap(3000)
    Error(_) -> 3000
  }
}

// BAD: Crashes if setting missing
fn get_port() -> Int {
  settings.get("PORT")
  |> result.unwrap("")
  |> int.parse()
  |> result.unwrap(0)
}
```

### 2. Validate on Load

```gleam
// GOOD: Validate immediately
pub fn load_config() -> Result(AppConfig, String) {
  let port = get_int_setting("PORT", 3000)
  let host = settings.get("HOST") |> result.unwrap("localhost")
  
  case port < 1 || port > 65535 {
    True -> Error("Invalid port: " <> int.to_string(port))
    False -> Ok(AppConfig(host: host, port: port))
  }
}

// BAD: Invalid config succeeds
pub fn load_config() -> Result(AppConfig, String) {
  Ok(AppConfig(
    host: settings.get("HOST") |> result.unwrap("localhost"),
    port: -1,  // Invalid but not caught!
  ))
}
```

### 3. Document Configuration

```gleam
// GOOD: Clear configuration documentation
pub type AppConfig {
  AppConfig(
    // Database connection string
    // Env: DATABASE_URL
    // Default: postgres://localhost/mydb
    database_url: String,
    
    // Server port (1-65535)
    // Env: PORT
    // Default: 3000
    port: Int,
    
    // Log level (debug, info, warn, error)
    // Env: LOG_LEVEL
    // Default: info
    log_level: String,
  )
}
```

### 4. Use Environment Prefixes

```gleam
// GOOD: Organized environment variables
settings.load_from_env("DISTRIBUTE_")  // DISTRIBUTE_PORT, DISTRIBUTE_HOST, etc.

// BAD: No organization
settings.load_from_env("")  // PORT, HOST, DATABASE_URL - could conflict
```

### 5. Watch Critical Settings

```gleam
// GOOD: React to important changes
settings.watch("LOG_LEVEL", fn(value) {
  logger.set_level(parse_log_level(value))
})

settings.watch("MAX_CONNECTIONS", fn(value) {
  case int.parse(value) {
    Ok(new_max) -> adjust_connection_pool(new_max)
    Error(_) -> logger.warn("Invalid MAX_CONNECTIONS: " <> value)
  }
})

// BAD: Ignore changes to critical settings
// Settings silently change with no effect
```

---

## Integration with Other Modules

### Settings + Connection Pool

```gleam
import distribute/settings
import distribute/connection_pool

pub fn create_db_pool_from_config() -> Result(connection_pool.Pool, String) {
  let min_size = get_int_setting("DB_POOL_MIN", 5)
  let max_size = get_int_setting("DB_POOL_MAX", 20)
  
  let config = connection_pool.PoolConfig(
    name: "database",
    min_size: min_size,
    max_size: max_size,
    idle_timeout_ms: 30000,
    max_queue_size: 100,
  )
  
  connection_pool.create(config, fn() {
    let db_url = settings.get("DATABASE_URL") |> result.unwrap("postgres://localhost/mydb")
    open_db_connection(db_url)
  })
}
```

### Settings + Actor

```gleam
import distribute/settings
import distribute/actor

pub fn create_configured_actor(actor_config_key: String) -> Result(Any, String) {
  case settings.get(actor_config_key) {
    Ok(config_str) -> {
      let initial_state = parse_actor_config(config_str)
      actor.start_typed_actor(initial_state, handle_message)
    }
    Error(_) -> Error("Actor config not found: " <> actor_config_key)
  }
}
```

### Settings + Cluster

```gleam
import distribute/settings
import distribute/cluster

pub fn join_cluster_from_config() -> Result(Nil, String) {
  let node_name = settings.get("NODE_NAME") |> result.unwrap("node@localhost")
  let seed_nodes = settings.get("SEED_NODES")
    |> result.unwrap("node1@host1,node2@host2")
    |> string.split(",")
  
  cluster.join(cluster.new(), node_name, seed_nodes)
}
```

---

## Configuration File Example

### JSON Format

```json
{
  "distribute": {
    "port": 3000,
    "host": "0.0.0.0",
    "log_level": "info",
    "database": {
      "url": "postgres://localhost/mydb",
      "pool": {
        "min_size": 5,
        "max_size": 20
      }
    },
    "cluster": {
      "node_name": "node1@localhost",
      "seed_nodes": ["node2@localhost", "node3@localhost"]
    }
  }
}
```

### Environment Variables

```bash
# Production configuration
export DISTRIBUTE_PORT=3000
export DISTRIBUTE_HOST=0.0.0.0
export DISTRIBUTE_LOG_LEVEL=warn
export DATABASE_URL=postgres://prod-db/mydb
export DB_POOL_MIN=10
export DB_POOL_MAX=50
export NODE_NAME=node1@prod-cluster
export SEED_NODES=node2@prod-cluster,node3@prod-cluster
export FEATURE_ENCRYPTION=true
export FEATURE_COMPRESSION=true
```

---

## Troubleshooting

### Settings Not Loading from Environment

```gleam
// Issue: Settings from env not picked up
// Solution: Check environment variable names

case settings.load_from_env("DISTRIBUTE_") {
  Ok(_) -> logger.info("Settings loaded")
  Error(e) -> {
    logger.error("Load failed: " <> string.inspect(e))
    // Debug: Print available env vars
    logger.info("Available env vars: " <> string.inspect(get_env_vars()))
  }
}
```

### Configuration Validation Failures

```gleam
// Issue: Settings load but validation fails
// Solution: Check constraints

pub fn validate_config(config: Dict(String, String)) -> Result(Nil, String) {
  let port = int.parse(dict.get(config, "PORT") |> result.unwrap("3000"))
    |> result.unwrap(3000)
  
  case port > 0 && port < 65536 {
    True -> Ok(Nil)
    False -> Error("Port must be 1-65535, got " <> int.to_string(port))
  }
}
```

---

## Performance Considerations

### Setting Lookup Speed

| Lookup Type | Time | Notes |
|------------|------|-------|
| In-memory cache | <1μs | Fastest |
| Environment var | 1-10μs | Moderate |
| File read | 1-100ms | Slow |
| Remote config | 10-1000ms | Slowest |

### Optimization Tips

1. **Cache settings** — Don't look up repeatedly
2. **Load once at startup** — Minimize I/O
3. **Use in-memory store** — For frequent access
4. **Batch updates** — When changing multiple settings

---

## Next Steps

- [Connection Pool](../connection-pool/) — For resource configuration
- [Cluster](../cluster/) — For cluster settings
- [Actor](../actor/) — For actor configuration

---

## References

- [12-Factor App Configuration](https://12factor.net/config)
- [Environment Variable Best Practices](https://www.12factor.net/backing-services)

*For runtime monitoring of configuration, see [Monitor](../monitor/).*
