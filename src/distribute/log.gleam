/// Structured logging for distributed systems.
///
/// This module provides structured logging capabilities with different levels,
/// metadata support, and correlation IDs for distributed tracing.
import gleam/list
import gleam/option
import distribute/settings

// Dynamic type used for ad-hoc FFI returns
type Dynamic

import gleam/string

// ============================================================================
// Types
// ============================================================================

/// Log levels from most verbose to least verbose
pub type Level {
  Debug
  Info
  Warn
  Error
  Off
  // Special level that disables all logging
}

/// Structured log entry with metadata
pub type LogEntry {
  LogEntry(Level, String, List(#(String, String)), option.Option(String), Int)
}

/// Logger configuration
pub type Config {
  Config(level: Level, enable_colors: Bool, enable_timestamps: Bool)
}

// ============================================================================
// Configuration
// ============================================================================

/// Default logger configuration
const default_config = Config(
  level: Info,
  enable_colors: True,
  enable_timestamps: True,
)

// ============================================================================
// Core logging functions
// ============================================================================

/// Log a debug message
pub fn debug(message: String, metadata: List(#(String, String))) -> Nil {
  log(Debug, message, metadata, option.None)
}

/// Log an info message
pub fn info(message: String, metadata: List(#(String, String))) -> Nil {
  log(Info, message, metadata, option.None)
}

/// Log a warning message
pub fn warn(message: String, metadata: List(#(String, String))) -> Nil {
  log(Warn, message, metadata, option.None)
}

/// Log an error message
pub fn error(message: String, metadata: List(#(String, String))) -> Nil {
  log(Error, message, metadata, option.None)
}

/// Log a debug message with correlation ID
pub fn debug_with_correlation(
  message: String,
  metadata: List(#(String, String)),
  correlation_id: String,
) -> Nil {
  log(Debug, message, metadata, option.Some(correlation_id))
}

/// Log an info message with correlation ID
pub fn info_with_correlation(
  message: String,
  metadata: List(#(String, String)),
  correlation_id: String,
) -> Nil {
  log(Info, message, metadata, option.Some(correlation_id))
}

/// Log a warning message with correlation ID
pub fn warn_with_correlation(
  message: String,
  metadata: List(#(String, String)),
  correlation_id: String,
) -> Nil {
  log(Warn, message, metadata, option.Some(correlation_id))
}

/// Log an error message with correlation ID
pub fn error_with_correlation(
  message: String,
  metadata: List(#(String, String)),
  correlation_id: String,
) -> Nil {
  log(Error, message, metadata, option.Some(correlation_id))
}

/// Core logging function
pub fn log(
  level: Level,
  message: String,
  metadata: List(#(String, String)),
  correlation_id: option.Option(String),
) -> Nil {
  case should_log(level) {
    True -> {
      let entry =
        LogEntry(level, message, metadata, correlation_id, get_timestamp())
      output_log_entry(entry)
    }
    False -> Nil
  }
}

// ============================================================================
// Correlation ID management
// ============================================================================

/// Execute a function with a specific correlation ID
pub fn with_correlation_id(_id: String, f: fn() -> a) -> a {
  // For now, we just execute the function
  // In a more advanced implementation, we could use process dictionary
  f()
}

/// Generate a new correlation ID
pub fn generate_correlation_id() -> String {
  case settings.is_use_crypto_ids() {
    True -> {
      let res = crypto_rand_base64_safe_ffi(8)
      case crypto_is_ok_ffi(res) {
        True -> {
          let v = element(2, res)
          "corr-" <> v
        }
        False -> fallback_correlation_id()
      }
    }
    False -> fallback_correlation_id()
  }
}

fn fallback_correlation_id() -> String {
  // Use timestamp plus monotonic time for uniqueness
  let ts = get_timestamp()
  let monotonic = erlang_monotonic_time(1_000_000)
  let seed = monotonic % 100_000
  "corr-" <> int_to_string(ts) <> "-" <> int_to_string(seed)
}

// ============================================================================
// Configuration
// ============================================================================

/// Disable all logging output (useful for tests)
pub fn disable_logging() -> Nil {
  set_persistent_term("distribute_log_disabled", True)
}

/// Enable logging output
pub fn enable_logging() -> Nil {
  set_persistent_term("distribute_log_disabled", False)
}

/// Set the logging backend implementation used to handle output.
/// Use "console" (default) or "erlang_logger".
pub fn set_backend(backend: String) -> Nil {
  logger_ffi_set_backend(backend)
}

/// Set the minimum log level
/// Messages below this level will not be logged
pub fn set_level(level: Level) -> Nil {
  set_persistent_term_level("distribute_log_level", level_to_int(level))
}

/// Get the current minimum log level
pub fn get_level() -> Level {
  let level_int = get_persistent_term_level("distribute_log_level", 1)
  int_to_level(level_int)
}

/// Convert int to Level
fn int_to_level(i: Int) -> Level {
  case i {
    0 -> Debug
    1 -> Info
    2 -> Warn
    3 -> Error
    _ -> Off
  }
}

/// Check if logging is disabled
fn is_logging_disabled() -> Bool {
  get_persistent_term("distribute_log_disabled", False)
}

/// Check if a level should be logged based on current configuration
fn should_log(level: Level) -> Bool {
  case is_logging_disabled() {
    True -> False
    False -> {
      let current_level = get_level()
      case current_level {
        Off -> False
        _ -> level_to_int(level) >= level_to_int(current_level)
      }
    }
  }
}

// ============================================================================
// Formatting and output
// ============================================================================

/// Output a log entry to the console
fn output_log_entry(entry: LogEntry) -> Nil {
  let formatted = format_log_entry(entry)
  let LogEntry(level, _, metadata, correlation_id, _) = entry
  let meta_str = case metadata {
    [] -> ""
    _ -> format_metadata(metadata)
  }
  let meta_with_corr = case correlation_id {
    option.Some(id) ->
      case string.length(meta_str) > 0 {
        True -> "corr=" <> id <> "," <> meta_str
        False -> "corr=" <> id
      }
    option.None -> meta_str
  }
  logger_ffi_log(level_to_name(level), formatted <> "\n", meta_with_corr)
}

fn level_to_name(level: Level) -> String {
  case level {
    Debug -> "DEBUG"
    Info -> "INFO"
    Warn -> "WARN"
    Error -> "ERROR"
    Off -> "OFF"
  }
}

/// Format a log entry as a string
pub fn format_log_entry(entry: LogEntry) -> String {
  let LogEntry(level, message, metadata, correlation_id, timestamp) = entry
  let level_str = format_level(level)
  let timestamp_str = case default_config.enable_timestamps {
    True -> "[" <> int_to_string(timestamp) <> "] "
    False -> ""
  }
  let correlation_str = case correlation_id {
    option.Some(id) -> "[" <> id <> "] "
    option.None -> ""
  }
  let metadata_str = case metadata {
    [] -> ""
    _ -> " " <> format_metadata(metadata)
  }

  timestamp_str
  <> correlation_str
  <> level_str
  <> ": "
  <> message
  <> metadata_str
}

/// Format log level with optional colors
fn format_level(level: Level) -> String {
  let level_str = case level {
    Debug -> "DEBUG"
    Info -> "INFO"
    Warn -> "WARN"
    Error -> "ERROR"
    Off -> "OFF"
  }

  case default_config.enable_colors {
    True -> colorize_level(level, level_str)
    False -> level_str
  }
}

/// Add ANSI colors to log levels
fn colorize_level(level: Level, text: String) -> String {
  case level {
    Debug -> "\u{001b}[36m" <> text <> "\u{001b}[0m"
    // Cyan
    Info -> "\u{001b}[32m" <> text <> "\u{001b}[0m"
    // Green
    Warn -> "\u{001b}[33m" <> text <> "\u{001b}[0m"
    // Yellow
    Error -> "\u{001b}[31m" <> text <> "\u{001b}[0m"
    // Red
    Off -> text
    // No color for Off level
  }
}

/// Format metadata as key=value pairs
fn format_metadata(metadata: List(#(String, String))) -> String {
  metadata
  |> list.map(fn(pair) { pair.0 <> "=" <> pair.1 })
  |> string.join(", ")
}

// ============================================================================
// Utilities
// ============================================================================

/// Convert level to integer for comparison
fn level_to_int(level: Level) -> Int {
  case level {
    Debug -> 0
    Info -> 1
    Warn -> 2
    Error -> 3
    Off -> 4
    // Higher than any logging level
  }
}

/// Get current timestamp (milliseconds since epoch)
fn get_timestamp() -> Int {
  erlang_system_time(1000)
}

// ============================================================================
// FFI declarations
// ============================================================================

// io_put_chars is now called via logger_ffi in Erlang; remove direct external

@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: Int) -> Int

@external(erlang, "erlang", "monotonic_time")
fn erlang_monotonic_time(unit: Int) -> Int

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(int: Int) -> String

@external(erlang, "persistent_term", "put")
fn set_persistent_term(key: String, value: Bool) -> Nil

@external(erlang, "persistent_term", "get")
fn get_persistent_term(key: String, default: Bool) -> Bool

@external(erlang, "persistent_term", "put")
fn set_persistent_term_level(key: String, value: Int) -> Nil

@external(erlang, "persistent_term", "get")
fn get_persistent_term_level(key: String, default: Int) -> Int

@external(erlang, "logger_ffi", "log")
fn logger_ffi_log(level: String, msg: String, meta: String) -> Nil

@external(erlang, "logger_ffi", "set_backend")
fn logger_ffi_set_backend(backend: String) -> Nil

@external(erlang, "crypto_ffi", "rand_base64_safe")
fn crypto_rand_base64_safe_ffi(n: Int) -> Dynamic

@external(erlang, "crypto_ffi", "is_ok")
fn crypto_is_ok_ffi(v: Dynamic) -> Bool

@external(erlang, "erlang", "element")
fn element(index: Int, tuple: Dynamic) -> a
// Use rand_base64_safe_ffi instead in calls to handle errors gracefully
// remove the dynamic crypto ffi declarations as they're no longer used
