/// Global process registry for cross-node name resolution.
///
/// This module wraps Erlang's `:global` module to provide cluster-wide
/// process registration. Registered names are visible across all connected nodes.
///
/// When a process is registered globally, it can be looked up by name from any
/// node in the cluster. If a network partition occurs, the registry will
/// eventually resolve conflicts when the partition heals.
import distribute/codec
import distribute/global
import distribute/log
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/string

/// Re-export Pid from gleam/erlang/process for API compatibility.
/// Use `gleam/erlang/process.Pid` directly in new code.
pub type Pid =
  process.Pid

pub type RegisterError {
  /// Name is already registered by another process.
  AlreadyRegistered
  /// Process is not alive or invalid.
  InvalidProcess
  /// Name contains invalid characters or is too long.
  InvalidName(String)
  /// Network partition or connectivity issue.
  NetworkError(String)
  /// Generic registration failure.
  RegisterFailed(String)
}

@external(erlang, "registry_ffi", "register")
fn register_ffi(name: String, pid: Pid) -> Dynamic

@external(erlang, "registry_ffi", "unregister")
fn unregister_ffi(name: String) -> Dynamic

@external(erlang, "registry_ffi", "whereis")
fn whereis_ffi(name: String) -> Dynamic

@external(erlang, "registry_ffi", "is_ok_atom")
fn is_ok_atom(value: Dynamic) -> Bool

@external(erlang, "registry_ffi", "is_already_registered")
fn is_already_registered(value: Dynamic) -> Bool

@external(erlang, "registry_ffi", "get_error_reason")
fn get_error_reason(value: Dynamic) -> String

@external(erlang, "registry_ffi", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "registry_ffi", "dynamic_to_pid")
fn dynamic_to_pid(value: Dynamic) -> Pid

@external(erlang, "registry_ffi", "store_subject")
fn store_subject_ffi(name: String, subject: process.Subject(msg)) -> Dynamic

@external(erlang, "registry_ffi", "get_subject")
fn get_subject_ffi(name: String) -> Dynamic

@external(erlang, "registry_ffi", "remove_subject")
fn remove_subject_ffi(name: String) -> Dynamic

/// Classify error reason into structured RegisterError
fn classify_register_error(reason: String) -> RegisterError {
  case reason {
    "already_registered" -> AlreadyRegistered
    "invalid_process" -> InvalidProcess
    "invalid_name" -> InvalidName("Name contains invalid characters")
    _ ->
      case is_network_error(reason) {
        True -> NetworkError(reason)
        False -> RegisterFailed(reason)
      }
  }
}

/// Convert RegisterError to string for logging
fn classify_register_error_to_string(error: RegisterError) -> String {
  case error {
    AlreadyRegistered -> "already_registered"
    InvalidProcess -> "invalid_process"
    InvalidName(msg) -> "invalid_name: " <> msg
    NetworkError(msg) -> "network_error: " <> msg
    RegisterFailed(msg) -> "register_failed: " <> msg
  }
}

/// Check if error is network-related
fn is_network_error(reason: String) -> Bool {
  string.contains(reason, "partition")
  || string.contains(reason, "network")
  || string.contains(reason, "connection")
}

/// Validate registry name
fn validate_name(name: String) -> Result(Nil, RegisterError) {
  case string.length(name) {
    0 -> Error(InvalidName("Name cannot be empty"))
    len if len > 255 -> Error(InvalidName("Name too long (max 255 chars)"))
    _ ->
      case string.contains(name, " ") {
        True -> Error(InvalidName("Name cannot contain spaces"))
        False -> Ok(Nil)
      }
  }
}

/// Register a typed Subject globally.
///
/// This registers the owner Pid of the subject.
///
/// Recommended: Use global.GlobalSubject and register it with this function.
/// The GlobalSubject pattern ensures type-safe messaging with encoder/decoder.
///
/// For GlobalSubject: Create with global.new(encoder, decoder), then register
/// with register_typed(name, global.subject(global_subject)).
///
/// For custom Subject: Works but requires clients to know the message format.
pub fn register_typed(
  name: String,
  subject: process.Subject(msg),
) -> Result(Nil, RegisterError) {
  case process.subject_owner(subject) {
    Ok(pid) -> register(name, pid)
    Error(Nil) -> Error(InvalidProcess)
  }
}

/// Register a Subject globally (alias for register_typed).
@deprecated("Use register_typed instead")
pub fn register_subject(
  name: String,
  subject: process.Subject(a),
) -> Result(Nil, RegisterError) {
  register_typed(name, subject)
}

/// Register a process globally under the given name.
pub fn register(name: String, pid: Pid) -> Result(Nil, RegisterError) {
  log.debug("Registering global name", [#("name", name)])
  case validate_name(name) {
    Error(e) -> {
      log.warn("Name validation failed during registration", [
        #("name", name),
        #("error", classify_register_error_to_string(e)),
      ])
      Error(e)
    }
    Ok(_) -> {
      let res = register_ffi(name, pid)
      case is_ok_atom(res) {
        True -> {
          log.info("Successfully registered global name", [#("name", name)])
          Ok(Nil)
        }
        False ->
          case is_already_registered(res) {
            True -> {
              log.warn("Name already registered", [#("name", name)])
              Error(AlreadyRegistered)
            }
            False -> {
              let error = classify_register_error(get_error_reason(res))
              log.error("Failed to register global name", [
                #("name", name),
                #("error", classify_register_error_to_string(error)),
              ])
              Error(error)
            }
          }
      }
    }
  }
}

/// Unregister a globally registered name.
pub fn unregister(name: String) -> Result(Nil, RegisterError) {
  let res = unregister_ffi(name)
  case is_ok_atom(res) {
    True -> Ok(Nil)
    False -> Error(classify_register_error(get_error_reason(res)))
  }
}

/// Look up a globally registered process by name.
/// Returns Ok(pid) if found, Error(Nil) otherwise.
pub fn whereis(name: String) -> Result(Pid, Nil) {
  log.debug("Resolving global name", [#("name", name)])
  let res = whereis_ffi(name)
  case is_pid(res) {
    True -> Ok(dynamic_to_pid(res))
    False -> Error(Nil)
  }
}

/// Look up a globally registered GlobalSubject (RECOMMENDED).
/// 
/// This is the type-safe way to lookup distributed processes.
/// Returns a GlobalSubject that enforces encoder/decoder usage.
///
/// On success, returns a GlobalSubject that can send/receive typed messages.
/// On error, returns Error(Nil) if the name is not registered.
pub fn whereis_global(
  name: String,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> Result(global.GlobalSubject(msg), Nil) {
  case whereis(name) {
    Ok(pid) -> Ok(global.from_pid(pid, encoder, decoder))
    Error(Nil) -> Error(Nil)
  }
}

/// Look up a globally registered process with explicit tag.
///
/// Use this when you know the tag of the remote process (e.g., for custom actors).
/// For GlobalSubject, use whereis_global instead (recommended).
///
/// The tag parameter should match the tag used by the remote actor.
/// For most cases with Nil tags, use dynamic.nil() as the tag.
pub fn whereis_with_tag(
  name: String,
  tag: Dynamic,
) -> Result(process.Subject(msg), Nil) {
  case whereis(name) {
    Ok(pid) -> Ok(process.unsafely_create_subject(pid, tag))
    Error(Nil) -> Error(Nil)
  }
}

/// Look up a globally registered process and return a typed Subject with Nil tag.
///
/// ⚠️ **DEPRECATED**: Use `whereis_global` for GlobalSubject or `whereis_with_tag` 
/// for standard actors where you know the tag.
///
/// The returned Subject has a `Nil` tag and won't work with standard gleam_otp actors.
@deprecated("Use whereis_global for GlobalSubject or whereis_with_tag for custom actors")
pub fn whereis_typed(name: String) -> Result(process.Subject(msg), Nil) {
  whereis_with_tag(name, dynamic.nil())
}

// ============================================================================
// Subject Storage API
// ============================================================================
// These functions store/retrieve complete Subject values (including their tag)
// using persistent_term. This is necessary for OTP actors where the tag must
// be preserved for message routing to work correctly.

/// Store a complete Subject by name.
///
/// Unlike `register_typed` which only stores the Pid in Erlang's global registry,
/// this function stores the entire Subject including its unique tag. This is
/// essential for OTP actors where the tag is used for message pattern matching.
///
/// Use this when you need to retrieve the exact same Subject later, for example
/// when implementing singleton actors that need to be looked up by name.
///
/// The Subject is stored using `persistent_term` which is optimized for
/// read-heavy, rarely-changing data.
pub fn store_subject(
  name: String,
  subject: process.Subject(msg),
) -> Result(Nil, RegisterError) {
  case validate_name(name) {
    Error(e) -> Error(e)
    Ok(_) -> {
      let _ = store_subject_ffi(name, subject)
      Ok(Nil)
    }
  }
}

/// Retrieve a stored Subject by name.
///
/// Returns the exact Subject that was stored with `store_subject`, preserving
/// the original tag. This allows proper message routing for OTP actors.
///
/// Returns Error(Nil) if no Subject is stored under this name.
pub fn lookup_subject(name: String) -> Result(process.Subject(msg), Nil) {
  let result = get_subject_ffi(name)
  case is_ok_tuple(result) {
    True -> Ok(extract_subject(result))
    False -> Error(Nil)
  }
}

/// Remove a stored Subject by name.
///
/// This removes the Subject from persistent_term storage. Note that this
/// does NOT unregister the process from Erlang's global registry - use
/// `unregister` for that.
pub fn remove_stored_subject(name: String) -> Nil {
  let _ = remove_subject_ffi(name)
  Nil
}

@external(erlang, "registry_ffi", "is_ok_tuple")
fn is_ok_tuple(value: Dynamic) -> Bool

@external(erlang, "registry_ffi", "extract_subject")
fn extract_subject(value: Dynamic) -> process.Subject(msg)

// ============================================================================
// Convenience Wrappers for Common Patterns
// ============================================================================

/// Try to register a GlobalSubject, automatically extracting its underlying Subject.
///
/// This is a convenience wrapper that combines the most common pattern:
/// 1. Extract the Subject(BitArray) from GlobalSubject
/// 2. Register it using register_typed
///
/// **Example:**
/// ```gleam
/// let global_subject = actor.start_typed_actor(init, loop, encoder, decoder)
/// register_global(global_subject, "my-service")
/// ```
pub fn register_global(
  global_subject: global.GlobalSubject(msg),
  name: String,
) -> Result(Nil, RegisterError) {
  register_typed(name, global.subject(global_subject))
}

/// Synchronous registration with retry logic.
///
/// Attempts to register a GlobalSubject, retrying up to `max_retries` times
/// if network errors occur. Useful in distributed environments with transient
/// connectivity issues.
///
/// **Parameters:**
/// - `global_subject`: The GlobalSubject to register
/// - `name`: The global name to register under
/// - `max_retries`: Maximum number of retry attempts (default: 3)
/// - `retry_delay_ms`: Delay between retries in milliseconds (default: 100)
///
/// Returns Ok(Nil) on success, Error(RegisterError) if all retries fail.
pub fn register_with_retry(
  global_subject: global.GlobalSubject(msg),
  name: String,
  max_retries: Int,
  retry_delay_ms: Int,
) -> Result(Nil, RegisterError) {
  do_register_with_retry(
    global_subject,
    name,
    max_retries,
    retry_delay_ms,
    0,
  )
}

fn do_register_with_retry(
  global_subject: global.GlobalSubject(msg),
  name: String,
  max_retries: Int,
  retry_delay_ms: Int,
  attempt: Int,
) -> Result(Nil, RegisterError) {
  case register_global(global_subject, name) {
    Ok(Nil) -> Ok(Nil)
    Error(NetworkError(_)) if attempt < max_retries -> {
      log.warn("Registration attempt failed, retrying...", [
        #("name", name),
        #("attempt", int_to_string(attempt + 1)),
        #("max_retries", int_to_string(max_retries)),
      ])
      process.sleep(retry_delay_ms)
      do_register_with_retry(
        global_subject,
        name,
        max_retries,
        retry_delay_ms,
        attempt + 1,
      )
    }
    Error(err) -> Error(err)
  }
}

/// Lookup a GlobalSubject by name (async-friendly version).
///
/// This is a convenience wrapper that simplifies the common pattern of
/// looking up a GlobalSubject with encoder/decoder.
///
/// **Example:**
/// ```gleam
/// case lookup_global("my-service", my_encoder(), my_decoder()) {
///   Ok(service) -> global.send(service, MyMessage)
///   Error(_) -> io.println("Service not found")
/// }
/// ```
pub fn lookup_global(
  name: String,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> Result(global.GlobalSubject(msg), Nil) {
  whereis_global(name, encoder, decoder)
}

/// Synchronous lookup with timeout.
///
/// Attempts to lookup a GlobalSubject, retrying until found or timeout.
/// Useful when waiting for a service to become available.
///
/// **Parameters:**
/// - `name`: The global name to lookup
/// - `encoder`: Encoder for the message type
/// - `decoder`: Decoder for the message type
/// - `timeout_ms`: Maximum time to wait in milliseconds
/// - `poll_interval_ms`: Time between lookup attempts in milliseconds
///
/// Returns Ok(GlobalSubject) if found, Error(Nil) on timeout.
pub fn lookup_with_timeout(
  name: String,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Result(global.GlobalSubject(msg), Nil) {
  let start_time = erlang_system_time_ms()
  do_lookup_with_timeout(
    name,
    encoder,
    decoder,
    start_time,
    timeout_ms,
    poll_interval_ms,
  )
}

fn do_lookup_with_timeout(
  name: String,
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
  start_time: Int,
  timeout_ms: Int,
  poll_interval_ms: Int,
) -> Result(global.GlobalSubject(msg), Nil) {
  case lookup_global(name, encoder, decoder) {
    Ok(subject) -> Ok(subject)
    Error(_) -> {
      let elapsed = erlang_system_time_ms() - start_time
      case elapsed >= timeout_ms {
        True -> {
          log.warn("Lookup timeout exceeded", [
            #("name", name),
            #("timeout_ms", int_to_string(timeout_ms)),
          ])
          Error(Nil)
        }
        False -> {
          process.sleep(poll_interval_ms)
          do_lookup_with_timeout(
            name,
            encoder,
            decoder,
            start_time,
            timeout_ms,
            poll_interval_ms,
          )
        }
      }
    }
  }
}

/// Check if a name is currently registered.
///
/// This is more efficient than whereis when you only need to check
/// existence without creating a Subject.
pub fn is_registered(name: String) -> Bool {
  case whereis(name) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Unregister and remove stored subject in one call.
///
/// Convenience wrapper that combines:
/// 1. Unregister from global registry
/// 2. Remove from persistent_term storage
///
/// Useful for complete cleanup of a named actor.
pub fn unregister_and_remove(name: String) -> Result(Nil, RegisterError) {
  let _ = remove_stored_subject(name)
  unregister(name)
}

// ============================================================================
// Helper Functions
// ============================================================================

@external(erlang, "erlang", "system_time")
fn erlang_system_time(unit: Dynamic) -> Int

fn erlang_system_time_ms() -> Int {
  erlang_system_time(millisecond_atom())
}

@external(erlang, "registry_ffi", "millisecond_atom")
fn millisecond_atom() -> Dynamic

fn int_to_string(i: Int) -> String {
  // This would normally use gleam/int.to_string
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    _ -> "N"
  }
}
