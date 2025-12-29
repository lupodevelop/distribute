import distribute/capability.{type NodeCapabilities}
import distribute/codec
import distribute/registry/actor as registry
import distribute/registry/behaviour
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Find the highest compatible version for a protocol given local and remote capabilities.
/// Returns Some(version) if a compatible version exists in the overlapping range,
/// None if the protocol is not supported by both sides or ranges don't overlap.
///
/// ## Algorithm
/// 1. Find capability for `protocol` in local capabilities
/// 2. Find capability for `protocol` in remote capabilities
/// 3. If both exist, compute overlap: max(local.min, remote.min) to min(local.max, remote.max)
/// 4. If overlap is valid (min <= max), return the max version of the overlap
/// 5. Otherwise return None
pub fn protocol_negotiate(
  local_caps: NodeCapabilities,
  remote_caps: NodeCapabilities,
  protocol: String,
) -> Option(Int) {
  // Find local capability for this protocol
  let local_cap =
    list.find(local_caps, fn(cap) { capability.protocol(cap) == protocol })

  // Find remote capability for this protocol
  let remote_cap =
    list.find(remote_caps, fn(cap) { capability.protocol(cap) == protocol })

  case local_cap, remote_cap {
    Ok(local), Ok(remote) -> {
      let local_min = capability.min_version(local)
      let local_max = capability.max_version(local)
      let remote_min = capability.min_version(remote)
      let remote_max = capability.max_version(remote)

      // Compute overlap: [max(local_min, remote_min), min(local_max, remote_max)]
      let overlap_min = int.max(local_min, remote_min)
      let overlap_max = int.min(local_max, remote_max)

      // If overlap is valid (min <= max), return max version
      case overlap_min <= overlap_max {
        True -> Some(overlap_max)
        False -> None
      }
    }
    _, _ -> None
  }
}

/// Validate that a list of capabilities is well-formed.
/// Returns Ok(Nil) if all capabilities have min <= max and non-empty protocol names.
/// Returns Error(reason) if any capability is invalid.
pub fn validate_capabilities(caps: NodeCapabilities) -> Result(Nil, String) {
  list.try_each(caps, fn(cap) {
    let protocol = capability.protocol(cap)
    let min = capability.min_version(cap)
    let max = capability.max_version(cap)

    case protocol {
      "" -> Error("Empty protocol name in capability")
      _ ->
        case min <= max {
          True -> Ok(Nil)
          False ->
            Error(
              "Capability '"
              <> protocol
              <> "': min version "
              <> int.to_string(min)
              <> " > max version "
              <> int.to_string(max),
            )
        }
    }
  })
}

/// Encode a value for a specific node using the negotiated version.
///
/// This function looks up the negotiated version for the given protocol from
/// the registry, selects the appropriate schema version, and encodes the value.
///
/// ## Parameters
/// - `registry`: The registry actor subject to lookup negotiated versions
/// - `timeout_ms`: Timeout for registry lookup in milliseconds
/// - `node_id`: Target node identifier
/// - `protocol`: Protocol name to use for version lookup
/// - `schemas`: List of available schemas for this protocol, ordered by version
/// - `value`: The value to encode
///
/// ## Returns
/// - `Ok(binary)` with encoded data using the negotiated schema
/// - `Error(reason)` if negotiation info not found or encoding fails
pub fn schema_encode_for_node(
  registry: process.Subject(registry.RegistryCommand),
  timeout_ms: Int,
  node_id: String,
  protocol: String,
  schemas: List(codec.Schema(a)),
  value: a,
) -> Result(BitArray, String) {
  // Lookup node metadata from registry
  case registry.lookup_sync(registry, timeout_ms, node_id) {
    Ok(metadata) -> {
      // Extract negotiated versions
      let negotiated_versions = extract_negotiated_versions(metadata.extra)

      // Find negotiated version for this protocol
      case list.key_find(negotiated_versions, protocol) {
        Ok(version) -> {
          // Find schema with matching version
          case find_schema_by_version(schemas, version) {
            Some(schema) -> {
              case codec.schema_encode(schema, value) {
                Ok(binary) -> Ok(binary)
                Error(e) -> Error(codec.encode_error_to_string(e))
              }
            }
            None ->
              Error(
                "No schema found for protocol '"
                <> protocol
                <> "' version "
                <> int.to_string(version),
              )
          }
        }
        Error(_) ->
          Error("No negotiated version found for protocol '" <> protocol <> "'")
      }
    }
    Error(behaviour.NotFound) ->
      Error("Node '" <> node_id <> "' not found in registry")
    Error(behaviour.InvalidArgument(msg)) -> Error("Invalid argument: " <> msg)
    Error(behaviour.AdapterFailure(msg)) ->
      Error("Registry adapter failure: " <> msg)
    Error(behaviour.AlreadyExists) -> Error("Unexpected AlreadyExists error")
  }
}

/// Decode a value from a specific node using the negotiated version.
///
/// Similar to `schema_encode_for_node`, but for decoding. Looks up the
/// negotiated version and uses the appropriate schema decoder.
pub fn schema_decode_from_node(
  registry: process.Subject(registry.RegistryCommand),
  timeout_ms: Int,
  node_id: String,
  protocol: String,
  schemas: List(codec.Schema(a)),
  binary: BitArray,
) -> Result(a, String) {
  case registry.lookup_sync(registry, timeout_ms, node_id) {
    Ok(metadata) -> {
      let negotiated_versions = extract_negotiated_versions(metadata.extra)

      case list.key_find(negotiated_versions, protocol) {
        Ok(version) -> {
          case find_schema_by_version(schemas, version) {
            Some(schema) -> {
              case codec.schema_decode(schema, binary) {
                Ok(value) -> Ok(value)
                Error(e) -> Error(codec.decode_error_to_string(e))
              }
            }
            None ->
              Error(
                "No schema found for protocol '"
                <> protocol
                <> "' version "
                <> int.to_string(version),
              )
          }
        }
        Error(_) ->
          Error("No negotiated version found for protocol '" <> protocol <> "'")
      }
    }
    Error(behaviour.NotFound) ->
      Error("Node '" <> node_id <> "' not found in registry")
    Error(behaviour.InvalidArgument(msg)) -> Error("Invalid argument: " <> msg)
    Error(behaviour.AdapterFailure(msg)) ->
      Error("Registry adapter failure: " <> msg)
    Error(behaviour.AlreadyExists) -> Error("Unexpected AlreadyExists error")
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Extract negotiated versions from metadata.extra field.
/// For now we parse a simple format: "proto1:v1,proto2:v2"
/// In a real implementation this would use proper JSON parsing.
fn extract_negotiated_versions(extra: String) -> List(#(String, Int)) {
  case extra {
    "" -> []
    _ -> {
      string.split(extra, ",")
      |> list.filter_map(fn(pair_str) {
        case string.split(pair_str, ":") {
          [protocol, version_str] ->
            case int.parse(version_str) {
              Ok(version) -> Ok(#(protocol, version))
              Error(_) -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      })
    }
  }
}

/// Find a schema with matching version.
fn find_schema_by_version(
  schemas: List(codec.Schema(a)),
  version: Int,
) -> Option(codec.Schema(a)) {
  list.find(schemas, fn(schema) { schema.version == version })
  |> option.from_result()
}
