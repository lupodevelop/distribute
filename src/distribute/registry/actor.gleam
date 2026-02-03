/// Actor adapter implementing the `distribute/registry/behaviour` contract.
///
/// This adapter provides a simple, OTP-friendly in-memory registry backed
/// by a local actor. It is intended as the default implementation for
/// development and testing. It is strongly typed and documented so it can be
/// swapped with other adapters (e.g. ETS) later.
import distribute/registry/behaviour
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification, worker}
import gleam/result
import gleam/string

pub type RegistryCommand {
  Register(String, behaviour.Metadata, process.Subject(Result(Nil, String)))
  Unregister(String, process.Subject(Result(Nil, String)))
  Lookup(String, process.Subject(Result(behaviour.Metadata, Nil)))
  ListAll(process.Subject(Result(List(String), Nil)))
}

/// Start the registry actor. Returns a `Subject(RegistryCommand)` which
/// accepts the commands defined above.
pub fn start() -> Result(process.Subject(RegistryCommand), actor.StartError) {
  actor.new([])
  |> actor.on_message(fn(state, msg) {
    case state, msg {
      items, Register(node_id, metadata, reply) -> {
        let new_state = upsert(items, node_id, metadata)
        process.send(reply, Ok(Nil))
        actor.continue(new_state)
      }
      items, Unregister(node_id, reply) -> {
        let new_state = remove(items, node_id)
        process.send(reply, Ok(Nil))
        actor.continue(new_state)
      }
      items, Lookup(node_id, reply) -> {
        case find(items, node_id) {
          Some(metadata) -> process.send(reply, Ok(metadata))
          None -> process.send(reply, Error(Nil))
        }
        actor.continue(items)
      }
      items, ListAll(reply) -> {
        let ids = list.map(items, with: fn(pair) { pair.0 })
        process.send(reply, Ok(ids))
        actor.continue(items)
      }
    }
  })
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

/// Start the registry actor returning the raw `actor.Started` result.
///
/// Useful for OTP supervision where the supervisor expects a start
/// function that returns `Result(actor.Started(data), actor.StartError)`.
pub fn start_link() -> Result(
  actor.Started(process.Subject(RegistryCommand)),
  actor.StartError,
) {
  actor.new([])
  |> actor.on_message(fn(state, msg) {
    case state, msg {
      items, Register(node_id, metadata, reply) -> {
        let new_state = upsert(items, node_id, metadata)
        process.send(reply, Ok(Nil))
        actor.continue(new_state)
      }
      items, Unregister(node_id, reply) -> {
        let new_state = remove(items, node_id)
        process.send(reply, Ok(Nil))
        actor.continue(new_state)
      }
      items, Lookup(node_id, reply) -> {
        case find(items, node_id) {
          Some(metadata) -> process.send(reply, Ok(metadata))
          None -> process.send(reply, Error(Nil))
        }
        actor.continue(items)
      }
      items, ListAll(reply) -> {
        let ids = list.map(items, with: fn(pair) { pair.0 })
        process.send(reply, Ok(ids))
        actor.continue(items)
      }
    }
  })
  |> actor.start()
}

/// Create a `ChildSpecification` for use with `gleam_otp` supervisors.
///
/// Example:
///
///     children: [distribute/registry/actor.child_spec()]
pub fn child_spec() -> ChildSpecification(process.Subject(RegistryCommand)) {
  worker(fn() { start_link() })
}

/// Typed synchronous helper: register node metadata via the registry actor.
pub fn register_sync(
  registry: process.Subject(RegistryCommand),
  timeout_ms: Int,
  node_id: String,
  metadata: behaviour.Metadata,
) -> Result(Nil, behaviour.RegistryError) {
  // Basic guardrails
  case string.length(node_id) {
    0 -> Error(behaviour.InvalidArgument("node_id cannot be empty"))
    len if len > 255 -> Error(behaviour.InvalidArgument("node_id too long"))
    _ ->
      // Ensure metadata.node_id matches provided node_id when possible
      case metadata {
        behaviour.Metadata(md_node, _, _) ->
          case md_node == node_id {
            True -> {
              let res =
                process.call(registry, timeout_ms, fn(reply) {
                  Register(node_id, metadata, reply)
                })
              case res {
                Ok(Nil) -> Ok(Nil)
                Error(_) -> Error(behaviour.AdapterFailure("actor failure"))
              }
            }
            False ->
              Error(behaviour.InvalidArgument("metadata.node_id mismatch"))
          }
      }
  }
}

/// Typed synchronous lookup helper.
pub fn lookup_sync(
  registry: process.Subject(RegistryCommand),
  timeout_ms: Int,
  node_id: String,
) -> Result(behaviour.Metadata, behaviour.RegistryError) {
  let res =
    process.call(registry, timeout_ms, fn(reply) { Lookup(node_id, reply) })
  case res {
    Ok(md) -> Ok(md)
    Error(_) -> Error(behaviour.NotFound)
  }
}

/// Typed synchronous unregister helper.
pub fn unregister_sync(
  registry: process.Subject(RegistryCommand),
  timeout_ms: Int,
  node_id: String,
) -> Result(Nil, behaviour.RegistryError) {
  let res =
    process.call(registry, timeout_ms, fn(reply) { Unregister(node_id, reply) })
  case res {
    Ok(Nil) -> Ok(Nil)
    Error(_) -> Error(behaviour.AdapterFailure("actor failure"))
  }
}

/// List known node ids synchronously.
pub fn list_nodes_sync(
  registry: process.Subject(RegistryCommand),
  timeout_ms: Int,
) -> List(String) {
  let res = process.call(registry, timeout_ms, fn(reply) { ListAll(reply) })
  case res {
    Ok(ids) -> ids
    Error(_) -> []
  }
}

// Helpers operating on a list of tuples: List(#(String, Metadata))
fn upsert(
  items: List(#(String, behaviour.Metadata)),
  node_id: String,
  metadata: behaviour.Metadata,
) -> List(#(String, behaviour.Metadata)) {
  let without = remove(items, node_id)
  [#(node_id, metadata), ..without]
}

fn remove(
  items: List(#(String, behaviour.Metadata)),
  node_id: String,
) -> List(#(String, behaviour.Metadata)) {
  list.filter(items, keeping: fn(pair) { pair.0 != node_id })
}

fn find(
  items: List(#(String, behaviour.Metadata)),
  node_id: String,
) -> Option(behaviour.Metadata) {
  let filtered = list.filter(items, keeping: fn(pair) { pair.0 == node_id })
  case list.first(filtered) {
    Ok(pair) ->
      case pair {
        #(_id, md) -> Some(md)
      }
    Error(_) -> None
  }
}
