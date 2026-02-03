/// Advanced example: SWIM-based service coordinator with failure detection.
///
/// This example demonstrates how to build a distributed coordinator that:
/// - Uses SWIM membership for failure detection
/// - Automatically detects and removes failed services
/// - Maintains a registry of available services
/// - Provides high availability through automatic failover
import distribute/actor
import distribute/cluster
import distribute/cluster/membership
import distribute/codec
import distribute/global
import distribute/receiver
import distribute/registry
import distribute/settings
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

/// Service registration message
pub type CoordinatorMsg {
  RegisterService(
    name: String,
    capabilities: List(String),
    reply: process.Subject(Result(Nil, String)),
  )
  UnregisterService(name: String)
  GetAvailableServices(reply: process.Subject(List(ServiceInfo)))
  CheckHealth
}

/// Service information
pub type ServiceInfo {
  ServiceInfo(
    name: String,
    node: String,
    capabilities: List(String),
    status: membership.Status,
  )
}

/// Coordinator state
type State {
  State(services: List(ServiceInfo), check_interval_ms: Int)
}

/// Message encoder
fn encoder() -> codec.Encoder(CoordinatorMsg) {
  fn(msg: CoordinatorMsg) -> Result(BitArray, codec.EncodeError) {
    case msg {
      RegisterService(_, _, _) -> Ok(<<0>>)
      UnregisterService(_) -> Ok(<<1>>)
      GetAvailableServices(_) -> Ok(<<2>>)
      CheckHealth -> Ok(<<3>>)
    }
  }
}

/// Message decoder
fn decoder() -> codec.Decoder(CoordinatorMsg) {
  fn(binary: BitArray) -> Result(CoordinatorMsg, codec.DecodeError) {
    case binary {
      <<0>> -> {
        let reply = process.new_subject()
        Ok(RegisterService("default", [], reply))
      }
      <<1>> -> Ok(UnregisterService("default"))
      <<2>> -> {
        let reply = process.new_subject()
        Ok(GetAvailableServices(reply))
      }
      <<3>> -> Ok(CheckHealth)
      _ -> Error(codec.InvalidBinary("Unknown coordinator message"))
    }
  }
}

/// Handle coordinator messages
fn handle_message(msg: CoordinatorMsg, state: State) -> receiver.Next(State) {
  case msg {
    RegisterService(name, capabilities, reply) -> {
      io.println("Registering service: " <> name)
      let node = cluster.node_name()
      let new_service = ServiceInfo(name, node, capabilities, membership.Alive)
      let new_services = [new_service, ..state.services]
      process.send(reply, Ok(Nil))
      receiver.Continue(State(..state, services: new_services))
    }

    UnregisterService(name) -> {
      io.println("Unregistering service: " <> name)
      let filtered = list.filter(state.services, fn(s) { s.name != name })
      receiver.Continue(State(..state, services: filtered))
    }

    GetAvailableServices(reply) -> {
      // Filter services by SWIM membership status
      let alive_nodes = membership.alive()
      let available =
        list.filter(state.services, fn(s) { list.contains(alive_nodes, s.node) })
      process.send(reply, available)
      receiver.Continue(state)
    }

    CheckHealth -> {
      // Periodic health check using SWIM
      let alive_nodes = membership.alive()
      let suspect_nodes = membership.suspect()

      io.println("=== Health Check ===")
      io.println("Alive nodes: " <> string.inspect(alive_nodes))
      io.println("Suspect nodes: " <> string.inspect(suspect_nodes))

      // Update service statuses based on SWIM
      let updated_services =
        list.map(state.services, fn(service) {
          let status = case list.contains(alive_nodes, service.node) {
            True -> membership.Alive
            False ->
              case list.contains(suspect_nodes, service.node) {
                True -> membership.Suspect
                False -> membership.Dead
              }
          }
          ServiceInfo(..service, status: status)
        })

      // Remove dead services
      let cleaned =
        list.filter(updated_services, fn(s) {
          case s.status {
            membership.Dead -> False
            _ -> True
          }
        })

      receiver.Continue(State(..state, services: cleaned))
    }
  }
}

/// Start the SWIM coordinator
pub fn start() -> Result(global.GlobalSubject(CoordinatorMsg), actor.ActorError) {
  // Start SWIM membership service (5 second interval)
  membership.start_service(5000)

  // Start coordinator actor
  let initial_state = State(services: [], check_interval_ms: 10_000)
  let coordinator =
    actor.start_typed_actor(initial_state, encoder(), decoder(), handle_message)

  // Register globally
  let name = "swim_coordinator"
  case registry.register_typed(name, global.subject(coordinator)) {
    Ok(_) -> {
      io.println("✓ SWIM Coordinator registered as '" <> name <> "'")
      Ok(coordinator)
    }
    Error(_) ->
      Error(actor.InvalidConfiguration("Failed to register coordinator"))
  }
}

/// Example: Start coordinator under supervision
pub fn start_supervised() -> Result(
  #(process.Pid, global.GlobalSubject(CoordinatorMsg)),
  actor.ActorError,
) {
  settings.set_allow_atom_creation(True)
  membership.start_service(5000)

  let initial_state = State(services: [], check_interval_ms: 10_000)

  case
    actor.start_typed_actor_supervised(
      initial_state,
      encoder(),
      decoder(),
      handle_message,
    )
  {
    Ok(#(sup_pid, coordinator)) -> {
      io.println("✓ SWIM Coordinator started under supervision")
      Ok(#(sup_pid, coordinator))
    }
    Error(err) -> Error(actor.StartFailed(err))
  }
}

/// Example: Register a service
pub fn register_service(
  coordinator: global.GlobalSubject(CoordinatorMsg),
  name: String,
  capabilities: List(String),
) -> Result(Nil, String) {
  let reply = process.new_subject()
  let msg = RegisterService(name, capabilities, reply)

  case global.send(coordinator, msg) {
    Ok(_) -> {
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error("Timeout waiting for registration response")
      }
    }
    Error(_) -> Error("Failed to send registration message")
  }
}

/// Example: Get available services
pub fn get_services(
  coordinator: global.GlobalSubject(CoordinatorMsg),
) -> Result(List(ServiceInfo), String) {
  let reply = process.new_subject()
  let msg = GetAvailableServices(reply)

  case global.send(coordinator, msg) {
    Ok(_) -> {
      case process.receive(reply, 5000) {
        Ok(services) -> Ok(services)
        Error(_) -> Error("Timeout waiting for services list")
      }
    }
    Error(_) -> Error("Failed to send get services message")
  }
}
