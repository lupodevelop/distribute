/// Practical registry patterns for actor discovery and coordination.
///
/// This example demonstrates:
/// - Synchronous vs asynchronous actor registration
/// - Name-based actor lookup
/// - Registry integration with supervision
/// - Global actor discovery across nodes
import distribute/actor
import distribute/codec
import distribute/global
import distribute/registry
import gleam/erlang/process
import gleam/io
import gleam/otp/actor as otp_actor
import gleam/result

// ===== Message Types =====

pub type ServiceMessage {
  Request(data: String, reply: process.Subject(String))
  Health(reply: process.Subject(Bool))
}

// ===== Basic Encoders/Decoders =====

fn service_encoder() -> codec.Encoder(ServiceMessage) {
  fn(msg) {
    case msg {
      Request(data, _) -> codec.encode(codec.string(), data)
      Health(_) -> codec.encode(codec.string(), "health")
    }
  }
}

fn service_decoder() -> codec.Decoder(ServiceMessage) {
  fn(bits) {
    // Simplified
    Ok(Health(process.new_subject()))
  }
}

// ===== Actor Implementation =====

type ServiceState {
  ServiceState(name: String, requests: Int)
}

fn service_init(
  name: String,
) -> otp_actor.InitResult(ServiceState, ServiceMessage) {
  let selector = process.new_selector()
  otp_actor.Ready(ServiceState(name, 0), selector)
}

fn service_loop(
  msg: ServiceMessage,
  state: ServiceState,
) -> otp_actor.Next(ServiceMessage, ServiceState) {
  case msg {
    Request(data, reply) -> {
      let response = state.name <> " processed: " <> data
      process.send(reply, response)
      otp_actor.continue(ServiceState(..state, requests: state.requests + 1))
    }
    Health(reply) -> {
      process.send(reply, True)
      otp_actor.continue(state)
    }
  }
}

// ===== Example 1: Start and Register Separately =====

/// Traditional two-step approach: start actor, then register.
pub fn example_manual_registration() {
  io.println("=== Example: Manual Registration ===")

  // Step 1: Start the actor
  let actor_result =
    actor.start_typed_actor(
      init: fn() { service_init("manual-service") },
      loop: service_loop,
      encoder: service_encoder(),
      decoder: service_decoder(),
    )

  case actor_result {
    Ok(service_subject) -> {
      io.println("✓ Actor started")

      // Step 2: Register manually
      case
        registry.register_typed(
          "manual-service",
          global.subject(service_subject),
        )
      {
        Ok(_) -> {
          io.println("✓ Actor registered as 'manual-service'")

          // Step 3: Lookup by name
          case registry.whereis_global("manual-service") {
            Ok(found_subject) -> {
              io.println("✓ Successfully looked up by name")
              use_service(found_subject)
            }
            Error(_) -> io.println("✗ Lookup failed")
          }
        }
        Error(err) -> io.println("✗ Registration failed")
      }
    }
    Error(_) -> io.println("✗ Actor start failed")
  }
}

// ===== Example 2: Start and Register in One Call =====

/// Convenience wrapper: start and register in a single call.
pub fn example_convenience_registration() {
  io.println("\n=== Example: Convenience Registration ===")

  // Single call to start AND register
  let result =
    actor.start_typed_actor_registered(
      name: "convenience-service",
      init: fn() { service_init("convenience-service") },
      loop: service_loop,
      encoder: service_encoder(),
      decoder: service_decoder(),
    )

  case result {
    Ok(service_subject) -> {
      io.println("✓ Actor started and registered in one call")

      // Already registered - can lookup immediately
      case registry.whereis_global("convenience-service") {
        Ok(found) -> {
          io.println("✓ Lookup successful")
          use_service(found)
        }
        Error(_) -> io.println("✗ Lookup failed")
      }
    }
    Error(registry.AlreadyRegistered) -> io.println("✗ Name already taken")
    Error(_) -> io.println("✗ Registration failed")
  }
}

// ===== Example 3: Supervised and Registered =====

/// Combine supervision and registration for production-ready actors.
pub fn example_supervised_registered() {
  io.println("\n=== Example: Supervised + Registered ===")

  // Start with supervision
  let supervised_result =
    actor.start_typed_actor_supervised(
      init: fn() { service_init("supervised-service") },
      loop: service_loop,
      encoder: service_encoder(),
      decoder: service_decoder(),
    )

  case supervised_result {
    Ok(#(supervisor_pid, service_subject)) -> {
      io.println("✓ Actor started under supervision")

      // Register after supervision is confirmed
      case
        registry.register_typed(
          "supervised-service",
          global.subject(service_subject),
        )
      {
        Ok(_) -> {
          io.println("✓ Supervised actor registered")

          // Now it's fault-tolerant AND discoverable
          case registry.whereis_global("supervised-service") {
            Ok(found) -> {
              io.println("✓ Service is discoverable and supervised")
              use_service(found)
            }
            Error(_) -> io.println("✗ Lookup failed")
          }
        }
        Error(_) -> io.println("✗ Registration failed")
      }
    }
    Error(_) -> io.println("✗ Supervision start failed")
  }
}

// ===== Example 4: Registry-Based Service Discovery =====

/// Use the registry as a service directory for multiple actors.
pub fn example_service_directory() {
  io.println("\n=== Example: Service Directory ===")

  // Register multiple services
  let services = [
    #("auth-service", "Authentication"),
    #("db-service", "Database"),
    #("cache-service", "Cache"),
  ]

  io.println("Registering services...")

  list_each(services, fn(service_info) {
    let #(name, description) = service_info
    let result =
      actor.start_typed_actor_registered(
        name: name,
        init: fn() { service_init(description) },
        loop: service_loop,
        encoder: service_encoder(),
        decoder: service_decoder(),
      )

    case result {
      Ok(_) -> io.println("  ✓ Registered: " <> name)
      Error(_) -> io.println("  ✗ Failed: " <> name)
    }
  })

  // Discover and use services
  io.println("\nDiscovering services...")
  list_each(services, fn(service_info) {
    let #(name, _) = service_info
    case registry.whereis_global(name) {
      Ok(service) -> {
        io.println("  ✓ Found: " <> name)
        use_service(service)
      }
      Error(_) -> io.println("  ✗ Not found: " <> name)
    }
  })
}

// ===== Example 5: Async Registration Pattern =====

/// Non-blocking registration with callback pattern.
pub fn example_async_registration() {
  io.println("\n=== Example: Async Registration ===")

  // Start actor first
  case
    actor.start_typed_actor(
      init: fn() { service_init("async-service") },
      loop: service_loop,
      encoder: service_encoder(),
      decoder: service_decoder(),
    )
  {
    Ok(service_subject) -> {
      io.println("✓ Actor started")

      // Register in background (non-blocking)
      spawn_register_task(service_subject, "async-service")

      io.println("✓ Registration initiated asynchronously")

      // Continue with other work...
      io.println("✓ Continuing with other work")

      // Later, check if registration succeeded
      process.sleep(100)
      case registry.whereis_global("async-service") {
        Ok(_) -> io.println("✓ Async registration completed")
        Error(_) -> io.println("⏳ Registration still pending")
      }
    }
    Error(_) -> io.println("✗ Actor start failed")
  }
}

// ===== Helper Functions =====

fn use_service(service: global.GlobalSubject(ServiceMessage)) {
  let reply = process.new_subject()
  global.send(service, Request("test data", reply))
  case process.receive(reply, 1000) {
    Ok(response) -> io.println("  → Service response: " <> response)
    Error(_) -> io.println("  ✗ No response from service")
  }
}

fn spawn_register_task(
  subject: global.GlobalSubject(ServiceMessage),
  name: String,
) {
  process.start(
    fn() {
      let _ = registry.register_typed(name, global.subject(subject))
      Nil
    },
    False,
  )
  Nil
}

fn list_each(list: List(a), f: fn(a) -> b) -> Nil {
  // Placeholder
  Nil
}

// ===== Main Demo =====

pub fn main() {
  io.println("======================================")
  io.println("Distribute Registry Patterns")
  io.println("======================================\n")

  // Enable dynamic atom creation
  distribute_settings_set_allow_atom_creation(True)

  example_manual_registration()
  example_convenience_registration()
  example_supervised_registered()
  example_service_directory()
  example_async_registration()

  io.println("\n======================================")
  io.println("All registry patterns demonstrated!")
  io.println("======================================")
}

@external(erlang, "distribute_settings_ffi", "set_allow_atom_creation")
fn distribute_settings_set_allow_atom_creation(allow: Bool) -> Nil
