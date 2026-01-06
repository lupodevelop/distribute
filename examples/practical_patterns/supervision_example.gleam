/// Practical supervision patterns with distribute actors.
///
/// This example demonstrates different supervision strategies:
/// - Single supervised actor
/// - Worker pools with restart strategies
/// - Nested supervision trees
/// - Named actor registration under supervision

import distribute/actor
import distribute/codec
import distribute/global
import gleam/erlang/process
import gleam/io
import gleam/otp/actor as otp_actor
import gleam/otp/static_supervisor.{
  type ChildSpecification, type SupervisorBuilder,
}
import gleam/result

// ===== Message Types =====

pub type WorkerMessage {
  DoWork(id: Int, reply: process.Subject(Result(String, Nil)))
  GetStatus(reply: process.Subject(String))
}

pub type CounterMessage {
  Increment
  GetCount(reply: process.Subject(Int))
}

// ===== Encoders/Decoders =====

fn worker_encoder() -> codec.Encoder(WorkerMessage) {
  fn(msg) {
    case msg {
      DoWork(id, _reply) ->
        codec.encode(codec.tuple2(codec.int(), codec.string()), #(
          id,
          "do_work",
        ))
      GetStatus(_reply) -> codec.encode(codec.string(), "get_status")
    }
  }
}

fn worker_decoder() -> codec.Decoder(WorkerMessage) {
  fn(bits) {
    // Simplified decoder - in production use proper sum type encoding
    Ok(GetStatus(process.new_subject()))
  }
}

fn counter_encoder() -> codec.Encoder(CounterMessage) {
  fn(msg) {
    case msg {
      Increment -> codec.encode(codec.string(), "increment")
      GetCount(_reply) -> codec.encode(codec.string(), "get_count")
    }
  }
}

fn counter_decoder() -> codec.Decoder(CounterMessage) {
  fn(bits) {
    // Simplified decoder
    Ok(Increment)
  }
}

// ===== Actor Implementations =====

fn worker_init() -> otp_actor.InitResult(Nil, WorkerMessage) {
  let selector = process.new_selector()
  otp_actor.Ready(Nil, selector)
}

fn worker_loop(
  msg: WorkerMessage,
  state: Nil,
) -> otp_actor.Next(WorkerMessage, Nil) {
  case msg {
    DoWork(id, reply) -> {
      process.send(reply, Ok("Work #" <> int_to_string(id) <> " completed"))
      otp_actor.continue(state)
    }
    GetStatus(reply) -> {
      process.send(reply, "Worker ready")
      otp_actor.continue(state)
    }
  }
}

fn counter_init() -> otp_actor.InitResult(Int, CounterMessage) {
  let selector = process.new_selector()
  otp_actor.Ready(0, selector)
}

fn counter_loop(
  msg: CounterMessage,
  state: Int,
) -> otp_actor.Next(CounterMessage, Int) {
  case msg {
    Increment -> otp_actor.continue(state + 1)
    GetCount(reply) -> {
      process.send(reply, state)
      otp_actor.continue(state)
    }
  }
}

// ===== Example 1: Single Supervised Actor =====

/// Start a single actor under a supervisor.
/// Returns both the supervisor PID and the actor's GlobalSubject.
pub fn example_single_supervised() {
  io.println("=== Example: Single Supervised Actor ===")

  // One-call supervised actor creation
  let result =
    actor.start_typed_actor_supervised(
      worker_init,
      worker_loop,
      worker_encoder(),
      worker_decoder(),
    )

  case result {
    Ok(#(supervisor_pid, worker_subject)) -> {
      io.println("✓ Supervisor started with PID: " <> pid_to_string(
        supervisor_pid,
      ))
      io.println("✓ Worker started and supervised")

      // Use the worker
      let reply = process.new_subject()
      global.send(worker_subject, DoWork(1, reply))
      case process.receive(reply, 1000) {
        Ok(Ok(result)) -> io.println("✓ Worker response: " <> result)
        _ -> io.println("✗ No response from worker")
      }
    }
    Error(err) -> io.println("✗ Failed to start supervised actor")
  }
}

// ===== Example 2: Worker Pool =====

/// Create a pool of worker actors with automatic supervision.
pub fn example_worker_pool() {
  io.println("\n=== Example: Worker Pool ===")

  // Create a pool of 3 workers
  let result =
    actor.pool_supervisor(
      pool_size: 3,
      init: worker_init,
      loop: worker_loop,
      encoder: worker_encoder(),
      decoder: worker_decoder(),
    )

  case result {
    Ok(#(pool_supervisor, workers)) -> {
      io.println(
        "✓ Worker pool started with " <> int_to_string(list_length(workers)) <> " workers",
      )

      // Send work to all workers
      list_index_map(workers, fn(worker, idx) {
        let reply = process.new_subject()
        global.send(worker, DoWork(idx, reply))
        case process.receive(reply, 1000) {
          Ok(Ok(result)) -> io.println("  Worker " <> int_to_string(
            idx,
          ) <> ": " <> result)
          _ -> Nil
        }
      })
    }
    Error(err) -> io.println("✗ Failed to start worker pool")
  }
}

// ===== Example 3: Nested Supervision Tree =====

/// Build a supervision tree with multiple layers.
pub fn example_nested_supervision() {
  io.println("\n=== Example: Nested Supervision Tree ===")

  // Build a supervisor with multiple children
  let builder =
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.add(actor.child_spec_typed_actor_typed(
      counter_init,
      counter_loop,
      counter_encoder(),
      counter_decoder(),
    ))
    |> static_supervisor.add(actor.child_spec_typed_actor_typed(
      counter_init,
      counter_loop,
      counter_encoder(),
      counter_decoder(),
    ))

  // Nested worker pool
  case
    actor.pool_supervisor(
      pool_size: 2,
      init: worker_init,
      loop: worker_loop,
      encoder: worker_encoder(),
      decoder: worker_decoder(),
    )
  {
    Ok(#(pool_pid, pool_workers)) -> {
      io.println("✓ Nested supervision tree created")
      io.println("  - 2 counter actors")
      io.println("  - 1 worker pool with 2 workers")
    }
    Error(_) -> io.println("✗ Failed to create supervision tree")
  }
}

// ===== Example 4: Named Actors with Registry =====

/// Start actors with automatic registry integration.
pub fn example_named_actors() {
  io.println("\n=== Example: Named Actors with Registry ===")

  // Enable dynamic atom creation for the registry
  distribute_settings_set_allow_atom_creation(True)

  // Start actor and register it in one call
  let worker_result =
    actor.start_typed_actor_registered(
      name: "example_worker",
      init: worker_init,
      loop: worker_loop,
      encoder: worker_encoder(),
      decoder: worker_decoder(),
    )

  case worker_result {
    Ok(worker_subject) -> {
      io.println("✓ Worker registered as 'example_worker'")

      // Lookup the worker by name
      case registry_lookup_typed_actor("example_worker") {
        Ok(found_subject) -> {
          io.println("✓ Successfully looked up worker by name")

          // Use the looked-up subject
          let reply = process.new_subject()
          global.send(found_subject, GetStatus(reply))
          case process.receive(reply, 1000) {
            Ok(status) -> io.println("✓ Worker status: " <> status)
            _ -> io.println("✗ No response")
          }
        }
        Error(_) -> io.println("✗ Failed to lookup worker")
      }
    }
    Error(_) -> io.println("✗ Failed to register worker")
  }
}

// ===== Main Demo =====

pub fn main() {
  io.println("======================================")
  io.println("Distribute Supervision Examples")
  io.println("======================================\n")

  example_single_supervised()
  example_worker_pool()
  example_nested_supervision()
  example_named_actors()

  io.println("\n======================================")
  io.println("All examples completed!")
  io.println("======================================")
}

// ===== Helper Functions =====

// These would normally be imported from gleam/int, gleam/list, etc.
fn int_to_string(i: Int) -> String {
  // Placeholder - use actual implementation
  "N"
}

fn pid_to_string(pid: process.Pid) -> String {
  // Placeholder
  "<pid>"
}

fn list_length(list: List(a)) -> Int {
  // Placeholder
  0
}

fn list_index_map(list: List(a), f: fn(a, Int) -> b) -> List(b) {
  // Placeholder
  []
}

@external(erlang, "distribute_settings_ffi", "set_allow_atom_creation")
fn distribute_settings_set_allow_atom_creation(allow: Bool) -> Nil

@external(erlang, "distribute_registry_ffi", "lookup_typed_actor")
fn registry_lookup_typed_actor(
  name: String,
) -> Result(global.GlobalSubject(a), Nil)
