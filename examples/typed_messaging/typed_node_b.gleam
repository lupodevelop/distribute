/// Node B: Type-safe messaging receiver example
///
/// This node demonstrates:
/// - Using GlobalSubject with integrated codecs
/// - Registering globally and receiving typed messages
/// - Type-safe process groups
import distribute/cluster
import distribute/codec
import distribute/global
import distribute/groups
import distribute/messaging
import distribute/node_builder
import distribute/registry
import gleam/erlang/process
import gleam/int
import gleam/io

pub fn main() {
  // Start distributed node
  io.println("=== Starting Node B (Typed Messaging Example) ===")

  let assert Ok(_) =
    node_builder.new()
    |> node_builder.with_name("node_b@127.0.0.1")
    |> node_builder.with_cookie("typed_example_cookie")
    |> node_builder.start()

  io.println("Node B started as: node_b@127.0.0.1")

  // Connect to Node A
  io.println("\nConnecting to Node A...")
  case cluster.connect("node_a@127.0.0.1") {
    Ok(_) -> io.println("✓ Connected to Node A")
    Error(_) -> {
      io.println("✗ Failed to connect to Node A")
      io.println("  Make sure Node A is running first!")
    }
  }

  // Create type-safe global subject with integrated codecs
  let global = global.new(codec.string_encoder(), codec.string_decoder())

  // Register globally so Node A can find us
  case registry.register_typed("node_b_receiver", global.subject(global)) {
    Ok(_) -> io.println("✓ Registered as 'node_b_receiver'")
    Error(_) -> io.println("✗ Failed to register global name")
  }

  // Join chat group
  case groups.join_typed("chat", global.subject(global)) {
    Ok(_) -> io.println("✓ Joined 'chat' group")
    Error(_) -> io.println("✗ Failed to join group")
  }

  // Spawn receiver loop in background
  process.start(fn() { receive_loop(global) }, True)

  io.println("\n=== Node B is now listening for typed messages ===")
  io.println("Messages will be printed as they arrive...")

  // Keep process alive
  process.sleep_forever()
}

fn receive_loop(global: global.GlobalSubject(String)) {
  receive_loop_internal(global, 0)
}

fn receive_loop_internal(global: global.GlobalSubject(String), count: Int) {
  // Wait for messages with 30 second timeout
  case global.receive(global, 30_000) {
    Ok(msg) -> {
      let new_count = count + 1
      let count_str = int.to_string(new_count)

      io.println("\n[Message #" <> count_str <> "] Received:")
      io.println("  " <> msg)

      // Echo back to Node A
      case
        messaging.send_global_typed(
          "node_a_receiver",
          "Echo: " <> msg,
          codec.string_encoder(),
        )
      {
        Ok(_) -> io.println("  ✓ Echoed back to Node A")
        Error(_) -> Nil
        // Node A might not have a receiver registered
      }

      receive_loop_internal(global, new_count)
    }
    Error(codec.DecodeTimeout) -> {
      io.println("Timeout waiting for message, continuing...")
      receive_loop_internal(global, count)
    }
    Error(err) -> {
      io.println("Decode error:")
      io.debug(err)
      receive_loop_internal(global, count)
    }
  }
}
