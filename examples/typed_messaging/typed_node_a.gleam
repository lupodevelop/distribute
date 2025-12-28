/// Node A: Type-safe messaging sender example
///
/// This node demonstrates:
/// - Using GlobalSubject with integrated codecs
/// - Type-safe process groups
/// - Type-safe messaging
import distribute/codec
import distribute/global
import distribute/groups
import distribute/messaging
import distribute/node_builder
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list

pub fn main() {
  // Start distributed node
  io.println("=== Starting Node A (Typed Messaging Example) ===")

  let assert Ok(_) =
    node_builder.new()
    |> node_builder.with_name("node_a@127.0.0.1")
    |> node_builder.with_cookie("typed_example_cookie")
    |> node_builder.start()

  io.println("Node A started as: node_a@127.0.0.1")

  // Create a type-safe global subject with integrated codecs
  let global = global.new(codec.string_encoder(), codec.string_decoder())

  // Join a process group for pub/sub
  case groups.join_typed("chat", global.subject(global)) {
    Ok(_) -> io.println("Joined 'chat' group")
    Error(err) -> {
      io.println("Failed to join group")
      io.debug(err)
    }
  }

  // Wait for Node B to connect
  io.println("\nWaiting 2 seconds for Node B to connect...")
  process.sleep(2000)

  // Example 1: Type-safe global send
  io.println("\n=== Example 1: Type-safe global send ===")
  send_typed_message_example()

  // Example 2: Type-safe group broadcast
  io.println("\n=== Example 2: Type-safe group broadcast ===")
  broadcast_example()

  // Example 3: Receive typed messages
  io.println("\n=== Example 3: Receiving typed messages ===")
  receive_example(global)

  io.println("\n=== Node A completed all examples ===")

  // Keep process alive
  process.sleep_forever()
}

fn send_typed_message_example() {
  // Create encoder for our message
  let encoder = codec.string_encoder()

  // Send typed message to globally registered name
  let message = "Hello from Node A (type-safe!)"
  io.println("Sending: " <> message)

  case messaging.send_global_typed("node_b_receiver", message, encoder) {
    Ok(_) -> io.println("✓ Message sent successfully")
    Error(err) -> {
      io.println("✗ Failed to send: ")
      io.debug(err)
    }
  }
}

fn broadcast_example() {
  let encoder = codec.string_encoder()

  // Broadcast to all members of 'chat' group
  let messages = [
    "Message 1: Type-safe broadcast",
    "Message 2: Using codecs",
    "Message 3: No unsafe terms!",
  ]

  list.each(messages, fn(msg) {
    io.println("Broadcasting: " <> msg)
    case groups.broadcast_typed("chat", msg, encoder) {
      Ok(_) -> io.println("  ✓ Broadcast successful")
      Error(err) -> {
        io.println("  ✗ Broadcast failed")
        io.debug(err)
      }
    }
    process.sleep(100)
  })
}

fn receive_example(global: global.GlobalSubject(String)) {
  io.println("Waiting for typed messages (5 second timeout)...")

  // Try to receive 3 messages using GlobalSubject's built-in receive
  list.range(1, 3)
  |> list.each(fn(n) {
    let msg_num = int.to_string(n)
    io.println("\nWaiting for message " <> msg_num <> "...")

    case global.receive(global, 5000) {
      Ok(msg) -> {
        io.println("✓ Received: " <> msg)
      }
      Error(codec.DecodeTimeout) -> {
        io.println("✗ Timeout - no message received")
      }
      Error(err) -> {
        io.println("✗ Decode error:")
        io.debug(err)
      }
    }
  })
}
