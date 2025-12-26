import distribute/cluster
import distribute/codec
import distribute/global
import distribute/registry
import distribute/settings
import gleam/io

pub fn main() -> Nil {
  // Allow atom creation for node name
  settings.set_allow_atom_creation(True)

  // Start this node in distributed mode
  let _ = cluster.start_node("app_a@127.0.0.1", "cookie_integ")

  // Create a type-safe global subject for receiving messages
  let global = global.new(codec.string_encoder(), codec.string_decoder())

  // Register it globally as 'calculator'
  case registry.register_typed("calculator", global.subject(global)) {
    Ok(_) -> io.println("Node A: 'calculator' registered successfully")
    Error(_) -> io.println("Node A: Failed to register 'calculator'")
  }

  io.println("Node A started and ready to receive messages")

  // Wait for incoming message
  case global.receive(global, 10_000) {
    Ok(msg) -> io.println("calc got: " <> msg)
    Error(_) -> io.println("No message received")
  }

  Nil
}
