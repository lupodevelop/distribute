import distribute/cluster
import distribute/codec
import distribute/messaging
import distribute/settings
import gleam/erlang/process
import gleam/io
import gleam/string

pub fn main() -> Nil {
  // Allow atom creation for connecting to other nodes
  settings.set_allow_atom_creation(True)

  let _ = cluster.start_node("app_b@127.0.0.1", "cookie_integ")

  // Connect to node A
  case cluster.connect("app_a@127.0.0.1") {
    Ok(_) -> io.println("Node B: Connected to app_a")
    Error(_) -> io.println("Node B: Failed to connect to app_a")
  }

  // Wait for connection and registration propagation
  process.sleep(1000)

  // Send a type-safe message to globally registered 'calculator'
  case
    messaging.send_global_typed(
      "calculator",
      "Hello from node B!",
      codec.string_encoder(),
    )
  {
    Ok(_) -> io.println("Node B: Message sent successfully")
    Error(err) -> io.println("Node B: Send failed - " <> string.inspect(err))
  }

  Nil
}
