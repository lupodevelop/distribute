import distribute/cluster
import distribute/cluster/membership
import distribute/codec
import distribute/global
import distribute/registry
import gleam/io

pub fn start() -> Nil {
  // Start this node in distributed mode
  let _ = cluster.start_node("app_a@127.0.0.1", "cookie_integ")

  // Start the membership background service
  membership.start_service(500)

  // Create a type-safe global subject
  let global = global.new(codec.string_encoder(), codec.string_decoder())

  // Register it globally as 'calculator'
  case registry.register_typed("calculator", global.subject(global)) {
    Ok(_) -> io.println("Node A: 'calculator' registered")
    Error(_) -> io.println("Node A: Registration failed")
  }

  // Log current membership and leader
  let alive = membership.alive()
  io.println("Alive nodes: " <> string.inspect(alive))
  let leader = membership.current_leader()
  case leader {
    Ok(l) -> io.println("Current leader: " <> l)
    Error(Nil) -> io.println("No leader elected")
  }

  Nil
}
