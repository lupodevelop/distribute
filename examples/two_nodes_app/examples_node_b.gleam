import distribute/cluster
import distribute/cluster/membership
import gleam/io
import distribute/messaging

pub fn start() -> Nil {
  let _ = cluster.start_node("app_b@127.0.0.1", "cookie_integ")
  // Start membership background service
  membership.start_service(500)

  // Connect to node A
  case cluster.connect("app_a@127.0.0.1") {
    Ok(_) -> io.println("Connected to app_a")
    Error(_) -> io.println("Failed to connect to app_a")
  }

  // Send a message to globally registered 'calculator'
  let _ = messaging.send_global("calculator", "Hello from node B!")
  io.println("Node B sent message to 'calculator'")
  Nil
}
