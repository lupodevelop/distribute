import distribute/cluster
import distribute/cluster/membership
import gleam/io
import gleam/option
import gleam/string
import distribute/monitor

pub fn start() -> Nil {
  // Start this node in distributed mode (short name)
  let _ = cluster.start_node("app_a@127.0.0.1", "cookie_integ")

  // Start the membership background service
  membership.start_service(500)

  // Spawn a process that receives messages and prints them
  let pid = monitor.self()

  io.println("Node A started (pid: " <> string.inspect(pid) <> ")")

  // Log current membership and leader (if any)
  let alive = membership.alive()
  io.println("Alive nodes: " <> string.inspect(alive))
  let leader = membership.current_leader()
  case leader {
    option.Some(l) -> io.println("Current leader: " <> l)
    option.None -> io.println("No leader elected")
  }

  // Keep the function alive; the integration script will keep the BEAM VM running
  Nil
}
