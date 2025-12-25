import cluster
import gleam/io
import gleam/string
import monitor

pub fn start() -> Nil {
  // Start this node in distributed mode (short name)
  let _ = cluster.start_node("app_a@127.0.0.1", "cookie_integ")

  // Spawn a process that receives messages and prints them
  // Use current process as a simple handler for example purposes
  let pid = monitor.self()

  // (Example) We could register the pid globally, but registry.Pid and
  // monitor.Pid are distinct opaque types in this example. For a real
  // integration test, use the library API directly in a compiled example
  // project. Here we just print the pid for demonstration.
  io.println("Node A started (pid: " <> string.inspect(pid) <> ")")
  // Keep the function alive; actual node will be kept alive by erl -noshell loop
  Nil
}
