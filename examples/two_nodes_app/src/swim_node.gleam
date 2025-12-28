import distribute/cluster
import distribute/cluster/membership
import distribute/settings
import env
import gleam/erlang/process
import gleam/io
import gleam/string

pub fn main() {
  settings.set_allow_atom_creation(True)

  let assert Ok(node_name) = env.get_env("NODE_NAME")
  let assert Ok(cookie) = env.get_env("NODE_COOKIE")

  case cluster.start_node(node_name, cookie) {
    Ok(_) -> Nil
    Error(e) -> {
      io.println("Failed to start node: " <> string.inspect(e))
      process.sleep(1000)
      panic as "Node start failed"
    }
  }

  // Start membership with 500ms interval
  membership.start_service(500)

  // Connect to other nodes if specified
  case env.get_env("JOIN_NODE") {
    Ok(join_node) -> {
      connect_with_retry(join_node, 5)
    }
    Error(_) -> Nil
  }

  io.println(node_name <> " ready")
  process.sleep_forever()
}

fn connect_with_retry(node: String, attempts: Int) -> Nil {
  case cluster.connect(node) {
    Ok(_) -> {
      io.println("Connected to " <> node)
      Nil
    }
    Error(_) -> {
      case attempts {
        0 -> {
          io.println("Failed to connect to " <> node <> " after retries")
          Nil
        }
        n -> {
          io.println("Failed to connect to " <> node <> ", retrying...")
          process.sleep(1000)
          connect_with_retry(node, n - 1)
        }
      }
    }
  }
}
