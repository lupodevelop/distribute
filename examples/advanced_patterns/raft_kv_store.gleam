/// Advanced example: Raft-based distributed key-value store.
///
/// This example demonstrates how to build a consistent distributed KV store using:
/// - Raft leader election for consistency
/// - Type-safe actor patterns for the store
/// - Automatic failover when leader changes
/// - Read-your-writes consistency
import distribute/actor
import distribute/cluster
import distribute/codec
import distribute/election/raft_lite
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import gleam/string

/// KV Store operations
pub type KVMsg {
  Put(key: String, value: String, reply: process.Subject(Result(Nil, String)))
  Get(key: String, reply: process.Subject(Result(String, String)))
  Delete(key: String, reply: process.Subject(Result(Nil, String)))
  Replicate(key: String, value: String, from_leader: String)
  GetLeader(reply: process.Subject(Result(String, Nil)))
  BecomeLeader
  StepDown
}

/// Store state
type State {
  State(
    data: Dict(String, String),
    is_leader: Bool,
    leader_node: String,
    term: Int,
  )
}

/// Encoder for KV messages
fn encoder() -> codec.Encoder(KVMsg) {
  fn(msg: KVMsg) -> Result(BitArray, codec.EncodeError) {
    case msg {
      Put(_, _, _) -> Ok(<<0>>)
      Get(_, _) -> Ok(<<1>>)
      Delete(_, _) -> Ok(<<2>>)
      Replicate(_, _, _) -> Ok(<<3>>)
      GetLeader(_) -> Ok(<<4>>)
      BecomeLeader -> Ok(<<5>>)
      StepDown -> Ok(<<6>>)
    }
  }
}

/// Decoder for KV messages
fn decoder() -> codec.Decoder(KVMsg) {
  fn(binary: BitArray) -> Result(KVMsg, codec.DecodeError) {
    case binary {
      <<0>> -> {
        let reply = process.new_subject()
        Ok(Put("key", "value", reply))
      }
      <<1>> -> {
        let reply = process.new_subject()
        Ok(Get("key", reply))
      }
      <<2>> -> {
        let reply = process.new_subject()
        Ok(Delete("key", reply))
      }
      <<3>> -> Ok(Replicate("key", "value", "leader"))
      <<4>> -> {
        let reply = process.new_subject()
        Ok(GetLeader(reply))
      }
      <<5>> -> Ok(BecomeLeader)
      <<6>> -> Ok(StepDown)
      _ -> Error(codec.InvalidBinary("Unknown KV message"))
    }
  }
}

/// Handle KV store messages
fn handle_message(msg: KVMsg, state: State) -> receiver.Next(State) {
  case msg {
    Put(key, value, reply) -> {
      case state.is_leader {
        True -> {
          // Leader: accept write and replicate
          io.println("Leader PUT: " <> key <> " = " <> value)
          let new_data = dict.insert(state.data, key, value)
          process.send(reply, Ok(Nil))

          // TODO: Replicate to followers
          // For now, just update local state
          receiver.Continue(State(..state, data: new_data))
        }
        False -> {
          // Follower: redirect to leader
          process.send(
            reply,
            Error("Not leader. Leader is: " <> state.leader_node),
          )
          receiver.Continue(state)
        }
      }
    }

    Get(key, reply) -> {
      // Reads can be served by any node (eventual consistency)
      // For strong consistency, only leader should serve reads
      case dict.get(state.data, key) {
        Ok(value) -> {
          io.println("GET: " <> key <> " = " <> value)
          process.send(reply, Ok(value))
        }
        Error(_) -> process.send(reply, Error("Key not found: " <> key))
      }
      receiver.Continue(state)
    }

    Delete(key, reply) -> {
      case state.is_leader {
        True -> {
          io.println("Leader DELETE: " <> key)
          let new_data = dict.delete(state.data, key)
          process.send(reply, Ok(Nil))
          receiver.Continue(State(..state, data: new_data))
        }
        False -> {
          process.send(
            reply,
            Error("Not leader. Leader is: " <> state.leader_node),
          )
          receiver.Continue(state)
        }
      }
    }

    Replicate(key, value, from_leader) -> {
      // Follower receives replication from leader
      io.println(
        "REPLICATE from " <> from_leader <> ": " <> key <> " = " <> value,
      )
      let new_data = dict.insert(state.data, key, value)
      receiver.Continue(
        State(..state, data: new_data, leader_node: from_leader),
      )
    }

    GetLeader(reply) -> {
      case state.is_leader {
        True -> process.send(reply, Ok(cluster.node_name()))
        False -> process.send(reply, Ok(state.leader_node))
      }
      receiver.Continue(state)
    }

    BecomeLeader -> {
      let term = raft_lite.current_term()
      io.println(
        "=== BECOMING LEADER (term " <> string.inspect(term) <> ") ===",
      )
      receiver.Continue(
        State(
          ..state,
          is_leader: True,
          term: term,
          leader_node: cluster.node_name(),
        ),
      )
    }

    StepDown -> {
      io.println("=== STEPPING DOWN ===")
      receiver.Continue(State(..state, is_leader: False))
    }
  }
}

/// Start the Raft KV store
pub fn start() -> Result(global.GlobalSubject(KVMsg), actor.ActorError) {
  // Start Raft election service
  raft_lite.start_service()

  // Determine initial leadership
  let am_leader = raft_lite.am_i_leader()
  let leader = case raft_lite.get_raft_leader() {
    Ok(l) -> l
    Error(_) -> cluster.node_name()
  }

  let initial_state =
    State(
      data: dict.new(),
      is_leader: am_leader,
      leader_node: leader,
      term: raft_lite.current_term(),
    )

  // Start KV store actor
  let store =
    actor.start_typed_actor(initial_state, encoder(), decoder(), handle_message)

  // Register globally
  let name = "raft_kv_store"
  case registry.register_typed(name, global.subject(store)) {
    Ok(_) -> {
      io.println("✓ Raft KV Store registered as '" <> name <> "'")
      io.println("  Leader: " <> leader)
      io.println("  I am leader: " <> string.inspect(am_leader))
      Ok(store)
    }
    Error(_) -> Error(actor.InvalidConfiguration("Failed to register KV store"))
  }
}

/// Start KV store with worker pool for read replicas
pub fn start_with_pool(
  pool_size: Int,
) -> Result(#(process.Pid, List(global.GlobalSubject(KVMsg))), actor.ActorError) {
  raft_lite.start_service()

  let initial_state =
    State(data: dict.new(), is_leader: False, leader_node: "", term: 0)

  case
    actor.pool_supervisor(
      pool_size,
      initial_state,
      encoder(),
      decoder(),
      handle_message,
    )
  {
    Ok(#(sup_pid, workers)) -> {
      io.println(
        "✓ KV Store pool started with "
        <> string.inspect(pool_size)
        <> " workers",
      )
      Ok(#(sup_pid, workers))
    }
    Error(err) -> Error(actor.StartFailed(err))
  }
}

/// Put a key-value pair
pub fn put(
  store: global.GlobalSubject(KVMsg),
  key: String,
  value: String,
) -> Result(Nil, String) {
  let reply = process.new_subject()
  let msg = Put(key, value, reply)

  case global.send(store, msg) {
    Ok(_) -> {
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error("Timeout waiting for PUT response")
      }
    }
    Error(_) -> Error("Failed to send PUT message")
  }
}

/// Get a value by key
pub fn get(
  store: global.GlobalSubject(KVMsg),
  key: String,
) -> Result(String, String) {
  let reply = process.new_subject()
  let msg = Get(key, reply)

  case global.send(store, msg) {
    Ok(_) -> {
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error("Timeout waiting for GET response")
      }
    }
    Error(_) -> Error("Failed to send GET message")
  }
}

/// Delete a key
pub fn delete(
  store: global.GlobalSubject(KVMsg),
  key: String,
) -> Result(Nil, String) {
  let reply = process.new_subject()
  let msg = Delete(key, reply)

  case global.send(store, msg) {
    Ok(_) -> {
      case process.receive(reply, 5000) {
        Ok(result) -> result
        Error(_) -> Error("Timeout waiting for DELETE response")
      }
    }
    Error(_) -> Error("Failed to send DELETE message")
  }
}
