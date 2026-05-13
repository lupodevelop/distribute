/// Interactive dev script for manual multi-node testing.
///
/// Starts a local distributed node, registers a typed actor, then spawns
/// a peer node (OTP 25+) that sends messages from the other side.
/// Run with:
///
///   gleam dev
///
/// Requirements:
///   - epmd must be running (`epmd -daemon`)
///   - OTP 25+ for the `peer` module
///
/// This script exercises the full distribute stack end-to-end:
///   peer node  →  raw binary send  →  main node actor  →  decode  →  handler
import distribute/actor
import distribute/cluster
import distribute/codec
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/string
import peer_support

// ---------------------------------------------------------------------------
// Dev scenario
// ---------------------------------------------------------------------------

pub fn main() {
  io.println("=== distribute dev: multi-node smoke test ===")
  io.println("")

  // 1. Start this node as distributed
  io.println("[1] Starting distributed node…")
  let node_name = "distribute_dev@127.0.0.1"
  let cookie = "devcookie"
  case cluster.start_node(node_name, cookie) {
    Ok(Nil) -> io.println("    Node: " <> cluster.self_node())
    Error(cluster.AlreadyStarted) ->
      io.println("    Already distributed: " <> cluster.self_node())
    Error(e) -> {
      let reason = case e {
        cluster.InvalidNodeName(r) -> "invalid node name: " <> r
        cluster.InvalidCookieFormat(r) -> "invalid cookie format: " <> r
        cluster.AlreadyStarted -> "already started"
        cluster.NetworkError(r) -> "network error: " <> r
        cluster.StartFailed(r) -> "start failed: " <> r
        cluster.StartAtomBudgetExceeded ->
          "atom budget exhausted (raise config.max_distribution_atoms)"
      }
      io.println("    Failed to start node: " <> reason)
      io.println("    Is epmd running? Try: epmd -daemon")
      Nil
    }
  }

  case cluster.is_distributed() {
    False -> {
      io.println("")
      io.println("Cannot continue without a distributed node. Exiting.")
    }
    True -> run_scenario()
  }
}

fn run_scenario() {
  // 2. Register a stateful actor
  io.println("")
  io.println("[2] Registering actor 'counter' (type: Int)…")
  let tn = registry.named("counter", codec.int())
  let result_subj = process.new_subject()

  let assert Ok(_) =
    actor.start_registered(
      tn,
      0,
      fn(msg, state) {
        let new_state = state + msg
        io.println(
          "    handler: received "
          <> int.to_string(msg)
          <> ", state → "
          <> int.to_string(new_state),
        )
        process.send(result_subj, new_state)
        receiver.Continue(new_state)
      },
      5000,
    )

  io.println("    Registered: " <> registry.typed_name_to_string(tn))

  // 3. Start a peer node
  io.println("")
  io.println("[3] Starting peer node…")
  case peer_support.start("dev_peer") {
    Error(reason) -> {
      io.println("    peer:start failed: " <> reason)
      io.println("    Falling back to local send simulation.")
      local_simulation(tn, result_subj)
    }
    Ok(peer) -> {
      let peer_node = peer_support.node(peer)
      io.println("    Peer node: " <> peer_node)

      // 4. peer_ffi:start_peer called net_kernel:connect_node(Origin) from the
      // peer side (via the TCP control channel) to establish real Erlang
      // distribution. Only after that can erlang:send(MainPid, Msg) work.
      io.println("")
      io.println("[4] peer connected to main node via Erlang distribution")
      process.sleep(200)

      let peer_nodes = peer_support.connected_nodes(peer)
      io.println(
        "    peer sees nodes: "
        <> case peer_nodes {
          [] -> "(none (distribution not established))"
          ns -> string.join(ns, ", ")
        },
      )

      // Lookup the actor PID on the main node
      let assert Ok(actor_pid) = registry.whereis("counter")

      // 5. Send from peer. Direct via PID (bypasses :global)
      io.println("")
      io.println("[5] Sending messages from peer node (via direct PID)…")
      send_from_peer(peer, actor_pid, "counter", [3, 4, 5])

      // 6. Verify on this node
      io.println("")
      io.println("[6] Verifying actor received messages…")
      verify_results(result_subj, [3, 7, 12])

      // 7. Cleanup
      io.println("")
      io.println("[7] Stopping peer…")
      peer_support.stop(peer)
      let _ = registry.unregister("counter")
      io.println("    Done.")
    }
  }

  io.println("")
  io.println("=== test complete ===")
}

fn send_from_peer(
  peer: peer_support.Peer,
  pid: process.Pid,
  tag: String,
  messages: List(Int),
) -> Nil {
  case messages {
    [] -> Nil
    [msg, ..rest] -> {
      let assert Ok(encoded) = codec.encode(codec.int_encoder(), msg)
      case peer_support.send_pid(peer, pid, tag, encoded) {
        Ok(Nil) ->
          io.println("    peer → sent " <> int.to_string(msg) <> " to " <> tag)
        Error(reason) ->
          io.println(
            "    peer → send failed for "
            <> int.to_string(msg)
            <> ": "
            <> reason,
          )
      }
      process.sleep(50)
      send_from_peer(peer, pid, tag, rest)
    }
  }
}

fn verify_results(
  result_subj: process.Subject(Int),
  expected: List(Int),
) -> Nil {
  case expected {
    [] -> Nil
    [exp, ..rest] -> {
      case process.receive(result_subj, 2000) {
        Ok(got) if got == exp ->
          io.println("    ✓ state = " <> int.to_string(got))
        Ok(got) ->
          io.println(
            "    ✗ expected "
            <> int.to_string(exp)
            <> " got "
            <> int.to_string(got),
          )
        Error(Nil) ->
          io.println("    ✗ timeout waiting for " <> int.to_string(exp))
      }
      verify_results(result_subj, rest)
    }
  }
}

fn local_simulation(
  tn: registry.TypedName(Int),
  result_subj: process.Subject(Int),
) -> Nil {
  io.println("")
  io.println("[local sim] Sending raw binaries directly (no peer)…")
  case registry.lookup(tn) {
    Error(Nil) -> io.println("    Cannot find actor.")
    Ok(gs) -> {
      let msgs = [3, 4, 5]
      send_local(gs, msgs)
      verify_results(result_subj, [3, 7, 12])
      let _ = registry.unregister(registry.typed_name_to_string(tn))
      Nil
    }
  }
}

fn send_local(gs: global.GlobalSubject(Int), messages: List(Int)) -> Nil {
  case messages {
    [] -> Nil
    [msg, ..rest] -> {
      let assert Ok(encoded) = codec.encode(codec.int_encoder(), msg)
      process.send(global.subject(gs), encoded)
      io.println("    local → sent " <> int.to_string(msg))
      process.sleep(50)
      send_local(gs, rest)
    }
  }
}
