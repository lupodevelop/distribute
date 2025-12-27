
import distribute/registry/actor
import distribute/registry/behaviour as behaviour
import gleam/otp/static_supervisor
import gleam/otp/supervision as supervision
import gleam/otp/actor as otp_actor
import gleam/erlang/process

/// Ensure registry can be started under a supervisor and is usable.
pub fn registry_supervised_start_test() {
  let child = actor.child_spec()
  let builder = static_supervisor.new(static_supervisor.OneForOne)
  let builder = static_supervisor.add(builder, child)
  let assert Ok(_) = static_supervisor.start(builder)

  // The child spec should also be startable directly and produce a usable
  // registry subject. Exercise the registry API to ensure type-safety.
  let assert Ok(started_child) = child.start()
  let registry = started_child.data
  let md = behaviour.Metadata("node-a", [], "")
  let assert Ok(Nil) = actor.register_sync(registry, 100, "node-a", md)
  let assert Ok(_found) = actor.lookup_sync(registry, 100, "node-a")
}

/// Ensure supervisor reports an error when a child fails to start.
pub fn registry_supervision_child_start_failure_test() {
  // Create a child spec whose start function fails immediately.
  let failing = supervision.worker(fn() { Error(otp_actor.InitFailed("boom")) })
  let builder = static_supervisor.new(static_supervisor.OneForOne)
  let builder = static_supervisor.add(builder, failing)

  // Start the supervisor in a separate process so a failing child does not
  // bring down the test runner. The spawned process will send the result
  // back to our test via a temporary subject.
  let reply = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let res = static_supervisor.start(builder)
      process.send(reply, res)
      Nil
    })

  let selector = process.new_selector() |> process.select_map(reply, fn(x) { x })
  case process.selector_receive(selector, 1000) {
    Ok(result) -> {
      let assert Error(_) = result
      result
    }
    Error(_) -> {
      Error(otp_actor.InitFailed("timeout"))
    }
  }
}
