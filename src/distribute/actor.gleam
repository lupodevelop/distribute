/// Named actor lifecycle: start, register, supervise, pool.
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{worker}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Start a named actor.
pub fn start(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(global.GlobalSubject(msg), actor.StartError) {
  case
    receiver.start_distributed_worker(
      registry.typed_name_to_string(typed_name),
      initial_state,
      registry.typed_name_encoder(typed_name),
      registry.typed_name_decoder(typed_name),
      handler,
    )
  {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// OTP child spec for a named actor.
pub fn child_spec(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.Next(state),
) -> supervision.ChildSpecification(global.GlobalSubject(msg)) {
  worker(fn() {
    receiver.start_distributed_worker(
      registry.typed_name_to_string(typed_name),
      initial_state,
      registry.typed_name_encoder(typed_name),
      registry.typed_name_decoder(typed_name),
      handler,
    )
  })
}

/// Start an actor and register it globally. Kills the actor if
/// registration fails.
pub fn start_registered(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(global.GlobalSubject(msg), registry.RegisterError) {
  let name = registry.typed_name_to_string(typed_name)
  case
    receiver.start_distributed_worker(
      name,
      initial_state,
      registry.typed_name_encoder(typed_name),
      registry.typed_name_decoder(typed_name),
      handler,
    )
  {
    Ok(started) -> {
      case registry.register_global(typed_name, started.data) {
        Ok(_) -> Ok(started.data)
        Error(err) -> {
          // Kill orphaned actor to prevent leaks
          process.kill(started.pid)
          Error(err)
        }
      }
    }
    Error(_) -> Error(registry.RegistrationFailed("actor failed to start"))
  }
}

/// Start a supervised actor that auto-registers on (re)start.
pub fn start_supervised(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(process.Pid, actor.StartError) {
  let name = registry.typed_name_to_string(typed_name)
  let spec =
    worker(fn() {
      case
        receiver.start_distributed_worker(
          name,
          initial_state,
          registry.typed_name_encoder(typed_name),
          registry.typed_name_decoder(typed_name),
          handler,
        )
      {
        Ok(started) -> {
          // Auto-register (re-register on supervisor restart)
          let _ = registry.unregister(name)
          let _ = registry.register_global(typed_name, started.data)
          Ok(started)
        }
        Error(err) -> Error(err)
      }
    })

  let builder =
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.add(spec)

  case static_supervisor.start(builder) {
    Ok(sup) -> Ok(sup.pid)
    Error(err) -> Error(err)
  }
}

/// Start N supervised actors, registered as `name_1` .. `name_N`.
pub fn pool(
  typed_name: registry.TypedName(msg),
  size: Int,
  initial_state: state,
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(process.Pid, actor.StartError) {
  let name_prefix = registry.typed_name_to_string(typed_name)
  let specs =
    list.range(1, size)
    |> list.map(fn(i) {
      let worker_name = name_prefix <> "_" <> int.to_string(i)
      let worker_tn = registry.pool_member(typed_name, i)
      worker(fn() {
        case
          receiver.start_distributed_worker(
            worker_name,
            initial_state,
            registry.typed_name_encoder(typed_name),
            registry.typed_name_decoder(typed_name),
            handler,
          )
        {
          Ok(started) -> {
            let _ = registry.unregister(worker_name)
            let _ = registry.register_global(worker_tn, started.data)
            Ok(started)
          }
          Error(err) -> Error(err)
        }
      })
    })

  let builder =
    list.fold(
      specs,
      static_supervisor.new(static_supervisor.OneForOne),
      fn(acc, spec) { static_supervisor.add(acc, spec) },
    )

  case static_supervisor.start(builder) {
    Ok(sup) -> Ok(sup.pid)
    Error(err) -> Error(err)
  }
}
