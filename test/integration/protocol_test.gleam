import distribute
import distribute/codec
import distribute/codec/variant
import distribute/receiver
import distribute/registry

pub type Protocol {
  Ping(Int)
  Pong(Int)
  Shutdown
}

fn protocol_codec() {
  variant.new()
  |> variant.add(0, "Ping", codec.int(), Ping, fn(m) {
    case m {
      Ping(i) -> Ok(i)
      _ -> Error(Nil)
    }
  })
  |> variant.add(1, "Pong", codec.int(), Pong, fn(m) {
    case m {
      Pong(i) -> Ok(i)
      _ -> Error(Nil)
    }
  })
  |> variant.unit(2, "Shutdown", Shutdown, fn(m) { m == Shutdown })
  |> variant.build()
}

pub fn integration_variant_actor_test() {
  let tn = registry.named("integ_test", protocol_codec())

  // Start actor that pongs back
  let assert Ok(gs) =
    distribute.start_actor(tn, Nil, fn(msg, state) {
      case msg {
        Ping(_i) -> {
          // In a real scenario we'd use global.reply if we had a reply_to
          // but for this simple test let's just observe state change if we could.
          receiver.Continue(state)
        }
        _ -> receiver.Continue(state)
      }
    })

  // Verify we can send all variants
  let assert Ok(Nil) = distribute.send(gs, Ping(123))
  let assert Ok(Nil) = distribute.send(gs, Pong(456))
  let assert Ok(Nil) = distribute.send(gs, Shutdown)

  // Cleanup name
  let _ = distribute.unregister("integ_test")
}
