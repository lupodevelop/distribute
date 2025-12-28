import distribute/codec.{type Decoder}
import distribute/receiver.{type Next}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor

/// Start an actor that receives and decodes typed messages.
///
/// This is a wrapper around `receiver.start_typed_receiver`.
/// The returned Subject accepts `BitArray` messages.
pub fn start(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> Result(Subject(BitArray), actor.StartError) {
  receiver.start_typed_receiver(initial_state, decoder, handler)
}

/// Start a global actor that can be registered with `registry.register_typed`.
///
/// This is a wrapper around `receiver.start_global_receiver`.
/// The returned Subject has a `Nil` tag and accepts `BitArray` messages.
pub fn start_global(
  initial_state: state,
  decoder: Decoder(msg),
  handler: fn(msg, state) -> Next(state),
) -> Subject(BitArray) {
  receiver.start_global_receiver(initial_state, decoder, handler)
}
