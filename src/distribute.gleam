//// Facade for common distributed operations: start a node, register
//// actors, send messages, look things up.

import distribute/actor as dist_actor
import distribute/cluster
import distribute/codec
import distribute/global
import distribute/receiver
import distribute/registry
import gleam/otp/actor

pub fn version() -> String {
  "3.0.0"
}

// -- Cluster -----------------------------------------------------------------

pub fn start(name: String, cookie: String) -> Result(Nil, cluster.StartError) {
  cluster.start_node(name, cookie)
}

pub fn connect(node: String) -> Result(Nil, cluster.ConnectError) {
  cluster.connect(node)
}

pub fn nodes() -> List(String) {
  cluster.nodes()
}

pub fn self_node() -> String {
  cluster.self_node()
}

pub fn ping(node: String) -> Bool {
  cluster.ping(node)
}

// -- Global Subject ----------------------------------------------------------

pub fn new_subject(
  encoder: codec.Encoder(msg),
  decoder: codec.Decoder(msg),
) -> global.GlobalSubject(msg) {
  global.new(encoder, decoder)
}

pub fn start_actor(
  typed_name: registry.TypedName(msg),
  initial_state: state,
  handler: fn(msg, state) -> receiver.Next(state),
) -> Result(global.GlobalSubject(msg), actor.StartError) {
  dist_actor.start(typed_name, initial_state, handler)
}

pub fn send(
  subject: global.GlobalSubject(msg),
  message: msg,
) -> Result(Nil, codec.EncodeError) {
  global.send(subject, message)
}

pub fn receive(
  subject: global.GlobalSubject(msg),
  timeout_ms: Int,
) -> Result(msg, codec.DecodeError) {
  global.receive(subject, timeout_ms)
}

// -- Registry ----------------------------------------------------------------

pub fn register(
  typed_name: registry.TypedName(msg),
  subject: global.GlobalSubject(msg),
) -> Result(Nil, registry.RegisterError) {
  registry.register_global(typed_name, subject)
}

pub fn lookup(
  typed_name: registry.TypedName(msg),
) -> Result(global.GlobalSubject(msg), Nil) {
  registry.lookup(typed_name)
}

pub fn unregister(name: String) -> Result(Nil, registry.RegisterError) {
  registry.unregister(name)
}
