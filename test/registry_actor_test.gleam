/// Tests for the registry actor public API wrappers.

import distribute/registry/actor
import distribute/registry/behaviour as behaviour
import gleam/list
import gleeunit/should

pub fn register_and_lookup_test() {
  let assert Ok(registry) = actor.start()
  // Prepare metadata
  let md = behaviour.Metadata("node@host", [], "")

  // Register
  let assert Ok(Nil) = actor.register_sync(registry, 100, "node@host", md)

  // Lookup
  let assert Ok(_found) = actor.lookup_sync(registry, 100, "node@host")
}

pub fn unregister_test() {
  let assert Ok(registry) = actor.start()
  let md = behaviour.Metadata("node2@host", [], "")
  let _ = actor.register_sync(registry, 100, "node2@host", md)

  let assert Ok(Nil) = actor.unregister_sync(registry, 100, "node2@host")
  let assert Error(_) = actor.lookup_sync(registry, 100, "node2@host")
}

pub fn list_nodes_test() {
  let assert Ok(registry) = actor.start()
  let md1 = behaviour.Metadata("a@h", [], "")
  let md2 = behaviour.Metadata("b@h", [], "")
  let _ = actor.register_sync(registry, 100, "a@h", md1)
  let _ = actor.register_sync(registry, 100, "b@h", md2)

  let ids = actor.list_nodes_sync(registry, 100)
  // Order not guaranteed; check membership
  let a = list.contains(ids, "a@h")
  let b = list.contains(ids, "b@h")
  should.be_true(a && b)
}
