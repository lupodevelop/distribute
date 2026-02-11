import distribute/codec
import distribute/global
import distribute/registry
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn register_and_whereis_test() {
  let name = "test_reg_" <> unique_id()
  let pid = process.self()
  let assert Ok(Nil) = registry.register(name, pid)
  let assert Ok(found) = registry.whereis(name)
  should.equal(found, pid)
  // Cleanup
  let assert Ok(Nil) = registry.unregister(name)
}

pub fn register_typed_test() {
  let name = "test_typed_" <> unique_id()
  let subj = process.new_subject()
  let assert Ok(Nil) = registry.register_typed(name, subj)
  let assert Ok(_pid) = registry.whereis(name)
  let assert Ok(Nil) = registry.unregister(name)
}

pub fn register_global_test() {
  let name = "test_global_" <> unique_id()
  let tn =
    registry.typed_name(name, codec.string_encoder(), codec.string_decoder())
  let gs = global.new(codec.string_encoder(), codec.string_decoder())
  let assert Ok(Nil) = registry.register_global(tn, gs)
  let assert Ok(_pid) = registry.whereis(name)
  let assert Ok(Nil) = registry.unregister(name)
}

pub fn whereis_not_found_test() {
  let result = registry.whereis("nonexistent_name_xyz_" <> unique_id())
  should.be_error(result)
}

pub fn lookup_test() {
  let name = "test_lk_" <> unique_id()
  let tn = registry.typed_name(name, codec.int_encoder(), codec.int_decoder())
  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let assert Ok(Nil) = registry.register_global(tn, gs)

  let assert Ok(found) = registry.lookup(tn)
  // Send via found subject
  let assert Ok(Nil) = global.send(found, 42)

  let assert Ok(Nil) = registry.unregister(name)
}

pub fn is_registered_test() {
  let name = "test_isreg_" <> unique_id()
  should.be_false(registry.is_registered(name))
  let assert Ok(Nil) = registry.register(name, process.self())
  should.be_true(registry.is_registered(name))
  let assert Ok(Nil) = registry.unregister(name)
}

pub fn register_already_exists_test() {
  let name = "test_dup_" <> unique_id()
  let assert Ok(Nil) = registry.register(name, process.self())
  let result = registry.register(name, process.self())
  should.be_error(result)
  let assert Ok(Nil) = registry.unregister(name)
}

pub fn register_empty_name_test() {
  let result = registry.register("", process.self())
  should.be_error(result)
}

pub fn register_name_with_spaces_test() {
  let result = registry.register("has space", process.self())
  should.be_error(result)
}

pub fn unregister_test() {
  let name = "test_unreg_" <> unique_id()
  let assert Ok(Nil) = registry.register(name, process.self())
  let assert Ok(Nil) = registry.unregister(name)
  should.be_false(registry.is_registered(name))
}

pub fn typed_name_accessors_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  let tn = registry.typed_name("my_service", enc, dec)

  should.equal(registry.typed_name_to_string(tn), "my_service")
  // Encoder and decoder should be retrievable (just check they exist)
  let _enc = registry.typed_name_encoder(tn)
  let _dec = registry.typed_name_decoder(tn)
  should.be_true(True)
}

pub fn pool_member_test() {
  let base =
    registry.typed_name(
      "worker",
      codec.string_encoder(),
      codec.string_decoder(),
    )
  let member = registry.pool_member(base, 3)
  should.equal(registry.typed_name_to_string(member), "worker_3")
}

pub fn named_creates_typed_name_test() {
  let tn = registry.named("svc", codec.int())
  should.equal(registry.typed_name_to_string(tn), "svc")
  // Verify the codec works through the TypedName
  let assert Ok(bin) = codec.encode(registry.typed_name_encoder(tn), 42)
  codec.decode(registry.typed_name_decoder(tn), bin)
  |> should.equal(Ok(42))
}

pub fn named_register_lookup_test() {
  let name = "test_named_" <> unique_id()
  let tn = registry.named(name, codec.string())
  let gs = global.new(codec.string_encoder(), codec.string_decoder())
  let assert Ok(Nil) = registry.register_global(tn, gs)
  let assert Ok(_found) = registry.lookup(tn)
  let assert Ok(Nil) = registry.unregister(name)
}

// Simple unique ID generator for test isolation
@external(erlang, "erlang", "unique_integer")
fn erlang_unique_int() -> Int

fn unique_id() -> String {
  let n = erlang_unique_int()
  int_to_string(case n < 0 {
    True -> 0 - n
    False -> n
  })
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String
