import distribute/codec
import distribute/global
import distribute/registry
import gleam/erlang/process
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

pub fn register_and_whereis_test() {
  let name = "test_reg_" <> test_helpers.unique_id()
  let pid = process.self()
  let assert Ok(Nil) = registry.register(name, pid)
  let assert Ok(found) = registry.whereis(name)
  should.equal(found, pid)
  // Cleanup
  let _ = registry.unregister(name)
}

pub fn register_typed_test() {
  let name = "test_typed_" <> test_helpers.unique_id()
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let assert Ok(Nil) = registry.register_typed(name, global.subject(gs))
  let assert Ok(_pid) = registry.whereis(name)
  let _ = registry.unregister(name)
}

pub fn register_global_test() {
  let name = "test_global_" <> test_helpers.unique_id()
  let tn =
    registry.typed_name(name, codec.string_encoder(), codec.string_decoder())
  // Subject tag must equal the registry name. see register_global doc.
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.string_encoder(),
      codec.string_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)
  let assert Ok(_pid) = registry.whereis(name)
  let _ = registry.unregister(name)
}

pub fn whereis_not_found_test() {
  let result =
    registry.whereis("nonexistent_name_xyz_" <> test_helpers.unique_id())
  should.be_error(result)
}

pub fn lookup_test() {
  let name = "test_lk_" <> test_helpers.unique_id()
  let tn = registry.typed_name(name, codec.int_encoder(), codec.int_decoder())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)

  let assert Ok(found) = registry.lookup(tn)
  let assert Ok(Nil) = global.send(found, 42)

  let _ = registry.unregister(name)
}

pub fn is_registered_test() {
  let name = "test_isreg_" <> test_helpers.unique_id()
  should.be_false(registry.is_registered(name))
  let assert Ok(Nil) = registry.register(name, process.self())
  should.be_true(registry.is_registered(name))
  let _ = registry.unregister(name)
}

pub fn register_already_exists_test() {
  let name = "test_dup_" <> test_helpers.unique_id()
  let assert Ok(Nil) = registry.register(name, process.self())
  let result = registry.register(name, process.self())
  should.be_error(result)
  let _ = registry.unregister(name)
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
  let name = "test_unreg_" <> test_helpers.unique_id()
  let assert Ok(Nil) = registry.register(name, process.self())
  let _ = registry.unregister(name)
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

pub fn lookup_with_timeout_found_immediately_test() {
  let name = "lkto_imm_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)
  let result = registry.lookup_with_timeout(tn, 200, 20)
  should.be_ok(result)
  let _ = registry.unregister(registry.typed_name_to_string(tn))
}

pub fn lookup_with_timeout_not_found_test() {
  let tn = registry.named("lkto_nf_" <> test_helpers.unique_id(), codec.int())
  // Nothing registered. should time out
  let result = registry.lookup_with_timeout(tn, 60, 20)
  should.be_error(result)
}

pub fn lookup_with_timeout_delayed_registration_test() {
  let name = "lkto_delayed_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let reply = process.new_subject()

  // Spawn a process that registers after 80ms
  let _ =
    process.spawn_unlinked(fn() {
      process.sleep(80)
      let _ = registry.register_global(tn, gs)
      Nil
    })

  // Poll for up to 500ms. should find it after the delay
  let _ =
    process.spawn_unlinked(fn() {
      let result = registry.lookup_with_timeout(tn, 500, 20)
      process.send(reply, result)
    })

  let assert Ok(result) = process.receive(reply, 700)
  should.be_ok(result)
  let _ = registry.unregister(registry.typed_name_to_string(tn))
}

pub fn lookup_async_found_test() {
  let name = "lkasync_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)

  let reply = process.new_subject()
  registry.lookup_async(tn, reply, 200, 20)

  let assert Ok(result) = process.receive(reply, 400)
  should.be_ok(result)
  let _ = registry.unregister(registry.typed_name_to_string(tn))
}

pub fn lookup_async_not_found_test() {
  let tn =
    registry.named("lkasync_nf_" <> test_helpers.unique_id(), codec.int())
  let reply = process.new_subject()
  registry.lookup_async(tn, reply, 50, 10)
  let assert Ok(result) = process.receive(reply, 300)
  should.be_error(result)
}

// ---------------------------------------------------------------------------
// Parameter validation. fail-fast with typed errors instead of crashing
// the FFI or hot-spinning the CPU.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Local-ownership ACL: `unregister` is a no-op for names whose owner PID
// runs on a different node, and for names not in :global at all.
// Prevents the "registry wipe" attack where unvalidated input could
// silently tear down arbitrary cluster routing. The check is stateless
// (`node(Pid) =:= node()`) so it has no leak surface and cannot be
// bypassed by stale local state.
// ---------------------------------------------------------------------------

pub fn unregister_owned_name_succeeds_test() {
  let name = "owned_acl_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)
  let assert True = registry.is_registered(name)

  let assert Ok(Nil) = registry.unregister(name)
  let assert False = registry.is_registered(name)
}

pub fn unregister_unknown_name_returns_not_found_test() {
  // Name not in :global at all. typed `NotFound` instead of silent
  // no-op so observability paths surface the cause.
  let name = "unknown_acl_" <> test_helpers.unique_id()
  let assert False = registry.is_registered(name)
  let assert Error(registry.NotFound) = registry.unregister(name)
  let assert False = registry.is_registered(name)
}

pub fn unregister_idempotent_under_repeated_calls_test() {
  // After the first successful unregister, the entry is gone. Every
  // subsequent call returns `Error(NotFound)` cleanly. the function
  // is idempotent in effect (registry stays empty) and total in shape
  // (always returns a typed Result, never crashes).
  let name = "idem_acl_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)

  let assert Ok(Nil) = registry.unregister(name)
  let assert Error(registry.NotFound) = registry.unregister(name)
  let assert Error(registry.NotFound) = registry.unregister(name)
  let assert Error(registry.NotFound) = registry.unregister(name)
  let assert False = registry.is_registered(name)
}

// ---------------------------------------------------------------------------
// register_global enforces subject tag == registry name. A mismatched
// tag causes silent mailbox accumulation on lookup; we surface it
// up-front instead.
// ---------------------------------------------------------------------------

pub fn register_global_rejects_random_tag_test() {
  let name = "tag_mismatch_random_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  // global.new uses a fresh process subject. random Ref as tag,
  // not equal to the name.
  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let assert Error(registry.InvalidArgument(_)) =
    registry.register_global(tn, gs)
}

pub fn register_global_rejects_nil_tag_test() {
  let name = "tag_mismatch_nil_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  // from_pid uses dynamic.nil() as tag. also not equal to the name.
  let gs =
    global.from_pid(process.self(), codec.int_encoder(), codec.int_decoder())
  let assert Error(registry.InvalidArgument(_)) =
    registry.register_global(tn, gs)
}

pub fn register_global_accepts_matching_name_tag_test() {
  let name = "tag_match_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.int())
  // unsafe_from_name builds a Subject whose tag equals the supplied name.
  // This is the contract.
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.int_encoder(),
      codec.int_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)
  let _ = registry.unregister(name)
}

pub fn register_typed_rejects_random_tag_test() {
  let name = "typed_tag_mismatch_random_" <> test_helpers.unique_id()
  let subj: process.Subject(BitArray) = process.new_subject()
  let assert Error(registry.InvalidArgument(_)) =
    registry.register_typed(name, subj)
}

pub fn register_typed_rejects_nil_tag_test() {
  let name = "typed_tag_mismatch_nil_" <> test_helpers.unique_id()
  let gs =
    global.from_pid(process.self(), codec.int_encoder(), codec.int_decoder())
  let assert Error(registry.InvalidArgument(_)) =
    registry.register_typed(name, global.subject(gs))
}

pub fn lookup_with_timeout_zero_timeout_rejected_test() {
  let tn =
    registry.named("lkto_zerot_" <> test_helpers.unique_id(), codec.int())
  let result = registry.lookup_with_timeout(tn, 0, 10)
  let assert Error(registry.LookupInvalidTimeout(0)) = result
}

pub fn lookup_with_timeout_negative_timeout_rejected_test() {
  let tn = registry.named("lkto_negt_" <> test_helpers.unique_id(), codec.int())
  let result = registry.lookup_with_timeout(tn, -50, 10)
  let assert Error(registry.LookupInvalidTimeout(-50)) = result
}

pub fn lookup_with_timeout_zero_poll_rejected_test() {
  let tn =
    registry.named("lkto_zerop_" <> test_helpers.unique_id(), codec.int())
  let result = registry.lookup_with_timeout(tn, 100, 0)
  let assert Error(registry.LookupInvalidPollInterval(0)) = result
}

pub fn lookup_with_timeout_negative_poll_rejected_test() {
  let tn = registry.named("lkto_negp_" <> test_helpers.unique_id(), codec.int())
  let result = registry.lookup_with_timeout(tn, 100, -1)
  let assert Error(registry.LookupInvalidPollInterval(-1)) = result
}

pub fn lookup_async_invalid_timeout_returns_error_test() {
  let tn =
    registry.named("lkasync_inv_" <> test_helpers.unique_id(), codec.int())
  let reply = process.new_subject()
  registry.lookup_async(tn, reply, 0, 10)
  let assert Ok(result) = process.receive(reply, 200)
  let assert Error(registry.LookupInvalidTimeout(0)) = result
}

pub fn named_register_lookup_test() {
  let name = "test_named_" <> test_helpers.unique_id()
  let tn = registry.named(name, codec.string())
  let gs =
    global.unsafe_from_name(
      name,
      process.self(),
      codec.string_encoder(),
      codec.string_decoder(),
    )
  let assert Ok(Nil) = registry.register_global(tn, gs)
  let assert Ok(_found) = registry.lookup(tn)
  let _ = registry.unregister(name)
}
