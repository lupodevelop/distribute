import distribute/codec
import distribute/config
import distribute/global
import gleam/erlang/process
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn global_subject_send_receive_test() {
  // Use from_subject with a unique subject for test isolation
  let subj = process.new_subject()
  let gs =
    global.from_subject(subj, codec.string_encoder(), codec.string_decoder())
  let assert Ok(Nil) = global.send(gs, "hello global")
  let assert Ok(msg) = global.receive(gs, 1000)
  should.equal(msg, "hello global")
}

pub fn global_subject_from_subject_test() {
  // Create a GlobalSubject from an existing Subject(BitArray)
  let subj = process.new_subject()
  let gs = global.from_subject(subj, codec.int_encoder(), codec.int_decoder())
  let assert Ok(Nil) = global.send(gs, 42)
  let assert Ok(val) = global.receive(gs, 1000)
  should.equal(val, 42)
}

pub fn global_subject_owner_test() {
  let gs = global.new(codec.string_encoder(), codec.string_decoder())
  let assert Ok(pid) = global.owner(gs)
  should.equal(pid, process.self())
}

pub fn global_subject_timeout_test() {
  // Use from_subject with a unique subject for test isolation
  let subj = process.new_subject()
  let gs =
    global.from_subject(subj, codec.string_encoder(), codec.string_decoder())
  // No message sent. should timeout
  let result = global.receive(gs, 10)
  should.be_error(result)
}

pub fn global_subject_encoder_decoder_access_test() {
  let enc = codec.int_encoder()
  let dec = codec.int_decoder()
  let gs = global.new(enc, dec)
  // Verify encoder/decoder are accessible
  let assert Ok(bin) = codec.encode(global.encoder(gs), 99)
  let assert Ok(val) = codec.decode(global.decoder(gs), bin)
  should.equal(val, 99)
}

pub fn global_new_unique_tags_test() {
  // Two global.new() calls should produce isolated subjects
  let gs1 = global.new(codec.int_encoder(), codec.int_decoder())
  let gs2 = global.new(codec.int_encoder(), codec.int_decoder())

  let assert Ok(Nil) = global.send(gs1, 111)
  let assert Ok(Nil) = global.send(gs2, 222)

  // Each receives only its own message
  let assert Ok(v1) = global.receive(gs1, 1000)
  let assert Ok(v2) = global.receive(gs2, 1000)
  should.equal(v1, 111)
  should.equal(v2, 222)
}

// ---------------------------------------------------------------------------
// SendError. payload too large
// ---------------------------------------------------------------------------

pub fn send_payload_too_large_test() {
  // Set a tiny limit, send something bigger
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_payload_size_bytes: 4,
      ),
    )
  let subj = process.new_subject()
  let gs =
    global.from_subject(subj, codec.string_encoder(), codec.string_decoder())
  // "hello" encodes to 4-byte length prefix + 5 bytes = 9 bytes > 4
  let result = global.send(gs, "hello")
  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  result |> should.be_error
}

pub fn send_encode_failed_wraps_error_test() {
  // An encoder that always fails
  let always_fail = fn(_: Int) { Error(codec.EncodeFailed("boom")) }
  let subj = process.new_subject()
  let gs = global.from_subject(subj, always_fail, codec.int_decoder())
  global.send(gs, 1) |> should.be_error
}

pub fn send_ok_within_limit_test() {
  let subj = process.new_subject()
  let gs = global.from_subject(subj, codec.int_encoder(), codec.int_decoder())
  global.send(gs, 42) |> should.be_ok
}

// ---------------------------------------------------------------------------
// receive. payload too large
// ---------------------------------------------------------------------------

pub fn receive_payload_too_large_test() {
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_payload_size_bytes: 2,
      ),
    )
  let subj: process.Subject(BitArray) = process.new_subject()
  let gs = global.from_subject(subj, codec.int_encoder(), codec.int_decoder())
  // int encodes to 9 bytes (1 tag + 8 body). well above limit 2
  process.send(subj, <<0, 0, 0, 0, 0, 0, 0, 0, 42>>)
  let result = global.receive(gs, 500)
  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  result |> should.be_error
}

// ---------------------------------------------------------------------------
// call. TargetDown when actor is dead
// ---------------------------------------------------------------------------

pub fn call_target_down_test() {
  // Spawn a process that exits immediately, then call on its dead PID.
  // erlang:monitor on a dead PID delivers an immediate DOWN. no timeout wait.
  let dead_pid = process.spawn_unlinked(fn() { Nil })
  process.sleep(20)
  let gs = global.from_pid(dead_pid, codec.int_encoder(), codec.int_decoder())
  global.call(gs, fn(_) { 0 }, codec.int_decoder(), 500)
  |> should.equal(Error(global.TargetDown))
}

// ---------------------------------------------------------------------------
// call. PayloadTooLarge on oversized request
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// drain_reply. must be exhaustive, not single-shot. A misbehaving target
// that replies twice after a timeout would otherwise leave orphan messages
// in the caller's mailbox.
// ---------------------------------------------------------------------------

pub fn drain_reply_clears_multiple_late_messages_test() {
  let subj = process.new_subject()
  // Simulate a target that replied many times after the deadline.
  process.send(subj, <<1>>)
  process.send(subj, <<2>>)
  process.send(subj, <<3>>)
  process.send(subj, <<4>>)
  process.send(subj, <<5>>)

  // Drain.
  global.drain_reply(subj)

  // Mailbox must be empty for this Subject. a follow-up receive must
  // time out, not pick up an orphan.
  let result = process.receive(subj, 50)
  should.be_error(result)
}

pub fn drain_reply_on_empty_subject_is_a_noop_test() {
  let subj = process.new_subject()
  global.drain_reply(subj)
  // Sending after drain must still work. drain didn't break the subject.
  process.send(subj, <<42>>)
  let assert Ok(<<42>>) = process.receive(subj, 50)
}

pub fn drain_reply_is_bounded_under_continuous_flood_test() {
  // A live flood of late replies must not stall the caller forever.
  // If `drain_reply` were unbounded, this test would time out waiting
  // for the drainer process to finish.
  let subj = process.new_subject()
  let done = process.new_subject()

  let flooder = process.spawn_unlinked(fn() { flood_subject_forever(subj) })

  let drainer =
    process.spawn_unlinked(fn() {
      global.drain_reply(subj)
      process.send(done, Nil)
    })

  let completed = process.receive(done, 1000)

  process.kill(flooder)
  case completed {
    Ok(Nil) -> should.be_true(True)
    Error(Nil) -> {
      process.kill(drainer)
      should.be_true(False)
    }
  }
}

// ---------------------------------------------------------------------------
// receive_default. uses configured timeout
// ---------------------------------------------------------------------------

pub fn receive_default_works_test() {
  let gs = global.new(codec.string_encoder(), codec.string_decoder())
  let assert Ok(Nil) = global.send(gs, "default-receive")
  let assert Ok(msg) = global.receive_default(gs)
  msg |> should.equal("default-receive")
}

pub fn receive_default_uses_config_timeout_test() {
  // Configure a tiny timeout, then receive on an empty subject. should
  // return DecodeTimeout, NOT block for the original 5s default.
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(..config.default(), default_call_timeout_ms: 10),
    )
  let gs = global.new(codec.int_encoder(), codec.int_decoder())
  let result = global.receive_default(gs)
  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  let assert Error(codec.DecodeTimeout) = result
}

// ---------------------------------------------------------------------------
// reply. payload size enforcement (mirror of send)
// ---------------------------------------------------------------------------

pub fn reply_payload_too_large_test() {
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_payload_size_bytes: 4,
      ),
    )
  let reply_subj = process.new_subject()
  // "much-too-long" string > 4 byte limit
  let result = global.reply(reply_subj, "much-too-long", codec.string_encoder())

  config.reset()
  let assert Ok(Nil) = config.configure(config.default())

  result |> should.be_error
}

pub fn reply_encode_failure_test() {
  let always_fail = fn(_: Int) { Error(codec.EncodeFailed("forced")) }
  let reply_subj = process.new_subject()
  global.reply(reply_subj, 42, always_fail) |> should.be_error
}

pub fn reply_within_limit_succeeds_test() {
  let reply_subj = process.new_subject()
  let assert Ok(Nil) = global.reply(reply_subj, 1, codec.int_encoder())
  // Caller can decode it back.
  let assert Ok(bits) = process.receive(reply_subj, 100)
  let assert Ok(1) = codec.int_decoder()(bits)
}

// ---------------------------------------------------------------------------
// from_pid. Nil tag is shared, so two GlobalSubjects on the same PID
// interleave. Verifies the documented warning at the API level.
// ---------------------------------------------------------------------------

pub fn from_pid_nil_tag_is_shared_test() {
  let here = process.self()
  let a = global.from_pid(here, codec.int_encoder(), codec.int_decoder())
  let b = global.from_pid(here, codec.int_encoder(), codec.int_decoder())

  // Two subjects, same PID, same Nil tag → same wire-level mailbox slot.
  let assert Ok(pid_a) = global.owner(a)
  let assert Ok(pid_b) = global.owner(b)
  should.equal(pid_a, pid_b)
}

pub fn call_payload_too_large_on_request_test() {
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(
        ..config.default(),
        default_call_timeout_ms: 5000,
        max_payload_size_bytes: 2,
      ),
    )
  let subj = process.new_subject()
  let gs =
    global.from_subject(subj, codec.string_encoder(), codec.string_decoder())
  let result =
    global.call(
      gs,
      fn(_) { "this is way too long" },
      codec.string_decoder(),
      500,
    )
  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  result |> should.be_error
}

fn flood_subject_forever(subj: process.Subject(BitArray)) -> Nil {
  process.send(subj, <<>>)
  flood_subject_forever(subj)
}
