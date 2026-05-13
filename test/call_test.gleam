import distribute/codec
import distribute/config
import distribute/global
import distribute/receiver
import gleam/erlang/process
import gleam/result
import gleeunit/should

// ---------------------------------------------------------------------------
// Test message type: a counter with increment + get
// ---------------------------------------------------------------------------

type CounterMsg {
  Increment(Int)
  GetCount(process.Subject(BitArray))
}

fn counter_encoder(msg: CounterMsg) -> Result(BitArray, codec.EncodeError) {
  case msg {
    Increment(n) -> {
      use body <- result.try(codec.int_encoder()(n))
      Ok(<<0, body:bits>>)
    }
    GetCount(reply) -> {
      use body <- result.try(codec.subject_encoder()(reply))
      Ok(<<1, body:bits>>)
    }
  }
}

fn counter_decoder(data: BitArray) -> Result(CounterMsg, codec.DecodeError) {
  case data {
    <<0, rest:bytes>> -> {
      use n <- result.try(codec.int_decoder()(rest))
      Ok(Increment(n))
    }
    <<1, rest:bytes>> -> {
      use reply <- result.try(codec.subject_decoder()(rest))
      Ok(GetCount(reply))
    }
    _ -> Error(codec.InvalidBinary("unknown counter msg tag"))
  }
}

fn counter_handler(msg: CounterMsg, state: Int) -> receiver.HandlerStep(Int) {
  case msg {
    Increment(n) -> receiver.Continue(state + n)
    GetCount(reply) -> {
      let _ = global.reply(reply, state, codec.int_encoder())
      receiver.Continue(state)
    }
  }
}

// ---------------------------------------------------------------------------
// Subject codec tests
// ---------------------------------------------------------------------------

pub fn subject_codec_roundtrip_test() {
  let subject = process.new_subject()

  // Encode
  let assert Ok(encoded) = codec.subject_encoder()(subject)
  // Decode
  let assert Ok(decoded) = codec.subject_decoder()(encoded)

  // Send through decoded subject, receive on original
  process.send(decoded, <<1, 2, 3>>)
  let assert Ok(<<1, 2, 3>>) = process.receive(subject, 100)
}

pub fn subject_sized_decoder_test() {
  let subject = process.new_subject()
  let assert Ok(encoded) = codec.subject_encoder()(subject)

  // Append trailing bytes
  let data = <<encoded:bits, 42, 99>>

  // Sized decoder returns subject + remaining bytes
  let assert Ok(#(_decoded, remaining)) = codec.subject_sized_decoder()(data)
  let assert <<42, 99>> = remaining
}

pub fn subject_bundled_codec_test() {
  let c = codec.subject()
  let subject = process.new_subject()

  let assert Ok(encoded) = c.encoder(subject)
  let assert Ok(decoded) = c.decoder(encoded)

  process.send(decoded, <<7, 8, 9>>)
  let assert Ok(<<7, 8, 9>>) = process.receive(subject, 100)
}

// ---------------------------------------------------------------------------
// Call pattern tests
// ---------------------------------------------------------------------------

pub fn call_basic_test() {
  let assert Ok(raw_subject) =
    receiver.start_receiver(0, counter_decoder, counter_handler)
  let gs = global.from_subject(raw_subject, counter_encoder, counter_decoder)

  // Increment
  let assert Ok(Nil) = global.send(gs, Increment(5))
  let assert Ok(Nil) = global.send(gs, Increment(3))
  process.sleep(20)

  // Call to get count
  let assert Ok(8) = global.call(gs, GetCount, codec.int_decoder(), 1000)
}

pub fn call_on_fresh_state_test() {
  let assert Ok(raw_subject) =
    receiver.start_receiver(0, counter_decoder, counter_handler)
  let gs = global.from_subject(raw_subject, counter_encoder, counter_decoder)

  // Call immediately. state should be 0
  let assert Ok(0) = global.call(gs, GetCount, codec.int_decoder(), 1000)
}

pub fn call_multiple_test() {
  let assert Ok(raw_subject) =
    receiver.start_receiver(0, counter_decoder, counter_handler)
  let gs = global.from_subject(raw_subject, counter_encoder, counter_decoder)

  let assert Ok(0) = global.call(gs, GetCount, codec.int_decoder(), 1000)

  let assert Ok(Nil) = global.send(gs, Increment(10))
  process.sleep(10)
  let assert Ok(10) = global.call(gs, GetCount, codec.int_decoder(), 1000)

  let assert Ok(Nil) = global.send(gs, Increment(7))
  process.sleep(10)
  let assert Ok(17) = global.call(gs, GetCount, codec.int_decoder(), 1000)
}

pub fn call_timeout_test() {
  // Nobody listening. should timeout
  let gs = global.new(counter_encoder, counter_decoder)
  let assert Error(global.Timeout) =
    global.call(gs, GetCount, codec.int_decoder(), 50)
}

pub fn reply_test() {
  // Test global.reply directly
  let reply_subject = process.new_subject()
  let assert Ok(Nil) = global.reply(reply_subject, 42, codec.int_encoder())

  let assert Ok(bits) = process.receive(reply_subject, 100)
  let assert Ok(42) = codec.int_decoder()(bits)
}

pub fn call_error_to_string_test() {
  let assert "Call timed out" = global.call_error_to_string(global.Timeout)
}

// ---------------------------------------------------------------------------
// call_isolated. proxy-per-call to keep the caller's mailbox bounded
// under sustained timeouts.
// ---------------------------------------------------------------------------

pub fn call_isolated_works_test() {
  let assert Ok(raw_subject) =
    receiver.start_receiver(0, counter_decoder, counter_handler)
  let gs = global.from_subject(raw_subject, counter_encoder, counter_decoder)

  let assert Ok(Nil) = global.send(gs, Increment(11))
  process.sleep(20)
  let assert Ok(11) =
    global.call_isolated(gs, GetCount, codec.int_decoder(), 1000)
}

pub fn call_isolated_propagates_timeout_test() {
  // Nobody listening. inner call times out, the outer proxy buffer
  // wins so we still get a typed `Timeout` (never a panic).
  let gs = global.new(counter_encoder, counter_decoder)
  let assert Error(global.Timeout) =
    global.call_isolated(gs, GetCount, codec.int_decoder(), 50)
}

pub fn call_isolated_late_reply_does_not_pollute_caller_test() {
  // Target replies AFTER our timeout. With plain `call`, the late
  // reply would orphan in the caller's mailbox. With `call_isolated`,
  // it lands in the proxy's mailbox which dies with the proxy. Run the
  // probe inside a dedicated child process so the mailbox we inspect is
  // exactly the synthetic caller's mailbox, not the test runner's.
  let assert Ok(raw_subject) =
    receiver.start_receiver(0, counter_decoder, counter_handler)
  let gs = global.from_subject(raw_subject, counter_encoder, counter_decoder)

  let probe_result = process.new_subject()
  let _ =
    process.spawn_unlinked(fn() {
      // Force the actor to be slow: send Increment first, sleep, then
      // call_isolated with a very short timeout so the reply arrives late.
      let assert Ok(Nil) = global.send(gs, Increment(7))
      process.sleep(10)
      let _ = global.call_isolated(gs, GetCount, codec.int_decoder(), 1)
      // Wait long enough for any late reply to arrive.
      process.sleep(50)

      // Probe the synthetic caller mailbox directly: any orphan raw term
      // left behind by the proxy would be matched by `select_other`
      // immediately.
      let caller_mailbox =
        process.new_selector()
        |> process.select_other(fn(_dyn) { Nil })
      process.send(probe_result, process.selector_receive(caller_mailbox, 0))
    })

  let assert Ok(result) = process.receive(probe_result, 500)
  should.be_error(result)

  let assert Ok(owner) = process.subject_owner(raw_subject)
  process.send_exit(owner)
}

pub fn call_isolated_slow_decoder_timeout_does_not_pollute_caller_test() {
  // The proxy can also outlive the outer timeout if work happens *after*
  // the inner reply arrives but before the proxy forwards the final result.
  // A deliberately slow response decoder gives us a deterministic race:
  // the outer call times out first, then the proxy finishes later.
  let assert Ok(raw_subject) =
    receiver.start_receiver(0, counter_decoder, counter_handler)
  let gs = global.from_subject(raw_subject, counter_encoder, counter_decoder)

  let result_probe = process.new_subject()
  let mailbox_probe = process.new_subject()
  let slow_decoder = fn(bits: BitArray) {
    process.sleep(250)
    codec.int_decoder()(bits)
  }

  let _ =
    process.spawn_unlinked(fn() {
      let result = global.call_isolated(gs, GetCount, slow_decoder, 10)
      process.send(result_probe, result)

      // Wait for the proxy to finish its delayed decode and attempt the
      // late send. If `call_isolated` timed out without killing/waiting
      // for the proxy, the orphan result is now sitting in this caller's
      // mailbox and `select_other` will catch it.
      process.sleep(300)

      let caller_mailbox =
        process.new_selector()
        |> process.select_other(fn(_dyn) { Nil })
      process.send(mailbox_probe, process.selector_receive(caller_mailbox, 0))
    })

  let assert Ok(result) = process.receive(result_probe, 500)
  should.equal(result, Error(global.Timeout))

  let assert Ok(mailbox_state) = process.receive(mailbox_probe, 1000)
  should.be_error(mailbox_state)

  let assert Ok(owner) = process.subject_owner(raw_subject)
  process.send_exit(owner)
}

pub fn call_isolated_caller_death_does_not_signal_observer_test() {
  // Spawn a child that issues a call_isolated, then kill the child.
  // The proxy is now `spawn_unlinked` so a kill on the caller does NOT
  // crash-propagate to the proxy; instead the proxy's `process.send`
  // to the (now dead) caller's result_subject becomes a silent BEAM
  // no-op and the proxy exits naturally. What this test validates is
  // the caller-side guarantee: a caller killed mid-`call_isolated`
  // never reaches the line after the call, so any code observable
  // through the caller (here, the `observer` send) must NOT fire.
  let observer = process.new_subject()
  let child =
    process.spawn(fn() {
      let gs = global.new(counter_encoder, counter_decoder)
      // Long timeout so the call is still in flight when we kill.
      let _ = global.call_isolated(gs, GetCount, codec.int_decoder(), 5000)
      process.send(observer, Nil)
    })

  // Give the child time to spawn its proxy.
  process.sleep(20)
  process.kill(child)

  // The child died before `call_isolated` returned, so the post-call
  // `send(observer, Nil)` never runs. A ~200 ms wait that times out
  // is enough proof the kill landed before the observer signal.
  let result = process.receive(observer, 200)
  should.be_error(result)
}

// ---------------------------------------------------------------------------
// call_default. uses config.get().default_call_timeout_ms
// ---------------------------------------------------------------------------

pub fn call_default_works_test() {
  let assert Ok(raw_subject) =
    receiver.start_receiver(0, counter_decoder, counter_handler)
  let gs = global.from_subject(raw_subject, counter_encoder, counter_decoder)

  let assert Ok(Nil) = global.send(gs, Increment(7))
  process.sleep(20)
  let assert Ok(7) = global.call_default(gs, GetCount, codec.int_decoder())
}

pub fn call_default_timeout_test() {
  // Configure a tiny timeout, verify call_default uses it (times out with nobody replying)
  config.reset()
  let assert Ok(Nil) =
    config.configure(
      config.Config(..config.default(), default_call_timeout_ms: 10),
    )
  let gs = global.new(counter_encoder, counter_decoder)
  let result = global.call_default(gs, GetCount, codec.int_decoder())
  config.reset()
  let assert Ok(Nil) = config.configure(config.default())
  let assert Error(global.Timeout) = result
}
