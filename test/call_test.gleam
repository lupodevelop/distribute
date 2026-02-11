import distribute/codec
import distribute/global
import distribute/receiver
import gleam/erlang/process
import gleam/result

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

fn counter_handler(msg: CounterMsg, state: Int) -> receiver.Next(Int) {
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

  // Call immediately — state should be 0
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
  // Nobody listening — should timeout
  let gs = global.new(counter_encoder, counter_decoder)
  let assert Error(global.CallTimeout) =
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
  let assert "Call timed out" = global.call_error_to_string(global.CallTimeout)
}
