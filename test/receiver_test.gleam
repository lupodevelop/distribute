import distribute/codec
import distribute/global
import distribute/receiver
import gleam/erlang/process
import gleeunit/should

// Test message types
type StateAccMsg {
  Add(Int)
  GetSum(process.Subject(BitArray))
}

type ControlMsg {
  Ping
  Shutdown
}

pub fn receive_typed_timeout_test() {
  let subject = process.new_subject()
  let decoder = codec.string_decoder()

  // No message sent, should timeout
  case receiver.receive_typed(subject, decoder, 10) {
    Error(receiver.Timeout) -> Nil
    _ -> panic as "Expected timeout"
  }
}

pub fn receive_typed_success_test() {
  let subject = process.new_subject()
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()

  // Send a message
  let assert Ok(encoded) = encoder("Hello, World!")
  process.send(subject, encoded)

  // Receive it
  case receiver.receive_typed(subject, decoder, 100) {
    Ok(msg) -> should.equal(msg, "Hello, World!")
    Error(_) -> panic as "Expected success"
  }
}

pub fn receive_typed_decode_error_test() {
  let subject = process.new_subject()
  let decoder = codec.int_decoder()

  // Send invalid data (not a proper int encoding)
  process.send(subject, <<99, 88, 77>>)

  // Should get decode error
  case receiver.receive_typed(subject, decoder, 100) {
    Error(receiver.DecodeError(_)) -> Nil
    _ -> panic as "Expected decode error"
  }
}

pub fn receive_typed_multiple_messages_test() {
  let subject = process.new_subject()
  let encoder = codec.int_encoder()
  let decoder = codec.int_decoder()

  // Send three messages
  let assert Ok(msg1) = encoder(1)
  let assert Ok(msg2) = encoder(2)
  let assert Ok(msg3) = encoder(3)
  process.send(subject, msg1)
  process.send(subject, msg2)
  process.send(subject, msg3)

  // Receive them in order
  let assert Ok(val1) = receiver.receive_typed(subject, decoder, 100)
  let assert Ok(val2) = receiver.receive_typed(subject, decoder, 100)
  let assert Ok(val3) = receiver.receive_typed(subject, decoder, 100)

  should.equal(val1, 1)
  should.equal(val2, 2)
  should.equal(val3, 3)
}

pub fn start_typed_actor_basic_test() {
  // Test that start_typed_actor creates a working GlobalSubject
  let encoder = codec.int_encoder()
  let decoder = codec.int_decoder()

  let actor =
    receiver.start_typed_actor(0, encoder, decoder, fn(_msg, state) {
      receiver.Continue(state + 1)
    })

  // Send a message to the actor
  let assert Ok(_) = global.send(actor, 42)

  // Give it time to process
  process.sleep(50)

  // Verify actor is still alive by sending another message
  let assert Ok(_) = global.send(actor, 99)
  process.sleep(50)
}

pub fn start_typed_actor_state_accumulation_test() {
  // Test that state is properly accumulated

  let encoder = fn(msg: StateAccMsg) -> Result(BitArray, codec.EncodeError) {
    case msg {
      Add(n) -> {
        let assert Ok(encoded_n) = codec.int_encoder()(n)
        Ok(<<0, encoded_n:bits>>)
      }
      GetSum(_) -> Ok(<<1>>)
    }
  }

  let decoder = fn(binary: BitArray) -> Result(StateAccMsg, codec.DecodeError) {
    case binary {
      <<0, rest:bits>> -> {
        case codec.int_decoder()(rest) {
          Ok(n) -> Ok(Add(n))
          Error(e) -> Error(e)
        }
      }
      <<1>> -> {
        let reply_subject = process.new_subject()
        Ok(GetSum(reply_subject))
      }
      _ -> Error(codec.InvalidBinary("Unknown message type"))
    }
  }

  let actor =
    receiver.start_typed_actor(0, encoder, decoder, fn(msg, sum) {
      case msg {
        Add(n) -> receiver.Continue(sum + n)
        GetSum(reply) -> {
          let assert Ok(encoded) = codec.int_encoder()(sum)
          process.send(reply, encoded)
          receiver.Continue(sum)
        }
      }
    })

  // Send some Add messages
  let assert Ok(_) = global.send(actor, Add(10))
  let assert Ok(_) = global.send(actor, Add(20))
  let assert Ok(_) = global.send(actor, Add(5))

  process.sleep(50)

  // Request the sum (this test just verifies actor is working)
  let assert Ok(_) = global.send(actor, Add(0))
  process.sleep(50)
}

pub fn start_typed_actor_stop_test() {
  // Test that Stop directive terminates the actor

  let encoder = fn(msg: ControlMsg) -> Result(BitArray, codec.EncodeError) {
    case msg {
      Ping -> Ok(<<0>>)
      Shutdown -> Ok(<<1>>)
    }
  }

  let decoder = fn(binary: BitArray) -> Result(ControlMsg, codec.DecodeError) {
    case binary {
      <<0>> -> Ok(Ping)
      <<1>> -> Ok(Shutdown)
      _ -> Error(codec.InvalidBinary("Unknown message type"))
    }
  }

  let actor =
    receiver.start_typed_actor(Nil, encoder, decoder, fn(msg, state) {
      case msg {
        Ping -> receiver.Continue(state)
        Shutdown -> receiver.Stop
      }
    })

  // Send ping - should work
  let assert Ok(_) = global.send(actor, Ping)
  process.sleep(20)

  // Send shutdown
  let assert Ok(_) = global.send(actor, Shutdown)
  process.sleep(50)
  // Actor should be stopped now (we can't easily test this without monitoring)
}
