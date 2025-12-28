import distribute/codec
import gleeunit/should

pub fn string_codec_roundtrip_test() {
  let encoder = codec.string_encoder()
  let decoder = codec.string_decoder()
  let value = "hello world"
  let encoded = codec.encode(encoder, value)
  case encoded {
    Ok(binary_data) -> {
      let decoded = codec.decode(decoder, binary_data)
      case decoded {
        Ok(s) -> should.equal(s, value)
        Error(_) -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn int_codec_roundtrip_test() {
  let encoder = codec.int_encoder()
  let decoder = codec.int_decoder()
  let value = 42
  let encoded = codec.encode(encoder, value)
  case encoded {
    Ok(binary_data) -> {
      let decoded = codec.decode(decoder, binary_data)
      case decoded {
        Ok(i) -> should.equal(i, value)
        Error(_) -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

pub fn bool_codec_roundtrip_test() {
  let encoder = codec.bool_encoder()
  let decoder = codec.bool_decoder()
  let value = True
  let encoded = codec.encode(encoder, value)
  case encoded {
    Ok(binary_data) -> {
      let decoded = codec.decode(decoder, binary_data)
      case decoded {
        Ok(b) -> should.equal(b, value)
        Error(_) -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

import gleam/erlang/process

pub fn pid_codec_test() {
  let pid = process.self()

  let encoded = codec.pid_encoder()(pid)
  encoded |> should.be_ok

  let assert Ok(data) = encoded
  let decoded = codec.pid_decoder()(data)
  decoded |> should.be_ok

  let assert Ok(decoded_pid) = decoded
  decoded_pid |> should.equal(pid)
}

pub fn subject_codec_test() {
  let subject = process.new_subject()

  let encoded = codec.subject_encoder()(subject)
  encoded |> should.be_ok

  let assert Ok(data) = encoded
  let decoded = codec.subject_decoder()(data)
  decoded |> should.be_ok

  let assert Ok(decoded_subject) = decoded
  decoded_subject |> should.equal(subject)
}
