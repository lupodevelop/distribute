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
