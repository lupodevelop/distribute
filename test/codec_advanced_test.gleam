/// Tests for advanced codec helpers (custom type builders)
import distribute/codec
import distribute/codec/builder
import gleeunit/should

// Example custom type for testing
pub type Person {
  Person(name: String, age: Int)
}

pub fn custom2_codec_test() {
  let #(encoder, decoder) =
    builder.custom2(
      codec.string_encoder(),
      codec.int_encoder(),
      codec.string_decoder(),
      codec.int_decoder(),
      Person,
      fn(p) { #(p.name, p.age) },
    )

  let person = Person("Alice", 30)
  let encoded = codec.encode(encoder, person)
  encoded |> should.be_ok

  let assert Ok(data) = encoded
  let decoded = codec.decode(decoder, data)
  decoded |> should.be_ok
  let assert Ok(decoded_person) = decoded

  decoded_person.name |> should.equal("Alice")
  decoded_person.age |> should.equal(30)
}

pub type Color {
  Red
  Green
  Blue
}

pub fn enum_codec_test() {
  let to_int = fn(color: Color) {
    case color {
      Red -> 0
      Green -> 1
      Blue -> 2
    }
  }

  let from_int = fn(n: Int) {
    case n {
      0 -> Ok(Red)
      1 -> Ok(Green)
      2 -> Ok(Blue)
      _ -> Error(Nil)
    }
  }

  let #(encoder, decoder) = builder.enum_codec(to_int, from_int)

  // Test Red
  let encoded = codec.encode(encoder, Red)
  encoded |> should.be_ok
  let assert Ok(data) = encoded
  let decoded = codec.decode(decoder, data)
  decoded |> should.equal(Ok(Red))

  // Test Green
  let encoded = codec.encode(encoder, Green)
  encoded |> should.be_ok
  let assert Ok(data) = encoded
  let decoded = codec.decode(decoder, data)
  decoded |> should.equal(Ok(Green))

  // Test Blue
  let encoded = codec.encode(encoder, Blue)
  encoded |> should.be_ok
  let assert Ok(data) = encoded
  let decoded = codec.decode(decoder, data)
  decoded |> should.equal(Ok(Blue))
}
