import distribute/codec
import distribute/codec/composite
import distribute/codec/variant
import gleeunit/should

pub type TestADT {
  Message(String)
  Quit
  Status(Int, Bool)
}

fn test_adt_codec() {
  variant.new()
  |> variant.add(0, "Message", codec.string(), Message, fn(m) {
    case m {
      Message(s) -> Ok(s)
      _ -> Error(Nil)
    }
  })
  |> variant.unit(1, "Quit", Quit, fn(m) {
    case m {
      Quit -> True
      _ -> False
    }
  })
  |> variant.add(
    2,
    "Status",
    composite.tuple2(codec.int(), codec.bool()),
    fn(t: #(Int, Bool)) { Status(t.0, t.1) },
    fn(m) {
      case m {
        Status(i, b) -> Ok(#(i, b))
        _ -> Error(Nil)
      }
    },
  )
  |> variant.build()
}

pub fn variant_encode_decode_test() {
  let c = test_adt_codec()

  // Test Message
  let msg = Message("hello")
  let assert Ok(bits1) = c.encoder(msg)
  let assert Ok(decoded1) = c.decoder(bits1)
  should.equal(decoded1, msg)

  // Test Quit
  let quit = Quit
  let assert Ok(bits2) = c.encoder(quit)
  let assert Ok(decoded2) = c.decoder(bits2)
  should.equal(decoded2, quit)

  // Test Status
  let status = Status(42, True)
  let assert Ok(bits3) = c.encoder(status)
  let assert Ok(decoded3) = c.decoder(bits3)
  should.equal(decoded3, status)
}

pub fn unknown_tag_test() {
  let c = test_adt_codec()
  let bits = <<99:8, "garbage":utf8>>

  case c.decoder(bits) {
    Error(codec.TagMismatch(expected: "one of Message, Quit, Status", got: "99")) ->
      Nil
    _ -> panic as "expected TagMismatch error"
  }
}
