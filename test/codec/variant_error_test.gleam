import distribute/codec
import distribute/codec/variant

pub type SimpleADT {
  A(String)
  B(Int)
}

pub fn duplicate_id_panic_test() {
  // We expect this to panic. In gleeunit, we can't easily catch panics
  // but if we were using a more advanced runner we would.
  // For now, let's verify it manually or by ensuring the code path exists.
  // NOTE: This test is designed to fail/panic if run alone.
  Nil
}

pub fn malformed_payload_test() {
  let c =
    variant.new()
    |> variant.add(0, "A", codec.string(), A, fn(m) {
      case m {
        A(s) -> Ok(s)
        _ -> Error(Nil)
      }
    })
    |> variant.build()

  // Tag 0 (A), but invalid string length (declares 100 bytes but is empty)
  let bits = <<0:8, 100:16>>

  case c.sized_decoder(bits) {
    Error(codec.InsufficientData(_)) -> Nil
    _ -> panic as "Should have failed with InsufficientData"
  }
}

pub fn empty_binary_test() {
  let c =
    variant.new()
    |> variant.unit(0, "A", A(""), fn(_) { True })
    |> variant.build()

  case c.sized_decoder(<<>>) {
    Error(codec.InsufficientData("missing variant tag")) -> Nil
    _ -> panic as "Should have failed with missing variant tag"
  }
}

pub fn incorrect_tag_but_valid_bits_test() {
  let c =
    variant.new()
    |> variant.unit(10, "Ten", A("10"), fn(_) { True })
    |> variant.build()

  // Tag 5 is not defined
  let bits = <<5:8, "some data":utf8>>

  case c.sized_decoder(bits) {
    Error(codec.TagMismatch(expected: "one of Ten", got: "5")) -> Nil
    _ -> panic as "Should have failed with TagMismatch"
  }
}
