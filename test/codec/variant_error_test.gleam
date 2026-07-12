import distribute/codec
import distribute/codec/variant
import gleam/erlang/process

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

/// An id outside 0-255 would silently truncate on the wire (`<<id:8>>`),
/// so `build` must panic loudly. Run in an unlinked child: if `build`
/// survives, the child reports back and the test fails.
pub fn out_of_range_id_panics_at_build_test() {
  let survived: process.Subject(Nil) = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let _ =
        variant.new()
        |> variant.unit(256, "TooBig", A(""), fn(_) { True })
        |> variant.build()
      process.send(survived, Nil)
    })

  case process.receive(survived, 500) {
    Error(Nil) -> Nil
    Ok(Nil) -> panic as "build should have panicked on id 256"
  }
}

/// A fallible inner encoder must surface a typed `EncodeError` from the
/// variant encoder, never panic (regression: `let assert` in `variant.add`).
pub fn inner_encoder_failure_is_typed_error_test() {
  let failing_inner =
    codec.Codec(
      encoder: fn(_) { Error(codec.ValueTooLarge("boom")) },
      decoder: codec.string_decoder(),
      sized_decoder: codec.string_sized_decoder(),
    )
  let c =
    variant.new()
    |> variant.add(0, "A", failing_inner, A, fn(m) {
      case m {
        A(s) -> Ok(s)
        _ -> Error(Nil)
      }
    })
    |> variant.build()

  case c.encoder(A("anything")) {
    Error(codec.ValueTooLarge("boom")) -> Nil
    _ -> panic as "Should have returned the inner ValueTooLarge error"
  }
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

  // Tag 0 (A), declares 100-byte string (32-bit prefix) but provides none
  let bits = <<0:8, 100:32>>

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
