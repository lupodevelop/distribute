# Codec Documentation

## Overview

The codec system is the **foundation** of distributed messaging in distribute. Every value crossing a node boundary must be serialized to `BitArray` and deserialized back to its original type.

This module provides:

- **Zero-copy binary serialization** using `BitArray`
- **Composable encoders/decoders** for complex types
- **Type-safe** encode/decode operations returning `Result`
- **SizedDecoder** for stateful byte tracking (lists, nested structures)
- **Schema helper** for envelope versioning

### Key Principle

**All serialization is explicit and type-safe.** No runtime reflection, no `Dynamic`, no surprises.

---

## Quick Start

### Basic Encoding/Decoding

```gleam
import distribute/codec

// Encode a string
let encoder = codec.string_encoder()
let encoded = codec.encode(encoder, "hello")
// Ok(<<5:16>> <> <<"hello">>)

// Decode back
let decoder = codec.string_decoder()
let decoded = codec.decode(decoder, encoded)
// Ok("hello")
```

### Working with Records

```gleam
// Define your message type
pub type UserMessage {
  UpdateName(id: Int, name: String)
}

// Create encoder
fn user_message_encoder() -> codec.Encoder(UserMessage) {
  fn(msg) {
    case msg {
      UpdateName(id, name) -> {
        let int_enc = codec.int_encoder()
        let str_enc = codec.string_encoder()
        
        let id_bits = codec.encode(int_enc, id)
        let name_bits = codec.encode(str_enc, name)
        
        case #(id_bits, name_bits) {
          #(Ok(ib), Ok(nb)) -> Ok(bit_array.append(ib, nb))
          _ -> Error(codec.InvalidValue("Failed to encode"))
        }
      }
    }
  }
}
```

---

## Core Concepts

### 1. Encoders

An `Encoder(a)` is a function that converts a value to `BitArray`:

```gleam
pub type Encoder(a) = fn(a) -> Result(BitArray, EncodeError)
```

**Built-in encoders:**
- `int_encoder()` — 64-bit big-endian integer (8 bytes)
- `string_encoder()` — UTF-8 with 16-bit length prefix
- `bool_encoder()` — Single byte (0 or 1)
- `float_encoder()` — 64-bit IEEE double
- `option_encoder(inner)` — Wraps `Option(a)` encoding
- `list_encoder(inner)` — Multiple values (length-prefixed)

### 2. Decoders (Simple)

A `Decoder(a)` expects to consume the **entire input**:

```gleam
pub type Decoder(a) = fn(BitArray) -> Result(a, DecodeError)
```

Use for **top-level message decoding**.

**Built-in decoders:**
- `int_decoder()` — 64-bit big-endian integer (8 bytes)
- `string_decoder()` — UTF-8 with 16-bit length prefix
- `bool_decoder()` — Single byte
- `float_decoder()` — 64-bit IEEE double

### 3. SizedDecoders (Stateful)

A `SizedDecoder(a)` returns **both the value AND remaining bytes**:

```gleam
pub type SizedDecoder(a) = fn(BitArray) -> Result(#(a, BitArray), DecodeError)
```

Use for **composing decoders** — essential for lists, tuples, nested structures.

**Why SizedDecoder?**

When decoding a list, you don't know how many bytes each element occupies. A SizedDecoder tracks bytes consumed so the next element starts at the right position.

```gleam
// Decoding a list of integers
fn list_of_ints_decoder() -> Decoder(List(Int)) {
  let sized = codec.list_sized_decoder(codec.int_sized_decoder())
  codec.to_decoder(sized)
}
```

---

## Composing Encoders/Decoders

### Pattern: Record to Bytes

```gleam
pub type Point {
  Point(x: Float, y: Float)
}

// Encoder: Point -> BitArray
fn point_encoder() -> codec.Encoder(Point) {
  fn(point) {
    use x_bits <- result.try(codec.encode(codec.float_encoder(), point.x))
    use y_bits <- result.try(codec.encode(codec.float_encoder(), point.y))
    Ok(bit_array.append(x_bits, y_bits))
  }
}

// Decoder: BitArray -> Point
fn point_decoder() -> codec.Decoder(Point) {
  fn(data) {
    use x_bits, rest1 <- result.try_map(
      codec.decode_sized(codec.float_sized_decoder(), data),
      fn(x) { #(x, rest1) }
    )
    use y_bits, _rest2 <- result.try_map(
      codec.decode_sized(codec.float_sized_decoder(), rest1),
      fn(y) { #(y, _rest2) }
    )
    Ok(Point(x: x_bits, y: y_bits))
  }
}
```

### Pattern: Enum with Tags

```gleam
pub type Action {
  Increment(Int)
  Decrement(Int)
  Reset
}

fn action_encoder() -> codec.Encoder(Action) {
  fn(action) {
    case action {
      Increment(n) -> {
        let tag = <<0:8>>
        use n_bits <- result.try(codec.encode(codec.int_encoder(), n))
        Ok(bit_array.append(tag, n_bits))
      }
      Decrement(n) -> {
        let tag = <<1:8>>
        use n_bits <- result.try(codec.encode(codec.int_encoder(), n))
        Ok(bit_array.append(tag, n_bits))
      }
      Reset -> Ok(<<2:8>>)
    }
  }
}

fn action_decoder() -> codec.Decoder(Action) {
  fn(data) {
    case data {
      <<0:8, rest:bytes>> -> {
        case codec.decode(codec.int_decoder(), rest) {
          Ok(n) -> Ok(Increment(n))
          Error(e) -> Error(e)
        }
      }
      <<1:8, rest:bytes>> -> {
        case codec.decode(codec.int_decoder(), rest) {
          Ok(n) -> Ok(Decrement(n))
          Error(e) -> Error(e)
        }
      }
      <<2:8>> -> Ok(Reset)
      _ -> Error(codec.InvalidBinary("Unknown action tag"))
    }
  }
}
```

---

## Envelope & Schema (Versioning)

The **envelope** wraps messages with metadata for safety:

```
[tag: 4 bytes][version: 1 byte][payload: N bytes]
```

### Using Schema Helper

```gleam
import distribute/codec

// Define schema for versioning
let user_schema = codec.Schema(
  tag: "User",
  version: 1,
  encoder: user_encoder(),
  decoder: user_decoder(),
  migrations: [],  // Empty for v1
)

// Encode with envelope
let encoded = codec.encode_with_schema(user_schema, user_value)
// Ok(<<"User":32, 1:8, ... payload ...>>)

// Decode with envelope (validates tag and version)
let decoded = codec.decode_with_schema(user_schema, encoded)
// Ok(user_value) — or error if tag/version mismatch
```

### Migration Example

If you upgrade from v1 to v2, define a migration:

```gleam
// v1: {name: String}
// v2: {name: String, email: String}

let user_schema_v2 = codec.Schema(
  tag: "User",
  version: 2,
  encoder: user_v2_encoder(),
  decoder: user_v2_decoder(),
  migrations: [
    codec.Migration(
      from_version: 1,
      migrate: fn(old_bytes) {
        // Parse v1, add email field, return v2 bytes
        use old <- result.try(decode_v1(old_bytes))
        encode_v2(UserV2(name: old.name, email: ""))
      }
    ),
  ],
)

// Decoder now accepts both v1 and v2
let decoded = codec.decode_with_schema(user_schema_v2, encoded)
// If v1: applies migration, returns v2
// If v2: decodes directly
```

---

## Error Handling

All encode/decode operations return `Result`:

```gleam
pub type EncodeError {
  InvalidValue(String)
  EncodeFailed(String)
  ValueTooLarge(String)
}

pub type DecodeError {
  InvalidBinary(String)
  TypeMismatch(String)
  DecodeFailed(String)
  InsufficientData(String)
  DecodeTimeout
  TagMismatch(expected: String, got: String)
  VersionMismatch(expected: Int, got: Int)
  MigrationMissing(Int)
  MigrationFailed(String)
}
```

### Pattern: Handling Errors

```gleam
case codec.encode(my_encoder, value) {
  Ok(bits) -> send_to_node(bits)
  Error(codec.ValueTooLarge(msg)) -> {
    // Log error, maybe split into chunks
    logger.error("Message too large: " <> msg)
  }
  Error(codec.InvalidValue(msg)) -> {
    // Value validation failed before encoding
    logger.error("Invalid value: " <> msg)
  }
  Error(e) -> {
    // Other encoding failures
    logger.error("Encoding failed: " <> string.inspect(e))
  }
}

case codec.decode(my_decoder, received_bits) {
  Ok(value) -> handle_message(value)
  Error(codec.InsufficientData(_)) -> {
    // Received incomplete message, request retransmit
    logger.warn("Incomplete message received")
  }
  Error(codec.TypeMismatch(msg)) -> {
    // Type doesn't match expected
    logger.error("Type mismatch: " <> msg)
  }
  Error(e) -> {
    logger.error("Decode failed: " <> string.inspect(e))
  }
}
```

---

## Built-in Codecs Reference

| Type | Encoder | Decoder | SizedDecoder | Notes |
|------|---------|---------|--------------|-------|
| `Int` | `int_encoder()` | `int_decoder()` | `int_sized_decoder()` | 32-bit signed |
| `Float` | `float_encoder()` | `float_decoder()` | `float_sized_decoder()` | 64-bit IEEE |
| `String` | `string_encoder()` | `string_decoder()` | `string_sized_decoder()` | UTF-8 + 16-bit len |
| `Bool` | `bool_encoder()` | `bool_decoder()` | `bool_sized_decoder()` | 1 byte |
| `Option(a)` | `option_encoder(inner)` | `option_decoder(inner)` | `option_sized_decoder(inner)` | 1-byte tag |
| `List(a)` | `list_encoder(inner)` | `list_decoder(inner)` | `list_sized_decoder(inner)` | 32-bit length prefix |
| `Bitarray` | `bitarray_encoder()` | `bitarray_decoder()` | `bitarray_sized_decoder()` | 32-bit len |
| `Pid` | `pid_encoder()` | `pid_decoder()` | `pid_sized_decoder()` | FFI to Erlang |
| `Subject(msg)` | `subject_encoder()` | `subject_decoder()` | `subject_sized_decoder()` | FFI to Erlang |

---

## Best Practices

1. **Always use SizedDecoder for composition** — When building larger codecs, use `SizedDecoder` internally, then wrap in `Decoder` at the top level
2. **Validate on decode** — Check bounds, lengths, and ranges after decoding
3. **Use Schema for versioning** — Define tag/version, document migrations
4. **Keep encoders simple** — One concern per encoder function
5. **Test with fuzzing** — Random inputs should never crash, only return errors
6. **Log errors** — Always log decode failures for debugging
7. **Use envelopes** — For cross-node messages, always include type metadata

---

## Next Steps

- [Global Subjects (Type-Safe)](./../global/README.md) — How to send typed messages across nodes
- [Registry](./../registry/README.md) — How to register and look up services with type-safe names
- [Messaging](./../messaging/README.md) — High-level send/call patterns using codecs

---

*For security and cryptographic concerns, see [docs/crypto/README.md](../crypto/README.md).*
