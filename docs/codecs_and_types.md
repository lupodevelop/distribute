# Codecs and Types

Since the BEAM transmits raw binaries across nodes, `distribute` requires explicit codecs to ensure type safety.

## Built-in Codecs

The `distribute/codec` module provides the building blocks:

### Primitives

- `codec.int()`
- `codec.string()`
- `codec.float()`
- `codec.bool()`
- `codec.bitarray()`
- `codec.nil()`
- `codec.subject()` (Special: serializes Erlang Subjects including node info)

### Composites (Built-in)

- `codec.list(inner_codec)`

### Composites (Module: `distribute/codec/composite`)

- `composite.option(inner)`
- `composite.result(ok, err)`
- `composite.tuple2(a, b)`
- `composite.tuple3(a, b, c)`

## Custom Types

Gleam does not have reflection or macros. To use your own records, use `codec.map`:

```gleam
import distribute/codec

pub type User {
  User(id: Int, name: String)
}

fn user_codec() {
  // 1. Pack into a tuple
  let tuple_codec = composite.tuple2(codec.int(), codec.string())

  // 2. Map to your record
  codec.map(
    tuple_codec,
    fn(t) { User(t.0, t.1) },      // Wrap
    fn(u) { #(u.id, u.name) },    // Unwrap
  )
}
```

## Tagged Messages (Versioned Protocols)

During rolling deployments, nodes with different code versions might communicate. `distribute/codec/tagged` helps you version your protocol to prevent crashes when breaking changes occur.

The wire format for a tagged message is:
`[tag_len:32][tag_bytes][version:32][payload]`

### Creating a Tagged Codec

```gleam
import distribute/codec
import distribute/codec/tagged

type MyMsg { Hello }

// This codec will ONLY accept messages with tag "auth" and version 1.
// If version 2 arrives, it returns `Error(VersionMismatch)`.
let auth_codec = tagged.codec("auth", 1, my_payload_codec)
```

## Wire Format

| Codec | Length prefix | Notes |
| --- | --- | --- |
| `string` | 32-bit (unsigned BE) | UTF-8 bytes; encoder rejects > 4 GiB |
| `bitarray` | 32-bit (unsigned BE) | Raw bytes; encoder rejects > 4 GiB |
| `list` | 32-bit element count | Encoder rejects > 10 000 elements |
| `int` | none, fixed 64-bit | Signed BE; encoder rejects out-of-`[-2^63, 2^63-1]` |
| `float` | none, fixed 64-bit | IEEE-754 BE |
| `bool` | none, fixed 1 byte | `0`/`1` |
| `nil` | zero bytes | - |
| `subject` | 32-bit + ETF binary | `term_to_binary/binary_to_term[safe]` |

## Binary Compatibility

v4 nodes are not wire-compatible with v3 nodes. `string` and `bitarray`
prefixes widened from 16 to 32 bits, and the `int` encoder now rejects
out-of-range values that v3 silently truncated.

---

- [Understand Safety and Limits](./safety_and_limits.md)
