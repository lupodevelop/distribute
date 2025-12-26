# Type-Safe Messaging Example

This example demonstrates the complete type-safe API of the distribute library, including:

- Binary encoding/decoding with codecs
- Type-safe message sending with `Subject(BitArray)`
- Type-safe message receiving with `receiver` helpers
- Process groups with typed broadcast
- Remote procedure calls with typed arguments

## Running the Example

This example requires two Erlang nodes. Open two terminals:

**Terminal 1:**
```bash
cd examples/typed_messaging
gleam run -m typed_node_a
```

**Terminal 2:**
```bash
gleam run -m typed_node_b
```

The nodes will:
1. Connect to each other
2. Exchange type-safe messages using codecs
3. Use process groups for pub/sub
4. Make type-safe remote procedure calls

## Key Concepts

### 1. Codecs for Type Safety

Instead of sending raw Erlang terms (unsafe), we use binary codecs:

```gleam
import distribute/codec

// Define encoders/decoders
let encoder = codec.string_encoder()
let decoder = codec.string_decoder()

// Encode message
let assert Ok(binary_msg) = codec.encode(encoder, "Hello")

// Decode message
let assert Ok(msg) = codec.decode(decoder, binary_msg)
```

### 2. Typed Message Sending

Use `Subject(BitArray)` from `gleam/erlang/process` and the `_typed` API variants:

```gleam
import gleam/erlang/process
import distribute/messaging

let subject = process.new_subject()
let encoder = codec.int_encoder()

// Type-safe send
let result = messaging.send_typed(subject, 42, encoder)
```

### 3. Typed Message Receiving

Use `receiver` helpers for convenient typed reception:

```gleam
import distribute/receiver

let decoder = codec.int_decoder()
case receiver.receive_typed(subject, decoder, 1000) {
  Ok(num) -> io.debug(num)
  Error(receiver.Timeout) -> io.println("Timeout")
  Error(receiver.DecodeError(_)) -> io.println("Bad data")
}
```

### 4. Process Groups (Pub/Sub)

Type-safe broadcast to multiple subscribers:

```gleam
import distribute/groups

// Join group with typed subject
let subject = process.new_subject()
groups.join_typed("my_group", subject)

// Broadcast typed message
let encoder = codec.string_encoder()
groups.broadcast_typed("my_group", "Hello all!", encoder)
```

### 5. Remote Procedure Calls

Type-safe RPC with binary arguments:

```gleam
import distribute/remote_call

let arg_encoder = codec.int_encoder()
let result_decoder = codec.string_decoder()

case remote_call.call_typed(
  "node@host",
  "my_module",
  "my_function",
  [10, 20],
  arg_encoder,
  result_decoder,
  5000
) {
  Ok(result) -> io.debug(result)
  Error(err) -> io.println("RPC failed")
}
```

## Why Type-Safe?

The typed API provides:

1. **Compile-time safety**: Message types are checked by the compiler
2. **Runtime validation**: Envelope tags and versions catch protocol mismatches
3. **Explicit errors**: Encoding/decoding errors are explicit `Result` values
4. **gleam_otp integration**: Works seamlessly with `gleam/erlang/process` ecosystem
5. **Cross-language safety**: Binary format protects against Erlang/Elixir interop issues

## Migration from Unsafe API

Old (unsafe):
```gleam
import distribute/messaging

messaging.send(pid, "any value")  // No type checking!
```

New (type-safe):
```gleam
import distribute/messaging
import distribute/codec
import gleam/erlang/process

let subject = process.new_subject()
let encoder = codec.string_encoder()

case messaging.send_typed(subject, "typed value", encoder) {
  Ok(_) -> io.println("Sent!")
  Error(err) -> io.println("Encode failed")
}
```

See `MIGRATION.md` for a complete migration guide.
