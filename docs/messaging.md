# Messaging Patterns

`distribute` supports two primary communication patterns: fire-and-forget (asynchronous) and request-response (synchronous).

## The Lifecycle of a Message

Regardless of the pattern, every message in `distribute` follows a strict sequence to ensure safety:

1. **Selection**: You target a `GlobalSubject(msg)`.
2. **Serialization**: The message is converted to a `BitArray` using the subject's encoder.
3. **Boundary Check**: The binary is checked against the global `max_payload_size_bytes`.
4. **Transport**: The bits are sent over the BEAM network to the target node.
5. **Rejection/Acceptance**: The receiver checks the binary size (before decoding).
6. **De-serialization**: The receiver converts the bits back into a typed Gleam value.

This sequence guarantees that your actor's main logic only ever processes valid, type-safe data.

## Asynchronous: `send`

Sending a message is non-blocking. The message is encoded, checked against payload limits, and enqueued in the target's mailbox.

```gleam
import distribute
import distribute/global

let assert Ok(Nil) = distribute.send(target_gs, MyMessage)
```

### Direct Reception

If you own the `GlobalSubject`, you can receive messages manually:

```gleam
import distribute

case distribute.receive_with_timeout(my_gs, 1000) {
  Ok(msg) -> handle(msg)
  // err is a codec.DecodeError. e.g. DecodeTimeout, PayloadTooLarge,
  // InvalidBinary, etc. Render it via codec.decode_error_to_string.
  Error(err) -> handle_error(err)
}
```

`distribute.receive(my_gs)` is the default-form shortcut that uses
`config.default_call_timeout_ms`.

## Synchronous: `call`

The `call` pattern implements a request-response cycle. It is much safer than manual `send` + `receive` because it uses **Erlang process monitors** to detect if the target dies during the call.

### 1. Define your protocol

Include a `process.Subject(BitArray)` for the reply. Use `codec.subject()` to manage its serialization.

```gleam
type MathReq {
  Add(a: Int, b: Int, reply: process.Subject(BitArray))
}
```

### 2. Implementation (Handler)

Use `global.reply` to send the response.

```gleam
fn handle(msg, state) {
  case msg {
    Add(a, b, reply_to) -> {
      let _ = global.reply(reply_to, a + b, codec.int_encoder())
      receiver.Continue(state)
    }
  }
}
```

### 3. Calling (Client)

Use `distribute.call` for the common case.

```gleam
let assert Ok(sum) = distribute.call(
  math_actor,
  fn(reply_to) { Add(10, 20, reply_to) },
  codec.int_decoder(),
)
```

## Benefits of `call`

- **Fast-fail**: If the remote node crashes or the actor is killed, `call` returns `Error(TargetDown)` immediately without waiting for the timeout.
- **Type Safety**: The reply is decoded using the decoder you provide at the call site.

## When to use `call_isolated`

`distribute.call` drains replies already present in the mailbox when the
timeout fires, but a reply that arrives **after** the function has already
returned can still orphan in a long-running caller. That is usually fine for
short-lived callers (CLI tools, request handlers that exit immediately after
replying), but it is the wrong default for long-lived actors or manager
processes that issue many RPCs under sustained timeouts.

For that shape, prefer `distribute.call_isolated`:

```gleam
let assert Ok(sum) = distribute.call_isolated(
  math_actor,
  fn(reply_to) { Add(10, 20, reply_to) },
  codec.int_decoder(),
)
```

`call_isolated` runs the inner `call` inside a short-lived proxy process. On
caller-side timeout it kills the proxy, waits for its `DOWN`, and only then
returns, so a late proxy send cannot re-pollute the caller mailbox after the
timeout path completes.

## Cluster Events

You can monitor the cluster itself by starting a `cluster_monitor`.

```gleam
import distribute
import distribute/cluster_monitor

let assert Ok(mon) = distribute.start_monitor()
let events = process.new_subject()
distribute.subscribe(mon, events)

// Later...
let assert Ok(cluster_monitor.NodeDown(node_name)) = process.receive(events, 5000)
```

## `ping` diagnostics in production

`cluster.ping` returns only `Bool` by design:

- `True` means the target answered.
- `False` means "cannot reach" (unreachable peer, invalid name, or atom-budget refusal).

If you need root-cause diagnostics, use telemetry plus typed APIs:

- `cluster.connect` / `cluster.start_node` return typed errors.
- telemetry emits `AtomBudgetExhausted(_, AtomBudgetOnPing)` on ping budget refusal.
- `cluster.health` gives the reachable/unreachable partition snapshot for operator workflows.

---

- [Learn how to build Codecs](./codecs_and_types.md)
- [Understand Safety and Limits](./safety_and_limits.md)
