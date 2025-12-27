import gleam/option.{type Option, None}

// Minimal stubs for negotiation registry APIs. These are intentionally
// simple: registration is a no-op and `protocol_negotiate` returns `None`.
// They serve as placeholders until a persistent registry (ETS or process)
// is added.

pub fn register_node_capabilities(_node: String, _capabilities: List(a)) -> Nil {
  Nil
}

pub fn protocol_negotiate(_node: String, _protocol: String) -> Option(Int) {
  None
}

pub fn schema_encode_for_node(
  _schema: a,
  _value: a,
  _node: String,
) -> Result(BitArray, String) {
  Error("not_implemented")
}

pub fn schema_decode_from_node(
  _schema: a,
  _binary: BitArray,
  _node: String,
) -> Result(a, String) {
  Error("not_implemented")
}
