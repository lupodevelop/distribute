import gleam/option.{type Option, None}

pub fn protocol_negotiate(_node: String, _protocol: String) -> Option(Int) {
  // Stub: lookup node capabilities and return selected version if compatible
  None
}

pub fn schema_encode_for_node(
  _schema: a,
  _value: a,
  _node: String,
) -> Result(BitArray, String) {
  // Stub: use negotiated version for node to encode
  Error("not_implemented")
}

pub fn schema_decode_from_node(
  _schema: a,
  _binary: BitArray,
  _node: String,
) -> Result(a, String) {
  // Stub: use negotiated version for node to decode
  Error("not_implemented")
}
