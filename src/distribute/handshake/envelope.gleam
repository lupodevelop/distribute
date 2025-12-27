pub type Envelope {
  Envelope(tag: String, version: Int, payload: BitArray)
}

pub fn peek_tag(_binary: BitArray) -> Result(#(String, Int), String) {
  // Stub: real implementation should parse the envelope without decoding payload
  Ok(#("unknown", 0))
}

pub fn wrap(_tag: String, _version: Int, payload: BitArray) -> BitArray {
  // Stub: real implementation should serialize envelope header + payload
  payload
}

pub fn unwrap(binary: BitArray) -> Result(#(String, Int, BitArray), String) {
  // Stub: real implementation should return tag, version, payload
  Ok(#("unknown", 0, binary))
}
