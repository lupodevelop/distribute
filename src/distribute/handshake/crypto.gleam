import gleam/option.{type Option, None}

pub type Provider {
  Provider(name: String)
}

pub type SecureContext {
  SecureContext(info: String)
}

pub fn register_provider(_p: Provider) -> Result(Nil, String) {
  // Stub: register a crypto provider
  Ok(Nil)
}

pub fn get_secure_context(_node: String) -> Option(SecureContext) {
  // Stub: return an opaque secure context for a node if available
  None
}
