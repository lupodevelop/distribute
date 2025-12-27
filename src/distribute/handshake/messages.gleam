import distribute/capability.{type NodeCapabilities}

pub type Hello {
  Hello(node_info: String, capabilities: NodeCapabilities)
}

pub type Capabilities {
  Capabilities(capabilities: NodeCapabilities)
}

pub type Accept {
  Accept(protocol: String, version: Int)
}

pub type Reject {
  Reject(reason: String)
}
