pub type Capability {
  Capability(protocol: String, min: Int, max: Int)
}

pub type NodeCapabilities =
  List(Capability)

pub fn make(protocol: String, min: Int, max: Int) -> Capability {
  Capability(protocol, min, max)
}

pub fn protocol(c: Capability) -> String {
  case c {
    Capability(p, _, _) -> p
  }
}

pub fn min_version(c: Capability) -> Int {
  case c {
    Capability(_, m, _) -> m
  }
}

pub fn max_version(c: Capability) -> Int {
  case c {
    Capability(_, _, x) -> x
  }
}
