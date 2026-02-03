import distribute/cluster

pub fn classify_start_reason_test() {
  let assert cluster.AlreadyStarted =
    cluster.classify_start_reason("already_started")
  let assert cluster.CookieTooLong =
    cluster.classify_start_reason("cookie_too_long")
  let assert cluster.NetworkError(_) =
    cluster.classify_start_reason("some network partition")
}

pub fn classify_connect_reason_test() {
  let assert cluster.ConnectTimeout =
    cluster.classify_connect_reason("timeout while connecting")
  let assert cluster.ConnectNetworkError(_) =
    cluster.classify_connect_reason("network unreachable")
}
