import distribute/crypto/provider
import distribute/crypto/provider_stub
import distribute/handshake
import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}

// Simple handshake state machine for initiator/responder roles.

pub type Role {
  Initiator
  Responder
}

pub type HandshakeState {
  InitiatorState(
    state: provider.ProviderState,
    local_hello: handshake.Hello,
    negotiated: Option(#(String, Int)),
    secure: Option(provider.SecureContext),
    waiting: Option(String),
    retries: Int,
    pending: Option(BitArray),
  )
  ResponderState(
    state: provider.ProviderState,
    negotiated: Option(#(String, Int)),
    secure: Option(provider.SecureContext),
    waiting: Option(String),
    retries: Int,
    pending: Option(BitArray),
  )
  Established(#(String, provider.SecureContext))
  Failed(String)
}

pub type Outcome {
  Sent(BitArray, HandshakeState)
  Received(Option(BitArray), HandshakeState)
}

// Start initiator: produce Hello and new state
pub fn initiator_start(local: handshake.Hello) -> #(BitArray, HandshakeState) {
  let state = provider_stub.init()
  let hello_b = handshake.encode_hello(local)
  case hello_b {
    Ok(bytes) -> #(
      bytes,
      InitiatorState(
        state,
        local,
        None,
        None,
        Some("capabilities"),
        0,
        Some(bytes),
      ),
    )
    Error(_) -> #(<<>>, Failed("encode hello failed"))
  }
}

// Responder initial state
pub fn responder_init() -> HandshakeState {
  ResponderState(provider_stub.init(), None, None, None, 0, None)
}

// Handle incoming message as responder; return optional outgoing message and new state
pub fn responder_handle_message(
  state: HandshakeState,
  data: BitArray,
) -> Result(Outcome, String) {
  case state {
    ResponderState(pstate, negotiated, secure, _waiting, _retries, _pending) -> {
      // Try hello
      case handshake.decode_hello(data) {
        Ok(h) -> {
          let caps = handshake.CapabilitiesMsg(h.capabilities)
          case handshake.encode_capabilities(caps) {
            Ok(caps_b) ->
              Ok(Sent(
                caps_b,
                ResponderState(
                  pstate,
                  negotiated,
                  secure,
                  Some("accept"),
                  0,
                  Some(caps_b),
                ),
              ))
            Error(_) -> Error("encode capabilities failed")
          }
        }
        Error(_) -> {
          // Try accept
          case handshake.decode_accept(data) {
            Ok(a) -> {
              let #(out, new_state) =
                provider_stub.start_key_exchange(pstate, <<>>)
              case handshake.encode_keyexchange(handshake.KeyExchangeMsg(out)) {
                Ok(ke_b) ->
                  Ok(Sent(
                    ke_b,
                    ResponderState(
                      new_state,
                      Some(#(a.protocol, a.version)),
                      None,
                      Some("ke"),
                      0,
                      Some(ke_b),
                    ),
                  ))
                Error(_) -> Error("encode keyexchange failed")
              }
            }
            Error(_) -> {
              // Try key exchange
              case handshake.decode_keyexchange(data) {
                Ok(k) -> {
                  let #(maybe_out, new_state2, maybe_ctx) =
                    provider_stub.handle_key_exchange(pstate, k.payload)
                  case maybe_ctx {
                    Some(ctx) ->
                      Ok(Received(None, Established(#("negotiated", ctx))))
                    None -> {
                      case maybe_out {
                        Some(out2) ->
                          case
                            handshake.encode_keyexchange(
                              handshake.KeyExchangeMsg(out2),
                            )
                          {
                            Ok(ob) ->
                              Ok(Sent(
                                ob,
                                ResponderState(
                                  new_state2,
                                  negotiated,
                                  None,
                                  Some("ke"),
                                  0,
                                  Some(ob),
                                ),
                              ))
                            Error(_) -> Error("encode keyexchange failed")
                          }
                        None ->
                          Ok(Received(
                            Some(<<>>),
                            ResponderState(
                              new_state2,
                              negotiated,
                              None,
                              None,
                              0,
                              None,
                            ),
                          ))
                      }
                    }
                  }
                }
                Error(_) -> Error("unrecognized message")
              }
            }
          }
        }
      }
    }
    _ -> Error("invalid state for responder")
  }
}

// Handle incoming message as initiator
pub fn initiator_handle_message(
  state: HandshakeState,
  data: BitArray,
) -> Result(Outcome, String) {
  case state {
    InitiatorState(
      pstate,
      local_hello,
      negotiated,
      _secure,
      _waiting,
      _retries,
      _pending,
    ) -> {
      // Expect capabilities
      case handshake.decode_capabilities(data) {
        Ok(caps) -> {
          // Find a mutual capability; pick the first that matches local_hello
          let match_opt =
            find_mutual(local_hello.capabilities, caps.capabilities)
          case match_opt {
            Some(#(proto, ver)) -> {
              // Send Accept and our initial key exchange
              let accept = handshake.Accept(proto, ver, [])
              case handshake.encode_accept(accept) {
                Ok(acc_b) -> {
                  // start key exchange
                  let #(ke_out, new_state) =
                    provider_stub.start_key_exchange(pstate, <<>>)
                  case
                    handshake.encode_keyexchange(handshake.KeyExchangeMsg(
                      ke_out,
                    ))
                  {
                    Ok(ke_b) ->
                      Ok(Sent(
                        bit_array.append(acc_b, ke_b),
                        InitiatorState(
                          new_state,
                          local_hello,
                          Some(#(proto, ver)),
                          None,
                          Some("ke"),
                          0,
                          Some(bit_array.append(acc_b, ke_b)),
                        ),
                      ))
                    Error(_) -> Error("encode keyexchange failed")
                  }
                }
                Error(_) -> Error("encode accept failed")
              }
            }
            None -> {
              case
                handshake.encode_reject(handshake.Reject(
                  "no matching capability",
                ))
              {
                Ok(rb) -> Ok(Sent(rb, Failed("no match")))
                Error(_) -> Error("encode reject failed")
              }
            }
          }
        }
        Error(_) -> {
          // Maybe keyexchange
          case handshake.decode_keyexchange(data) {
            Ok(k) -> {
              let #(maybe_out, new_state2, maybe_ctx) =
                provider_stub.handle_key_exchange(pstate, k.payload)
              case maybe_ctx {
                Some(ctx) ->
                  Ok(Received(None, Established(#("negotiated", ctx))))
                None -> {
                  case maybe_out {
                    Some(o) ->
                      case
                        handshake.encode_keyexchange(handshake.KeyExchangeMsg(o))
                      {
                        Ok(ob) ->
                          Ok(Sent(
                            ob,
                            InitiatorState(
                              new_state2,
                              local_hello,
                              negotiated,
                              None,
                              None,
                              0,
                              None,
                            ),
                          ))
                        Error(_) -> Error("encode keyexchange failed")
                      }
                    None ->
                      Ok(Received(
                        None,
                        InitiatorState(
                          new_state2,
                          local_hello,
                          negotiated,
                          None,
                          None,
                          0,
                          None,
                        ),
                      ))
                  }
                }
              }
            }
            Error(_) -> Error("unrecognized message")
          }
        }
      }
    }
    _ -> Error("invalid state for initiator")
  }
}

// Handle timeout (external trigger) — retransmit or fail
pub fn handshake_on_timeout(state: HandshakeState) -> Result(Outcome, String) {
  let max_accept_retries = 3
  let max_ke_retries = 3
  case state {
    InitiatorState(pstate, local, negotiated, secure, waiting, retries, pending) -> {
      case waiting {
        Some(tag) -> {
          case pending {
            Some(bytes) -> {
              case tag {
                "capabilities" ->
                  case retries < max_accept_retries {
                    True ->
                      Ok(Sent(
                        bytes,
                        InitiatorState(
                          pstate,
                          local,
                          negotiated,
                          secure,
                          Some("capabilities"),
                          retries + 1,
                          Some(bytes),
                        ),
                      ))
                    False -> Ok(Received(None, Failed("capabilities timeout")))
                  }
                "ke" ->
                  case retries < max_ke_retries {
                    True ->
                      Ok(Sent(
                        bytes,
                        InitiatorState(
                          pstate,
                          local,
                          negotiated,
                          secure,
                          Some("ke"),
                          retries + 1,
                          Some(bytes),
                        ),
                      ))
                    False -> Ok(Received(None, Failed("key exchange timeout")))
                  }
                _ -> Error("unknown waiting tag")
              }
            }
            None -> Error("no pending message to retransmit")
          }
        }
        None -> Error("nothing waiting")
      }
    }
    ResponderState(pstate, negotiated, secure, waiting, retries, pending) -> {
      case waiting {
        Some(tag) -> {
          case pending {
            Some(bytes) -> {
              case tag {
                "accept" ->
                  case retries < max_accept_retries {
                    True ->
                      Ok(Sent(
                        bytes,
                        ResponderState(
                          pstate,
                          negotiated,
                          secure,
                          Some("accept"),
                          retries + 1,
                          Some(bytes),
                        ),
                      ))
                    False -> Ok(Received(None, Failed("accept timeout")))
                  }
                "ke" ->
                  case retries < max_ke_retries {
                    True ->
                      Ok(Sent(
                        bytes,
                        ResponderState(
                          pstate,
                          negotiated,
                          secure,
                          Some("ke"),
                          retries + 1,
                          Some(bytes),
                        ),
                      ))
                    False -> Ok(Received(None, Failed("key exchange timeout")))
                  }
                _ -> Error("unknown waiting tag")
              }
            }
            None -> Error("no pending message to retransmit")
          }
        }
        None -> Error("nothing waiting")
      }
    }
    _ -> Error("no timeout handling for this state")
  }
}

fn find_mutual(
  local_caps: List(handshake.Capability),
  remote_caps: List(handshake.Capability),
) -> Option(#(String, Int)) {
  // Find first matching protocol and overlapping version
  case local_caps {
    [] -> None
    [handshake.Capability(protocol: lp, min: lmin, max: lmax, meta: _), ..rest] ->
      case
        list.find(remote_caps, fn(rc) {
          case rc {
            handshake.Capability(protocol: rp, min: rmin, max: rmax, meta: _) ->
              rp == lp && rmin <= lmax && rmax >= lmin
          }
        })
      {
        Ok(cap_found) -> {
          case cap_found {
            handshake.Capability(protocol: _rp, min: rmin, max: _rmax, meta: _) -> {
              // Choose min overlap
              case rmin > lmin {
                True -> Some(#(lp, rmin))
                False -> Some(#(lp, lmin))
              }
            }
          }
        }
        Error(_) -> find_mutual(rest, remote_caps)
      }
  }
}
