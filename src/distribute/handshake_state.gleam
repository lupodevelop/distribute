import distribute/handshake.{type Hello, type Capability, CapabilitiesMsg, Accept, Reject, KeyExchangeMsg, Capability, encode_hello, encode_capabilities, encode_accept, encode_reject, encode_keyexchange, decode_capabilities, decode_accept, decode_reject, decode_keyexchange, decode_hello}
import distribute/crypto/provider as provider
import distribute/crypto/provider_stub as provider_stub
import gleam/bit_array
import gleam/option.{type Option, Some, None}
import gleam/result
import gleam/list

// Simple handshake state machine for initiator/responder roles.

pub type Role {
  Initiator
  Responder
}

pub type HandshakeState {
  InitiatorState(
    state: provider.ProviderState,
    local_hello: Hello,
    negotiated: Option(#(String, Int)),
    secure: Option(provider.SecureContext),
  )
  ResponderState(
    state: provider.ProviderState,
    negotiated: Option(#(String, Int)),
    secure: Option(provider.SecureContext),
  )
  Established(#(String, provider.SecureContext))
  Failed(String)
}

pub type Outcome {
  Sent(BitArray, HandshakeState)
  Received(Option(BitArray), HandshakeState)
}

// Start initiator: produce Hello and new state
pub fn initiator_start(local: Hello) -> #(BitArray, HandshakeState) {
  let state = provider_stub.init()
  let hello_b = encode_hello(local)
  case hello_b {
    Ok(bytes) -> #(bytes, InitiatorState(state, local, None, None))
    Error(_) -> #(<<>>, Failed("encode hello failed"))
  }
}

// Responder initial state
pub fn responder_init() -> HandshakeState {
  ResponderState(provider_stub.init(), None, None)
}

// Handle incoming message as responder; return optional outgoing message and new state
pub fn responder_handle_message(state: HandshakeState, data: BitArray) -> Result(Outcome, String) {
  case state {
    ResponderState(pstate, negotiated, secure) -> {
      // Try hello
      case decode_hello(data) {
        Ok(h) -> {
          let caps = CapabilitiesMsg(h.capabilities)
          case encode_capabilities(caps) {
            Ok(caps_b) -> Ok(Sent(caps_b, ResponderState(pstate, negotiated, secure)))
            Error(_) -> Error("encode capabilities failed")
          }
        }
        Error(_) -> {
          // Try accept
          case decode_accept(data) {
            Ok(a) -> {
              let #(out, new_state) = provider_stub.start_key_exchange(pstate, <<>>)
              case encode_keyexchange(KeyExchangeMsg(out)) {
                Ok(ke_b) -> Ok(Sent(ke_b, ResponderState(new_state, Some(#(a.protocol, a.version)), None)))
                Error(_) -> Error("encode keyexchange failed")
              }
            }
            Error(_) -> {
              // Try key exchange
              case decode_keyexchange(data) {
                Ok(k) -> {
                  let #(maybe_out, new_state2, maybe_ctx) = provider_stub.handle_key_exchange(pstate, k.payload)
                  case maybe_ctx {
                    Some(ctx) -> Ok(Received(None, Established(#("negotiated", ctx))))
                    None -> {
                      case maybe_out {
                        Some(out2) ->
                          case encode_keyexchange(KeyExchangeMsg(out2)) {
                            Ok(out_b) -> Ok(Sent(out_b, ResponderState(new_state2, negotiated, None)))
                            Error(_) -> Error("encode keyexchange failed")
                          }
                        None -> Ok(Received(None, ResponderState(new_state2, negotiated, None)))
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
pub fn initiator_handle_message(state: HandshakeState, data: BitArray) -> Result(Outcome, String) {
  case state {
    InitiatorState(pstate, local_hello, negotiated, secure) -> {
      // Expect capabilities
      case decode_capabilities(data) {
        Ok(caps) -> {
          // Find a mutual capability; pick the first that matches local_hello
          let match_opt = find_mutual(local_hello.capabilities, caps.capabilities)
          case match_opt {
            Some(#(proto, ver)) -> {
              // Send Accept and our initial key exchange
              let accept = Accept(proto, ver, [])
              case encode_accept(accept) {
                Ok(acc_b) -> {
                  // start key exchange
                  let #(ke_out, new_state) = provider_stub.start_key_exchange(pstate, <<>>)
                  case encode_keyexchange(KeyExchangeMsg(ke_out)) {
                    Ok(ke_b) -> Ok(Sent(bit_array.append(acc_b, ke_b), InitiatorState(new_state, local_hello, Some(#(proto, ver)), None)))
                    Error(_) -> Error("encode keyexchange failed")
                  }
                }
                Error(_) -> Error("encode accept failed")
              }
            }
            None -> {
              case encode_reject(Reject("no matching capability")) {
                Ok(rb) -> Ok(Sent(rb, Failed("no match")))
                Error(_) -> Error("encode reject failed")
              }
            }
          }
        }
        Error(_) -> {
          // Maybe keyexchange
          case decode_keyexchange(data) {
            Ok(k) -> {
              let #(maybe_out, new_state2, maybe_ctx) = provider_stub.handle_key_exchange(pstate, k.payload)
              case maybe_ctx {
                Some(ctx) -> Ok(Received(None, Established(#("negotiated", ctx))))
                None -> {
                  case maybe_out {
                    Some(o) ->
                      case encode_keyexchange(KeyExchangeMsg(o)) {
                        Ok(ob) -> Ok(Sent(ob, InitiatorState(new_state2, local_hello, negotiated, None)))
                        Error(_) -> Error("encode keyexchange failed")
                      }
                    None -> Ok(Received(None, InitiatorState(new_state2, local_hello, negotiated, None)))
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

fn find_mutual(local_caps: List(Capability), remote_caps: List(Capability)) -> Option(#(String, Int)) {
  // Find first matching protocol and overlapping version
  case local_caps {
    [] -> None
    [Capability(protocol: lp, min: lmin, max: lmax, meta: _), ..rest] ->
      case list.find(remote_caps, fn(rc) {
        case rc {
          Capability(protocol: rp, min: rmin, max: rmax, meta: _) -> rp == lp && rmin <= lmax && rmax >= lmin
        }
      }) {
        Ok(cap_found) -> {
          case cap_found {
            Capability(protocol: rp, min: rmin, max: rmax, meta: _) -> {
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
