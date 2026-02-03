import distribute/crypto/provider
import distribute/crypto/provider_stub
import distribute/handshake
import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}

// =============================================================================
// Crypto Provider Interface
// =============================================================================

/// Interface for crypto providers in the handshake state machine.
///
/// This type allows injecting different crypto implementations:
/// - `provider_stub` for testing (no real crypto)
/// - Custom implementations for production
///
/// ## Example
///
/// ```gleam
/// // Use stub for testing
/// let test_provider = stub_crypto_provider()
///
/// // Use custom provider
/// let custom_provider = CryptoProvider(
///   init: my_init,
///   start_key_exchange: my_start_ke,
///   handle_key_exchange: my_handle_ke,
/// )
/// ```
pub type CryptoProvider {
  CryptoProvider(
    init: fn() -> provider.ProviderState,
    start_key_exchange: fn(provider.ProviderState, BitArray) ->
      #(BitArray, provider.ProviderState),
    handle_key_exchange: fn(provider.ProviderState, BitArray) ->
      #(
        Option(BitArray),
        provider.ProviderState,
        Option(provider.SecureContext),
      ),
  )
}

/// Default stub crypto provider (insecure, for testing only).
///
/// Returns a `CryptoProvider` that wraps `provider_stub`, providing
/// passthrough encryption (no actual security).
///
/// ## Warning
///
/// **Do NOT use in production.** This provider performs no actual
/// cryptographic operations.
pub fn stub_crypto_provider() -> CryptoProvider {
  CryptoProvider(
    init: provider_stub.init,
    start_key_exchange: provider_stub.start_key_exchange,
    handle_key_exchange: provider_stub.handle_key_exchange,
  )
}

// =============================================================================
// Types
// =============================================================================

/// Handshake state machine for secure connection establishment.
/// 
/// This module implements the state transitions for both initiator and
/// responder roles in a cryptographic handshake. The state machine handles:
/// - Hello message exchange with capabilities
/// - Protocol/version negotiation
/// - Key exchange for establishing secure contexts
/// - Retry logic for transient failures
/// Role in the handshake - determines which state machine path to follow.
pub type Role {
  Initiator
  Responder
}

/// State of the handshake state machine.
/// 
/// The handshake progresses through several states depending on the role:
/// - `InitiatorState`: Active state for the connection initiator
/// - `ResponderState`: Active state for the connection responder
/// - `Established`: Terminal success state with secure context
/// - `Failed`: Terminal error state with reason
pub type HandshakeState {
  InitiatorState(
    state: provider.ProviderState,
    local_hello: handshake.Hello,
    negotiated: Option(#(String, Int)),
    secure: Option(provider.SecureContext),
    waiting: Option(String),
    retries: Int,
    pending: Option(BitArray),
    crypto: CryptoProvider,
  )
  ResponderState(
    state: provider.ProviderState,
    negotiated: Option(#(String, Int)),
    secure: Option(provider.SecureContext),
    waiting: Option(String),
    retries: Int,
    pending: Option(BitArray),
    crypto: CryptoProvider,
  )
  Established(#(String, provider.SecureContext))
  Failed(String)
}

/// Result of processing a handshake message.
/// 
/// - `Sent`: Produced an outgoing message and new state
/// - `Received`: Processed incoming message, optionally with reply
pub type Outcome {
  Sent(BitArray, HandshakeState)
  Received(Option(BitArray), HandshakeState)
}

/// Start the handshake as initiator.
/// 
/// Produces the initial Hello message to send to the responder
/// and returns the new initiator state.
///
/// ## Arguments
///
/// - `local` - Local node's Hello message with capabilities
/// - `crypto` - Crypto provider to use for key exchange
///
/// ## Example
///
/// ```gleam
/// let crypto = stub_crypto_provider()  // Use real provider in production!
/// let hello = handshake.Hello(node_id: "node1", capabilities: [...])
/// let #(msg, state) = initiator_start(hello, crypto)
/// ```
pub fn initiator_start(
  local: handshake.Hello,
  crypto: CryptoProvider,
) -> #(BitArray, HandshakeState) {
  let state = { crypto.init }()
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
        crypto,
      ),
    )
    Error(_) -> #(<<>>, Failed("encode hello failed"))
  }
}

/// Start the handshake as initiator using the default stub provider.
///
/// **Deprecated**: Use `initiator_start(local, crypto)` instead.
/// This function exists for backward compatibility.
///
/// ## Warning
///
/// Uses `provider_stub` which provides NO actual security.
@deprecated("Use initiator_start(local, crypto) with explicit provider")
pub fn initiator_start_with_stub(
  local: handshake.Hello,
) -> #(BitArray, HandshakeState) {
  initiator_start(local, stub_crypto_provider())
}

/// Initialize the responder state.
/// 
/// Creates a fresh responder state ready to receive a Hello message
/// from the initiator.
///
/// ## Arguments
///
/// - `crypto` - Crypto provider to use for key exchange
///
/// ## Example
///
/// ```gleam
/// let crypto = stub_crypto_provider()  // Use real provider in production!
/// let state = responder_init(crypto)
/// ```
pub fn responder_init(crypto: CryptoProvider) -> HandshakeState {
  ResponderState({ crypto.init }(), None, None, None, 0, None, crypto)
}

/// Initialize the responder state using the default stub provider.
///
/// **Deprecated**: Use `responder_init(crypto)` instead.
/// This function exists for backward compatibility.
///
/// ## Warning
///
/// Uses `provider_stub` which provides NO actual security.
@deprecated("Use responder_init(crypto) with explicit provider")
pub fn responder_init_with_stub() -> HandshakeState {
  responder_init(stub_crypto_provider())
}

/// Handle an incoming message as the responder.
/// 
/// Processes Hello, Accept, or KeyExchange messages and returns
/// the appropriate Outcome with optional outgoing message and new state.
pub fn responder_handle_message(
  state: HandshakeState,
  data: BitArray,
) -> Result(Outcome, String) {
  case state {
    ResponderState(
      pstate,
      negotiated,
      secure,
      _waiting,
      _retries,
      _pending,
      crypto,
    ) -> {
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
                  crypto,
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
                { crypto.start_key_exchange }(pstate, <<>>)
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
                      crypto,
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
                    { crypto.handle_key_exchange }(pstate, k.payload)
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
                                  crypto,
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
                              crypto,
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

/// Handle an incoming message as the initiator.
/// 
/// Processes Capabilities, Reject, or KeyExchange messages and returns
/// the appropriate Outcome with optional outgoing message and new state.
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
      crypto,
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
                    { crypto.start_key_exchange }(pstate, <<>>)
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
                          crypto,
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
                { crypto.handle_key_exchange }(pstate, k.payload)
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
                              crypto,
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
                          crypto,
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

/// Handle a timeout during handshake.
/// 
/// Either retransmits the pending message if retries remain,
/// or transitions to Failed state if max retries exceeded.
pub fn handshake_on_timeout(state: HandshakeState) -> Result(Outcome, String) {
  let max_accept_retries = 3
  let max_ke_retries = 3
  case state {
    InitiatorState(
      pstate,
      local,
      negotiated,
      secure,
      waiting,
      retries,
      pending,
      crypto,
    ) -> {
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
                          crypto,
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
                          crypto,
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
    ResponderState(
      pstate,
      negotiated,
      secure,
      waiting,
      retries,
      pending,
      crypto,
    ) -> {
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
                          crypto,
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
                          crypto,
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
