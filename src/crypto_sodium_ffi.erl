%% @doc FFI functions for sodium crypto adapter.
%%
%% Provides X25519 key exchange and ChaCha20-Poly1305 AEAD encryption
%% using Erlang's built-in crypto module (which uses OpenSSL/LibreSSL).
%%
%% This module uses standard Erlang crypto functions that are available
%% in OTP 22+ without requiring external libsodium installation.
%%
%% ## Algorithms
%%
%% - Key exchange: X25519 (Curve25519 ECDH)
%% - AEAD: ChaCha20-Poly1305 (RFC 8439, 12-byte nonce)
%% - KDF: HKDF-SHA256
%% - Random: crypto:strong_rand_bytes/1
%%
%% ## Nonce Policy
%%
%% This implementation uses **12-byte nonces** as specified in RFC 8439.
%% XChaCha20-Poly1305 (24-byte nonces) is NOT supported.
%% Nonces MUST be unique per key - we generate them randomly which is safe
%% for reasonable message volumes (birthday bound ~2^48 for 12-byte nonces).
%%
%% ## Security Notes
%%
%% - Never log key material or secrets
%% - Nonces are generated randomly (12 bytes, RFC 8439 standard)
%% - Key IDs are unpredictable (timestamp + random)

-module(crypto_sodium_ffi).
-export([
    gen_keypair/0,
    scalarmult/2,
    aead_encrypt/4,
    aead_decrypt/4,
    random_bytes/1,
    hkdf/4,
    generate_key_id/0,
    generate_nonce/0,
    system_time_ms/0,
    wrap_key_material/1,
    unwrap_key_material/1,
    wrap_handshake_data/1
]).

%% =============================================================================
%% Key Generation
%% =============================================================================

%% @doc Generate an X25519 keypair.
%% Returns {ok, {PublicKey, SecretKey}} where both are 32-byte binaries.
-spec gen_keypair() -> {ok, {binary(), binary()}}.
gen_keypair() ->
    {Pub, Priv} = crypto:generate_key(ecdh, x25519),
    {ok, {Pub, Priv}}.

%% @doc Compute shared secret using X25519 scalar multiplication.
%% Takes peer's public key and our private key.
%% Returns {ok, SharedSecret} (32 bytes) or {error, Reason}.
-spec scalarmult(binary(), binary()) -> {ok, binary()} | {error, term()}.
scalarmult(PeerPubKey, OurPrivKey) when byte_size(PeerPubKey) =:= 32,
                                        byte_size(OurPrivKey) =:= 32 ->
    try
        SharedSecret = crypto:compute_key(ecdh, PeerPubKey, OurPrivKey, x25519),
        {ok, SharedSecret}
    catch
        _:Reason ->
            {error, {scalarmult_failed, Reason}}
    end;
scalarmult(_, _) ->
    {error, invalid_key_size}.

%% =============================================================================
%% AEAD Encryption (ChaCha20-Poly1305)
%% =============================================================================

%% @doc Encrypt plaintext using ChaCha20-Poly1305 AEAD (RFC 8439).
%% Key must be 32 bytes, Nonce must be 12 bytes.
%% Returns {ok, Ciphertext} where Ciphertext includes the 16-byte auth tag.
%%
%% NOTE: Only 12-byte nonces are supported (standard ChaCha20-Poly1305).
%% XChaCha20-Poly1305 (24-byte nonces) is NOT implemented.
-spec aead_encrypt(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
aead_encrypt(Key, Nonce, AAD, Plaintext) when byte_size(Key) =:= 32,
                                               byte_size(Nonce) =:= 12 ->
    try
        {Ciphertext, Tag} = crypto:crypto_one_time_aead(
            chacha20_poly1305,
            Key,
            Nonce,
            Plaintext,
            AAD,
            true  %% encrypt
        ),
        %% Append tag to ciphertext
        {ok, <<Ciphertext/binary, Tag/binary>>}
    catch
        _:Reason ->
            {error, {encrypt_failed, Reason}}
    end;
aead_encrypt(Key, _Nonce, _AAD, _Plaintext) when byte_size(Key) =/= 32 ->
    {error, {invalid_key_size, byte_size(Key), expected_32}};
aead_encrypt(_Key, Nonce, _AAD, _Plaintext) when byte_size(Nonce) =/= 12 ->
    {error, {invalid_nonce_size, byte_size(Nonce), expected_12}}.

%% @doc Decrypt ciphertext using ChaCha20-Poly1305 AEAD (RFC 8439).
%% Key must be 32 bytes, Nonce must be 12 bytes.
%% Ciphertext includes the 16-byte auth tag at the end.
%% Returns {ok, Plaintext} or {error, decrypt_failed}.
%%
%% NOTE: Only 12-byte nonces are supported (standard ChaCha20-Poly1305).
%% XChaCha20-Poly1305 (24-byte nonces) is NOT implemented.
-spec aead_decrypt(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
aead_decrypt(Key, Nonce, AAD, CiphertextWithTag) when byte_size(Key) =:= 32,
                                                       byte_size(Nonce) =:= 12 ->
    try
        %% Extract tag (last 16 bytes)
        CipherLen = byte_size(CiphertextWithTag) - 16,
        case CipherLen >= 0 of
            true ->
                <<Ciphertext:CipherLen/binary, Tag:16/binary>> = CiphertextWithTag,
                case crypto:crypto_one_time_aead(
                    chacha20_poly1305,
                    Key,
                    Nonce,
                    Ciphertext,
                    AAD,
                    Tag,
                    false  %% decrypt
                ) of
                    Plaintext when is_binary(Plaintext) ->
                        {ok, Plaintext};
                    error ->
                        {error, decrypt_failed}
                end;
            false ->
                {error, ciphertext_too_short}
        end
    catch
        _:Reason ->
            {error, {decrypt_failed, Reason}}
    end;
aead_decrypt(Key, _Nonce, _AAD, _CiphertextWithTag) when byte_size(Key) =/= 32 ->
    {error, {invalid_key_size, byte_size(Key), expected_32}};
aead_decrypt(_Key, Nonce, _AAD, _CiphertextWithTag) when byte_size(Nonce) =/= 12 ->
    {error, {invalid_nonce_size, byte_size(Nonce), expected_12}}.

%% =============================================================================
%% Key Derivation (HKDF-SHA256)
%% =============================================================================

%% @doc Derive key material using HKDF-SHA256.
%% Salt and IKM are input key material, Info is context, Len is output length.
-spec hkdf(binary(), binary(), binary(), integer()) -> {ok, binary()}.
hkdf(Salt, IKM, Info, Len) when is_binary(Salt), is_binary(IKM),
                                 is_binary(Info), is_integer(Len), Len > 0 ->
    DerivedKey = hkdf_extract_expand(IKM, Salt, Info, Len),
    {ok, DerivedKey}.

%% Internal HKDF implementation
hkdf_extract_expand(IKM, Salt, Info, Len) ->
    %% Extract
    PRK = crypto:mac(hmac, sha256, Salt, IKM),
    %% Expand
    hkdf_expand(PRK, Info, Len, <<>>, 1).

hkdf_expand(_PRK, _Info, Len, Acc, _Counter) when byte_size(Acc) >= Len ->
    <<Result:Len/binary, _/binary>> = Acc,
    Result;
hkdf_expand(PRK, Info, Len, Acc, Counter) when Counter =< 255 ->
    T = crypto:mac(hmac, sha256, PRK, <<Acc/binary, Info/binary, Counter:8>>),
    hkdf_expand(PRK, Info, Len, <<Acc/binary, T/binary>>, Counter + 1).

%% =============================================================================
%% Random
%% =============================================================================

%% @doc Generate N random bytes using cryptographically secure RNG.
-spec random_bytes(integer()) -> binary().
random_bytes(N) when is_integer(N), N > 0 ->
    crypto:strong_rand_bytes(N).

%% @doc Generate a 12-byte nonce for ChaCha20-Poly1305 (RFC 8439).
%%
%% Uses cryptographically secure random bytes. For typical usage volumes,
%% random nonces are safe (birthday bound ~2^48 messages per key).
%% If you need to encrypt more than ~2^48 messages with the same key,
%% consider using a counter-based nonce scheme instead.
-spec generate_nonce() -> binary().
generate_nonce() ->
    crypto:strong_rand_bytes(12).

%% =============================================================================
%% Utilities
%% =============================================================================

%% @doc Generate a unique key ID for session keys.
%% Uses node name, timestamp, and random bytes.
-spec generate_key_id() -> binary().
generate_key_id() ->
    Node = atom_to_binary(node(), utf8),
    Timestamp = integer_to_binary(erlang:system_time(nanosecond)),
    Random = base64:encode(crypto:strong_rand_bytes(8)),
    <<Node/binary, "_", Timestamp/binary, "_", Random/binary>>.

%% @doc Get current system time in milliseconds.
-spec system_time_ms() -> integer().
system_time_ms() ->
    erlang:system_time(millisecond).

%% @doc Wrap key material for opaque storage.
-spec wrap_key_material(term()) -> term().
wrap_key_material({key_material, AeadKey, MasterSecret, Counter}) ->
    {sodium_key_material, AeadKey, MasterSecret, Counter};
wrap_key_material(KeyMaterial) ->
    %% Handle Gleam record format
    case KeyMaterial of
        {key_material, AeadKey, MasterSecret, Counter} ->
            {sodium_key_material, AeadKey, MasterSecret, Counter};
        _ ->
            {sodium_key_material, KeyMaterial, <<>>, 0}
    end.

%% @doc Unwrap key material from opaque storage.
-spec unwrap_key_material(term()) -> {ok, term()} | {error, nil}.
unwrap_key_material({sodium_key_material, AeadKey, MasterSecret, Counter}) ->
    {ok, {AeadKey, MasterSecret, Counter}};
unwrap_key_material(_) ->
    {error, nil}.

%% @doc Wrap handshake data for opaque storage.
-spec wrap_handshake_data(term()) -> term().
wrap_handshake_data(Pending) ->
    {sodium_handshake_data, Pending}.
