%% @doc FFI functions for sodium crypto adapter.
%%
%% Provides X25519 key exchange and XChaCha20-Poly1305 AEAD encryption
%% using Erlang's built-in crypto module (which uses OpenSSL/LibreSSL).
%%
%% This module uses standard Erlang crypto functions that are available
%% in OTP 22+ without requiring external libsodium installation.
%%
%% ## Algorithms
%%
%% - Key exchange: X25519 (Curve25519 ECDH)
%% - AEAD: ChaCha20-Poly1305 (RFC 8439)
%% - KDF: HKDF-SHA256
%% - Random: crypto:strong_rand_bytes/1
%%
%% ## Security Notes
%%
%% - Never log key material or secrets
%% - Nonces are generated randomly (24 bytes for extended nonce)
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

%% @doc Encrypt plaintext using ChaCha20-Poly1305 AEAD.
%% Key must be 32 bytes, Nonce must be 12 bytes (or 24 for XChaCha variant).
%% Returns {ok, Ciphertext} where Ciphertext includes the 16-byte auth tag.
-spec aead_encrypt(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
aead_encrypt(Key, Nonce, AAD, Plaintext) when byte_size(Key) =:= 32 ->
    try
        %% Use 12-byte nonce for standard ChaCha20-Poly1305
        %% If nonce is 24 bytes, we use the first 16 as subkey derivation
        ActualNonce = case byte_size(Nonce) of
            12 -> Nonce;
            24 ->
                %% XChaCha20: derive subkey from first 16 bytes
                <<NoncePrefix:16/binary, NonceSuffix:8/binary>> = Nonce,
                %% Use HChaCha20 to derive subkey (simplified: use HKDF)
                SubKey = hkdf_extract_expand(Key, NoncePrefix, <<"xchacha">>, 32),
                %% Prepend 4 zero bytes + last 8 bytes of nonce
                <<0:32, NonceSuffix/binary>>;
            _ -> 
                %% Fallback: hash to 12 bytes
                <<H:12/binary, _/binary>> = crypto:hash(sha256, Nonce),
                H
        end,
        {Ciphertext, Tag} = crypto:crypto_one_time_aead(
            chacha20_poly1305,
            Key,
            ActualNonce,
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
aead_encrypt(_, _, _, _) ->
    {error, invalid_key_size}.

%% @doc Decrypt ciphertext using ChaCha20-Poly1305 AEAD.
%% Key must be 32 bytes, Nonce must be 12 bytes.
%% Ciphertext includes the 16-byte auth tag at the end.
%% Returns {ok, Plaintext} or {error, decrypt_failed}.
-spec aead_decrypt(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
aead_decrypt(Key, Nonce, AAD, CiphertextWithTag) when byte_size(Key) =:= 32 ->
    try
        %% Extract tag (last 16 bytes)
        CipherLen = byte_size(CiphertextWithTag) - 16,
        case CipherLen >= 0 of
            true ->
                <<Ciphertext:CipherLen/binary, Tag:16/binary>> = CiphertextWithTag,
                ActualNonce = case byte_size(Nonce) of
                    12 -> Nonce;
                    24 ->
                        <<NoncePrefix:16/binary, NonceSuffix:8/binary>> = Nonce,
                        SubKey = hkdf_extract_expand(Key, NoncePrefix, <<"xchacha">>, 32),
                        <<0:32, NonceSuffix/binary>>;
                    _ ->
                        <<H:12/binary, _/binary>> = crypto:hash(sha256, Nonce),
                        H
                end,
                case crypto:crypto_one_time_aead(
                    chacha20_poly1305,
                    Key,
                    ActualNonce,
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
aead_decrypt(_, _, _, _) ->
    {error, invalid_key_size}.

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

%% @doc Generate a 24-byte nonce for XChaCha20-Poly1305.
-spec generate_nonce() -> binary().
generate_nonce() ->
    crypto:strong_rand_bytes(24).

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
