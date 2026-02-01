%% Settings FFI for distribute
-module(settings_ffi).
-export([set_allow_atom_creation/1, get_allow_atom_creation/0, 
         set_use_crypto_ids/1, get_use_crypto_ids/0,
         set_max_message_size/1, get_max_message_size/0]).

%% Default max message size: 10MB
-define(DEFAULT_MAX_MESSAGE_SIZE, 10485760).

set_allow_atom_creation(true) -> persistent_term:put(distribute_allow_atom_creation, true), ok;
set_allow_atom_creation(false) -> persistent_term:put(distribute_allow_atom_creation, false), ok;
set_allow_atom_creation(_) -> {error, badarg}.

get_allow_atom_creation() -> persistent_term:get(distribute_allow_atom_creation, false).

set_use_crypto_ids(true) -> persistent_term:put(distribute_use_crypto_ids, true), ok;
set_use_crypto_ids(false) -> persistent_term:put(distribute_use_crypto_ids, false), ok;
set_use_crypto_ids(_) -> {error, badarg}.

get_use_crypto_ids() -> persistent_term:get(distribute_use_crypto_ids, false).

%% Set maximum allowed message size in bytes.
%% Must be a positive integer. Use 0 to disable size checking (not recommended).
set_max_message_size(Size) when is_integer(Size), Size >= 0 ->
    persistent_term:put(distribute_max_message_size, Size),
    ok;
set_max_message_size(_) -> 
    {error, badarg}.

%% Get maximum allowed message size in bytes.
%% Defaults to 10MB (10485760 bytes) if not set.
get_max_message_size() -> 
    persistent_term:get(distribute_max_message_size, ?DEFAULT_MAX_MESSAGE_SIZE).
