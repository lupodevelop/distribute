%% Settings FFI for distribute
-module(settings_ffi).
-export([set_allow_atom_creation/1, get_allow_atom_creation/0, set_use_crypto_ids/1, get_use_crypto_ids/0]).

set_allow_atom_creation(true) -> persistent_term:put(distribute_allow_atom_creation, true), ok;
set_allow_atom_creation(false) -> persistent_term:put(distribute_allow_atom_creation, false), ok;
set_allow_atom_creation(_) -> {error, badarg}.

get_allow_atom_creation() -> persistent_term:get(distribute_allow_atom_creation, false).

set_use_crypto_ids(true) -> persistent_term:put(distribute_use_crypto_ids, true), ok;
set_use_crypto_ids(false) -> persistent_term:put(distribute_use_crypto_ids, false), ok;
set_use_crypto_ids(_) -> {error, badarg}.

get_use_crypto_ids() -> persistent_term:get(distribute_use_crypto_ids, false).
