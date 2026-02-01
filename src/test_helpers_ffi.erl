%% Test helper FFI for Gleam tests
-module(test_helpers_ffi).
-export([binary_to_existing_atom_exists/1, get_allow_atom_creation/0, inspect_to_atom/1, inspect_to_atom_status/1,
         to_atom_safe_with_list/1, to_atom_safe_with_atom/1]).

binary_to_existing_atom_exists(Bin) when is_binary(Bin) ->
    case catch binary_to_existing_atom(Bin, utf8) of
        {'EXIT', _} -> false;
        _Atom -> true
    end;
binary_to_existing_atom_exists(_) -> false.

get_allow_atom_creation() -> persistent_term:get(distribute_allow_atom_creation, false).

inspect_to_atom(Bin) when is_binary(Bin) ->
    case catch binary_to_existing_atom(Bin, utf8) of
        {'EXIT', _} -> case persistent_term:get(distribute_allow_atom_creation, false) of
                           true -> {ok, {created, binary_to_atom(Bin, utf8)}};
                           false -> {error, atom_not_existing}
                       end;
        Atom -> {ok, {existing, Atom}}
    end;
inspect_to_atom(List) when is_list(List) -> inspect_to_atom(list_to_binary(List));
inspect_to_atom(Atom) when is_atom(Atom) -> {ok, {existing, Atom}};
inspect_to_atom(_) -> {error, badarg}.

inspect_to_atom_status(Bin) when is_binary(Bin) ->
    Allow = persistent_term:get(distribute_allow_atom_creation, false),
    case catch binary_to_existing_atom(Bin, utf8) of
        {'EXIT', _} -> case Allow of
                           true -> <<"created">>;
                           false -> <<"error">>
                       end;
        _Atom -> <<"existing">>
    end;
inspect_to_atom_status(List) when is_list(List) -> inspect_to_atom_status(list_to_binary(List));
inspect_to_atom_status(Atom) when is_atom(Atom) -> <<"existing">>;
inspect_to_atom_status(_) -> <<"error">>.

%% Helper to test to_atom_safe with list input (charlist)
to_atom_safe_with_list(List) when is_list(List) ->
    distribute_ffi_utils:to_atom_safe(List).

%% Helper to test to_atom_safe with atom input (passthrough)
to_atom_safe_with_atom(Atom) when is_atom(Atom) ->
    distribute_ffi_utils:to_atom_safe(Atom).
