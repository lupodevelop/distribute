%% Monitor FFI for Gleam distribute library
-module(monitor_ffi).
-export([to_atom/1]).

%% Import shared utility for safe atom conversion
-import(distribute_ffi_utils, [to_atom_safe/1]).

%% Safe atom conversion that respects the allow_atom_creation setting.
%% Returns the atom directly (unwrapped) for backward compatibility.
%% NOTE: For monitoring node names, we need actual atoms, so we unwrap
%% the result. Node names come from Erlang distribution and are already
%% atoms in the BEAM, so binary_to_existing_atom should always succeed
%% for legitimate node names.
to_atom(Input) when is_atom(Input) -> Input;
to_atom(Input) ->
    case to_atom_safe(Input) of
        {ok, Atom} -> Atom;
        {error, _} ->
            %% For node names from distribution, try existing atom only.
            %% If it doesn't exist, the node name is invalid.
            try
                case Input of
                    Bin when is_binary(Bin) -> binary_to_existing_atom(Bin, utf8);
                    List when is_list(List) -> list_to_existing_atom(List)
                end
            catch
                _:_ -> error({invalid_node_name, Input})
            end
    end.
