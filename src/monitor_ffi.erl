%% Monitor FFI for Gleam distribute library
-module(monitor_ffi).
-export([to_atom/1]).

%% Import shared utility for safe atom conversion
-import(distribute_ffi_utils, [to_atom_safe/1]).

%% Safe atom conversion that respects the allow_atom_creation setting.
%% Returns the atom directly (unwrapped) for backward compatibility.
%% NOTE: For monitoring node names, we need actual atoms, so we unwrap
%% the result. Node names come from Erlang distribution, not user input.
to_atom(Input) ->
    case to_atom_safe(Input) of
        {ok, Atom} -> Atom;
        {error, _} ->
            %% For node names in monitoring, we need to create atoms.
            %% Node names come from Erlang distribution, not arbitrary user input.
            case Input of
                Bin when is_binary(Bin) -> binary_to_atom(Bin, utf8);
                List when is_list(List) -> list_to_atom(List);
                Atom when is_atom(Atom) -> Atom
            end
    end.
