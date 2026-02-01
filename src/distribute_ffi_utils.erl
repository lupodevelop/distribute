%% @doc Common utility functions for distribute FFI modules.
%%
%% This module provides shared functionality used across multiple FFI modules
%% to avoid code duplication and ensure consistent behavior.
%%
%% ## Key Functions
%%
%% - `to_atom_safe/1`: Safely convert binaries/lists to atoms respecting
%%   the `allow_atom_creation` setting.
%%
%% ## Atom Creation Policy
%%
%% Atom creation is controlled by the `allow_atom_creation` flag in settings.
%% When disabled (default), only existing atoms can be looked up, preventing
%% atom table exhaustion attacks from untrusted input.
%%
%% When enabled, new atoms can be created. Use this only when:
%% - The input is from a trusted source (e.g., configuration)
%% - You've validated the input won't cause unbounded atom creation
%%
%% ## Example Usage
%%
%% ```erlang
%% -import(distribute_ffi_utils, [to_atom_safe/1]).
%%
%% register_name(Name) ->
%%     case to_atom_safe(Name) of
%%         {ok, Atom} -> {ok, register(Atom, self())};
%%         {error, Reason} -> {error, Reason}
%%     end.
%% ```

-module(distribute_ffi_utils).
-export([
    to_atom_safe/1,
    get_allow_atom_creation/0
]).

%% @doc Safely convert a binary, list, or atom to an atom.
%%
%% Respects the `allow_atom_creation` setting from settings_ffi.
%% When allow_atom_creation is false (default), uses binary_to_existing_atom
%% which only succeeds if the atom already exists.
%%
%% When allow_atom_creation is true, uses binary_to_atom which can create
%% new atoms (use with caution to avoid atom table exhaustion).
%%
%% ## Arguments
%%
%% - `Input` - A binary, list (string), or atom
%%
%% ## Returns
%%
%% - `{ok, Atom}` - Successfully converted to atom
%% - `{error, Reason}` - Conversion failed
%%   - `<<"badarg">>` - Invalid input type
%%   - `<<"atom_not_found">>` - Atom doesn't exist (when creation disabled)
%%
%% ## Examples
%%
%% ```erlang
%% {ok, foo} = to_atom_safe(<<"foo">>).  % Existing atom
%% {ok, bar} = to_atom_safe("bar").       % From string
%% {ok, baz} = to_atom_safe(baz).         % Already an atom
%% {error, _} = to_atom_safe(123).        % Invalid type
%% ```
-spec to_atom_safe(Input :: binary() | list() | atom()) ->
    {ok, atom()} | {error, binary()}.
to_atom_safe(Bin) when is_list(Bin) -> 
    to_atom_safe(list_to_binary(Bin));
to_atom_safe(Bin) when is_binary(Bin) ->
    AllowCreation = get_allow_atom_creation(),
    try
        case AllowCreation of
            true -> {ok, binary_to_atom(Bin, utf8)};
            false -> {ok, binary_to_existing_atom(Bin, utf8)}
        end
    catch
        _:_ -> {error, <<"atom_not_found">>}
    end;
to_atom_safe(Atom) when is_atom(Atom) -> 
    {ok, Atom};
to_atom_safe(_) -> 
    {error, <<"badarg">>}.

%% @doc Get the current allow_atom_creation setting.
%%
%% Returns the value from settings_ffi if available, otherwise defaults to false.
%% This is a safe default that prevents atom table exhaustion.
%%
%% ## Returns
%%
%% - `true` - Atom creation is allowed
%% - `false` - Only existing atoms can be looked up (default)
-spec get_allow_atom_creation() -> boolean().
get_allow_atom_creation() ->
    try
        settings_ffi:get_allow_atom_creation()
    catch
        _:_ -> false  % Default to safe behavior
    end.
