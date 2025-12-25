%% Messaging FFI for Gleam distribute library
-module(messaging_ffi).
-export([send_global/2, is_ok_atom/1, is_not_found/1, get_error_reason/1]).

send_global(Name, Msg) ->
    case to_atom_safe(Name) of
        {ok, A} ->
            case global:whereis_name(A) of
                undefined -> {error, not_found};
                Pid when is_pid(Pid) ->
                    Pid ! Msg,
                    ok
            end;
        Error -> Error
    end.

%% Helpers for Gleam FFI
is_ok_atom(ok) -> true;
is_ok_atom(_) -> false.

is_not_found({error, not_found}) -> true;
is_not_found(_) -> false.

get_error_reason({error, Reason}) when is_binary(Reason) -> Reason;
get_error_reason({error, Reason}) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
get_error_reason({error, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_error_reason(_) -> <<"unknown_error">>.

%% Internal helpers
to_atom_safe(Bin) when is_list(Bin) -> to_atom_safe(list_to_binary(Bin));
to_atom_safe(Bin) when is_binary(Bin) -> 
    Allow = persistent_term:get(distribute_allow_atom_creation, false),
    case catch binary_to_existing_atom(Bin, utf8) of
        {'EXIT', _} -> case Allow of
                           true -> {ok, binary_to_atom(Bin, utf8)};
                           false -> {error, <<"atom_not_existing">>} 
                       end;
        Atom -> {ok, Atom}
    end;
to_atom_safe(Atom) when is_atom(Atom) -> {ok, Atom};
to_atom_safe(_) -> {error, <<"badarg">>}.
