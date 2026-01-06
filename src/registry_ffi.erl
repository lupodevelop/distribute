%% Registry FFI for Gleam distribute library
-module(registry_ffi).
-export([register/2, unregister/1, whereis/1,
         is_ok_atom/1, is_already_registered/1, get_error_reason/1,
         is_pid/1, dynamic_to_pid/1, make_subject/2,
         store_subject/2, get_subject/1, remove_subject/1,
         is_ok_tuple/1, extract_subject/1, millisecond_atom/0]).

make_subject(Pid, Tag) -> {subject, Pid, Tag}.

register(Name, Pid) ->
    case erlang:is_pid(Pid) of
        true ->
            case to_atom_safe(Name) of
                {ok, A} ->
                    case global:register_name(A, Pid) of
                        yes -> ok;
                        no -> {error, already_registered}
                    end;
                Error -> Error
            end;
        false ->
            {error, <<"not_a_pid">>}
    end.

unregister(Name) ->
    case to_atom_safe(Name) of
        {ok, A} -> 
            global:unregister_name(A),
            ok;
        Error -> Error
    end.

whereis(Name) ->
    case to_atom_safe(Name) of
        {ok, A} ->
            case global:whereis_name(A) of
                undefined -> not_found;
                Pid -> 
                    case erlang:is_pid(Pid) of
                        true -> Pid;
                        false -> not_found
                    end
            end;
        _Error -> not_found
    end.

%% Helpers for Gleam FFI
is_ok_atom(ok) -> true;
is_ok_atom(_) -> false.

is_already_registered({error, already_registered}) -> true;
is_already_registered(_) -> false.

get_error_reason({error, Reason}) when is_binary(Reason) -> Reason;
get_error_reason({error, Reason}) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
get_error_reason({error, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_error_reason(_) -> <<"unknown_error">>.

is_pid(Value) -> erlang:is_pid(Value).

dynamic_to_pid(Pid) -> Pid.

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

%% Store a complete Subject (with tag) by name using persistent_term
%% This is useful for OTP actors where the tag must be preserved
store_subject(Name, Subject) when is_binary(Name) ->
    Key = {distribute_registry_subject, Name},
    persistent_term:put(Key, Subject),
    ok.

%% Get a stored Subject by name
get_subject(Name) when is_binary(Name) ->
    Key = {distribute_registry_subject, Name},
    try persistent_term:get(Key) of
        Subject -> {ok, Subject}
    catch
        error:badarg -> {error, not_found}
    end.

%% Remove a stored Subject by name
remove_subject(Name) when is_binary(Name) ->
    Key = {distribute_registry_subject, Name},
    try 
        persistent_term:erase(Key),
        ok
    catch
        error:badarg -> ok
    end.

%% Check if value is {ok, _}
is_ok_tuple({ok, _}) -> true;
is_ok_tuple(_) -> false.

%% Extract Subject from {ok, Subject}
extract_subject({ok, Subject}) -> Subject.

%% Return the 'millisecond' atom for erlang:system_time/1
millisecond_atom() -> millisecond.
