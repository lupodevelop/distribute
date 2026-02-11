-module(registry_ffi).
-export([register/2, unregister/1, whereis/1,
         is_ok_atom/1, is_already_registered/1, get_error_reason/1,
         is_pid/1, dynamic_to_pid/1]).

%% Uses binary names directly with global:register_name/2.
%% No atom conversion - no atom table exhaustion risk.
%% global:register_name accepts any Erlang term as a name.

register(Name, Pid) ->
    case erlang:is_pid(Pid) of
        true ->
            case global:register_name(Name, Pid) of
                yes -> ok;
                no -> {error, already_registered}
            end;
        false ->
            {error, <<"not_a_pid">>}
    end.

unregister(Name) ->
    global:unregister_name(Name),
    ok.

whereis(Name) ->
    case global:whereis_name(Name) of
        undefined -> not_found;
        Pid when erlang:is_pid(Pid) -> Pid;
        _ -> not_found
    end.

%% Helpers for Gleam FFI result classification (delegate to shared utils)
is_ok_atom(V) -> distribute_ffi_utils:is_ok_atom(V).
get_error_reason(V) -> distribute_ffi_utils:get_error_reason(V).

is_already_registered({error, already_registered}) -> true;
is_already_registered(_) -> false.

is_pid(Value) -> erlang:is_pid(Value).

dynamic_to_pid(Pid) -> Pid.
