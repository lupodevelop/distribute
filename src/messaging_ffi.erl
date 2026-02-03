%% Messaging FFI for Gleam distribute library
-module(messaging_ffi).
-export([send_global/2, send_binary_global/2, is_ok_atom/1, is_not_found/1, get_error_reason/1]).

%% Import shared utility for safe atom conversion
-import(distribute_ffi_utils, [to_atom_safe/1]).

send_global(Name, Msg) ->
    case to_atom_safe(Name) of
        {ok, A} ->
            case global:whereis_name(A) of
                undefined -> {error, not_found};
                Pid when is_pid(Pid) ->
                    try
                        Pid ! Msg,
                        ok
                    catch
                        _:_ -> {error, <<"send_failed">>}
                    end
            end;
        Error -> Error
    end.

%% Send binary message to a globally registered name
%% This is used by typed messaging to send envelope-wrapped payloads
send_binary_global(Name, BinaryMsg) when is_binary(BinaryMsg) ->
    case to_atom_safe(Name) of
        {ok, A} ->
            case global:whereis_name(A) of
                undefined -> {error, not_found};
                Pid when is_pid(Pid) ->
                    %% Validate binary size against configurable limit
                    MaxSize = settings_ffi:get_max_message_size(),
                    case MaxSize > 0 andalso byte_size(BinaryMsg) > MaxSize of
                        true -> {error, <<"message_too_large">>};
                        false ->
                            try
                                %% Wrap in {nil, Msg} to match Subject(Pid, Nil)
                                Pid ! {nil, BinaryMsg},
                                ok
                            catch
                                error:badarg -> {error, <<"invalid_message">>};
                                _:Reason -> {error, iolist_to_binary(io_lib:format("send_failed: ~p", [Reason]))}
                            end
                    end
            end;
        Error -> Error
    end;
send_binary_global(_Name, _Msg) ->
    {error, <<"invalid_binary">>}.

%% Helpers for Gleam FFI
is_ok_atom(ok) -> true;
is_ok_atom(_) -> false.

is_not_found({error, not_found}) -> true;
is_not_found(_) -> false.

get_error_reason({error, Reason}) when is_binary(Reason) -> Reason;
get_error_reason({error, Reason}) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
get_error_reason({error, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_error_reason(_) -> <<"unknown_error">>.
