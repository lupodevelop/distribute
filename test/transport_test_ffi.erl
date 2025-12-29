-module(transport_test_ffi).
-export([register_process/2, unregister_process/1, receive_message/1]).

%% Register a process with an atom name (for testing)
register_process(Pid, NameBin) when is_pid(Pid), is_binary(NameBin) ->
    Name = binary_to_atom(NameBin, utf8),
    try
        register(Name, Pid),
        nil
    catch
        error:badarg -> 
            %% Name already registered, unregister and try again
            case whereis(Name) of
                OldPid when is_pid(OldPid) ->
                    unregister(Name),
                    register(Name, Pid),
                    nil;
                _ -> 
                    nil
            end
    end.

%% Unregister a process by name
unregister_process(NameBin) when is_binary(NameBin) ->
    try
        Name = binary_to_existing_atom(NameBin, utf8),
        unregister(Name),
        nil
    catch
        _:_ -> nil
    end.

%% Receive a transport message {nil, Payload}
receive_message(Timeout) ->
    receive
        {nil, Payload} when is_binary(Payload) ->
            {ok, Payload}
    after Timeout ->
        {error, nil}
    end.
