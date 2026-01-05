%% FFI for BEAM transport adapter
-module(beam_adapter_ffi).
-export([wrap_subject/1, unwrap_subject/1, to_dynamic/1, send_to_registered/2, register_process_by_name/2]).

%% Wrap a Subject into a dynamic value (no-op in Erlang)
wrap_subject(Subject) -> Subject.

%% Unwrap a dynamic value into a Subject
unwrap_subject(Dynamic) -> {ok, Dynamic}.

%% Convert Int to Dynamic (no-op in Erlang)
to_dynamic(Value) -> Value.

%% Send binary payload to a registered process (local or remote)
%% peer format: "registered_name@node" or "registered_name" (local)
send_to_registered(Peer, Payload) when is_binary(Peer), is_binary(Payload) ->
    case byte_size(Payload) > 10485760 of
        true ->
            {error, <<"message_too_large">>};
        false ->
            case parse_peer(Peer) of
                {local, Name} ->
                    send_to_local_registered(Name, Payload);
                {remote, Name, Node} ->
                    send_to_remote_registered(Name, Node, Payload);
                error ->
                    {error, <<"invalid_peer_format">>}
            end
    end;
send_to_registered(_Peer, _Payload) ->
    {error, <<"invalid_arguments">>}.

%% Parse peer string into {local, Name} or {remote, Name, Node}
parse_peer(Peer) ->
    case binary:split(Peer, <<"@">>) of
        [Name] -> 
            {local, Name};
        [Name, NodeBin] ->
            try
                Node = binary_to_atom(NodeBin, utf8),
                {remote, Name, Node}
            catch
                _:_ -> error
            end;
        _ -> 
            error
    end.

%% Send to locally registered process
send_to_local_registered(Name, Payload) ->
    case to_atom_safe(Name) of
        {ok, AtomName} ->
            %% Try local first, then global
            Pid = case whereis(AtomName) of
                undefined -> global:whereis_name(AtomName);
                P -> P
            end,
            
            case Pid of
                undefined -> 
                    {error, <<"not_found">>};
                _ when is_pid(Pid) ->
                    try
                        Pid ! {nil, Payload},
                        {ok, nil}
                    catch
                        _:_ -> {error, <<"send_failed">>}
                    end
            end;
        Error -> 
            Error
    end.

%% Send to remotely registered process
send_to_remote_registered(Name, Node, Payload) ->
    case to_atom_safe(Name) of
        {ok, AtomName} ->
            %% Use {registered_name, Node} tuple for remote send
            RemoteRef = {AtomName, Node},
            try
                %% Send message
                RemoteRef ! {nil, Payload},
                {ok, nil}
            catch
                error:badarg -> 
                    {error, <<"connection_closed">>};
                _:Reason -> 
                    ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
                    {error, ReasonBin}
            end;
        Error -> 
            Error
    end.

%% Safe atom conversion (prevent atom table exhaustion)
to_atom_safe(Binary) when is_binary(Binary) ->
    try
        %% Only convert if atom already exists
        Atom = binary_to_existing_atom(Binary, utf8),
        {ok, Atom}
    catch
        error:badarg ->
            %% Atom doesn't exist - for safety, reject
            %% In production, might want to allow via whitelist
            {error, <<"unknown_registered_name">>}
    end;
to_atom_safe(_) ->
    {error, <<"invalid_name">>}.

%% Look up a registered process by name (string)
whereis_name(Name) when is_binary(Name) ->
    try binary_to_existing_atom(Name, utf8) of
        Atom ->
            case erlang:whereis(Atom) of
                undefined -> {error, nil};
                Pid -> {ok, Pid}
            end
    catch
        error:badarg -> {error, nil}
    end.

%% Register a process with a string name
register_process_by_name(Pid, Name) when is_pid(Pid), is_binary(Name) ->
    try
        Atom = binary_to_atom(Name, utf8),
        erlang:register(Atom, Pid),
        {ok, nil}
    catch
        error:badarg -> {error, <<"registration_failed">>}
    end.
