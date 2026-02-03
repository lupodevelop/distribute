%% FFI for BEAM discovery adapter
-module(discovery_ffi).
-export([wrap_subject/1, unwrap_subject/1, register_process_by_name/2, 
         get_erlang_nodes/0, get_self_subject/0, spawn_event_handler/2,
         spawn_hook_handler/3]).

%% Wrap a Subject into a dynamic value (no-op in Erlang)
wrap_subject(Subject) -> Subject.

%% Unwrap a dynamic value into a Subject
unwrap_subject(Dynamic) -> {ok, Dynamic}.

%% Register a process with the given name (binary)
register_process_by_name(Pid, Name) when is_pid(Pid), is_binary(Name) ->
    case to_atom_safe(Name) of
        {ok, AtomName} ->
            try
                register(AtomName, Pid),
                {ok, nil}
            catch
                error:badarg -> 
                    %% Already registered
                    {error, {start_failed, <<"Name already registered">>}}
            end;
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end;
register_process_by_name(_Pid, _Name) ->
    {error, {start_failed, <<"Invalid arguments">>}}.

%% Get list of connected Erlang nodes as binary strings
get_erlang_nodes() ->
    Nodes = nodes(),
    lists:map(fun(Node) -> 
        atom_to_binary(Node, utf8) 
    end, Nodes).

%% Get self subject - returns error since we can't reliably get the actor's subject
%% The caller should have stored the subject during start
get_self_subject() ->
    {error, nil}.

%% Safe atom conversion with allowed list
to_atom_safe(Binary) when is_binary(Binary) ->
    case binary:match(Binary, [<<"\0">>, <<" ">>, <<"@">>]) of
        nomatch ->
            %% Check if it's already an existing atom
            try binary_to_existing_atom(Binary, utf8) of
                Atom -> {ok, Atom}
            catch
                error:badarg ->
                    %% For discovery names, we allow creation of new atoms
                    %% but only for valid identifiers
                    case is_valid_identifier(Binary) of
                        true ->
                            NewAtom = binary_to_atom(Binary, utf8),
                            {ok, NewAtom};
                        false ->
                            {error, <<"invalid_name_format">>}
                    end
            end;
        _ ->
            {error, <<"invalid_characters_in_name">>}
    end.

%% Check if a binary is a valid identifier
is_valid_identifier(<<>>) -> false;
is_valid_identifier(Binary) ->
    case Binary of
        <<First, Rest/binary>> when First >= $a, First =< $z ->
            is_valid_rest(Rest);
        <<First, Rest/binary>> when First >= $A, First =< $Z ->
            is_valid_rest(Rest);
        <<"_", Rest/binary>> ->
            is_valid_rest(Rest);
        _ ->
            false
    end.

is_valid_rest(<<>>) -> true;
is_valid_rest(<<C, Rest/binary>>) when C >= $a, C =< $z -> is_valid_rest(Rest);
is_valid_rest(<<C, Rest/binary>>) when C >= $A, C =< $Z -> is_valid_rest(Rest);
is_valid_rest(<<C, Rest/binary>>) when C >= $0, C =< $9 -> is_valid_rest(Rest);
is_valid_rest(<<"_", Rest/binary>>) -> is_valid_rest(Rest);
is_valid_rest(_) -> false.

%% Spawn a process to handle an event callback asynchronously
%% This protects the main actor from slow or blocking callbacks
spawn_event_handler(Callback, Event) ->
    spawn(fun() ->
        try
            Callback(Event)
        catch
            _:_ -> ok  %% Silently ignore callback errors
        end
    end).

%% Spawn a process to handle a hook callback asynchronously
spawn_hook_handler(Hook, Peer, Metadata) ->
    spawn(fun() ->
        try
            Hook(Peer, Metadata)
        catch
            _:_ -> ok  %% Silently ignore hook errors
        end
    end).
