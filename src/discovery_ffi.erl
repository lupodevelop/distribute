%% FFI for BEAM discovery adapter
-module(discovery_ffi).
-export([wrap_subject/1, unwrap_subject/1, register_process_by_name/2, 
         get_erlang_nodes/0, get_self_subject/0, spawn_event_handler/2,
         spawn_hook_handler/3]).

%% Import shared utility for safe atom conversion
-import(distribute_ffi_utils, [to_atom_safe/1]).

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

%% Spawn a process to handle an event callback asynchronously
%% This protects the main actor from slow or blocking callbacks
spawn_event_handler(Callback, Event) ->
    spawn(fun() ->
        try
            Callback(Event)
        catch
            Class:Reason ->
                error_logger:warning_msg(
                    "distribute discovery: event callback failed ~p:~p~n",
                    [Class, Reason])
        end
    end).

%% Spawn a process to handle a hook callback asynchronously
spawn_hook_handler(Hook, Peer, Metadata) ->
    spawn(fun() ->
        try
            Hook(Peer, Metadata)
        catch
            Class:Reason ->
                error_logger:warning_msg(
                    "distribute discovery: hook callback failed ~p:~p~n",
                    [Class, Reason])
        end
    end).
