%% @doc FFI functions for crypto provider.
%% Provides helper functions for the Gleam crypto adapter implementation.

-module(crypto_provider_ffi).
-export([
    wrap_subject/1,
    unwrap_subject/1,
    register_process_by_name/2,
    system_time_ms/0,
    generate_key_id/0,
    put_persistent_term/2,
    get_persistent_term/1
]).

%% @doc Wrap a Gleam Subject for storage as Dynamic.
%% This preserves the Subject structure for later retrieval.
-spec wrap_subject(term()) -> term().
wrap_subject(Subject) ->
    {crypto_subject, Subject}.

%% @doc Unwrap a stored Dynamic back to a Subject.
%% Returns {ok, Subject} or {error, nil} if invalid.
-spec unwrap_subject(term()) -> {ok, term()} | {error, nil}.
unwrap_subject({crypto_subject, Subject}) ->
    {ok, Subject};
unwrap_subject(_) ->
    {error, nil}.

%% @doc Register a process by name using the process registry.
%% This allows lookup of the crypto provider by name.
-spec register_process_by_name(pid(), binary()) -> {ok, nil} | {error, term()}.
register_process_by_name(Pid, NameBin) when is_binary(NameBin) ->
    Name = binary_to_atom(NameBin, utf8),
    try
        case whereis(Name) of
            undefined ->
                true = register(Name, Pid),
                {ok, nil};
            Pid ->
                % Already registered with same pid
                {ok, nil};
            _OtherPid ->
                % Already registered with different pid
                {error, {already_registered, Name}}
        end
    catch
        _:Reason ->
            {error, Reason}
    end;
register_process_by_name(Pid, Name) when is_atom(Name) ->
    register_process_by_name(Pid, atom_to_binary(Name, utf8)).

%% @doc Get current system time in milliseconds.
-spec system_time_ms() -> integer().
system_time_ms() ->
    erlang:system_time(millisecond).

%% @doc Generate a unique key ID for session keys.
%% Uses a combination of node name, timestamp, and random bytes.
-spec generate_key_id() -> binary().
generate_key_id() ->
    Node = atom_to_binary(node(), utf8),
    Timestamp = integer_to_binary(erlang:system_time(nanosecond)),
    Random = base64:encode(crypto:strong_rand_bytes(8)),
    <<Node/binary, "_", Timestamp/binary, "_", Random/binary>>.

%% @doc Store a value in persistent_term for fast global lookup.
-spec put_persistent_term(binary(), term()) -> nil.
put_persistent_term(Key, Value) when is_binary(Key) ->
    persistent_term:put({crypto_handle, Key}, Value),
    nil.

%% @doc Get a value from persistent_term.
-spec get_persistent_term(binary()) -> {ok, term()} | {error, nil}.
get_persistent_term(Key) when is_binary(Key) ->
    try
        Value = persistent_term:get({crypto_handle, Key}),
        {ok, Value}
    catch
        error:badarg ->
            {error, nil}
    end.
