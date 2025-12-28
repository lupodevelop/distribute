%% Groups FFI (wrapper around :pg) for Gleam distribute library
-module(groups_ffi).
-export([join/2, leave/2, members/1, broadcast/2, broadcast_binary/2,
         is_ok_atom/1, get_error_reason/1, unwrap_members/1]).

join(Group, Pid) when is_pid(Pid) ->
    case to_atom_safe(Group) of
        {ok, G} -> 
            case try_pg_join(G, Pid) of
                ok -> ok;
                Error -> Error
            end;
        Error -> Error
    end;
join(_Group, _NotPid) ->
    {error, <<"not_a_pid">>}.

leave(Group, Pid) when is_pid(Pid) ->
    case to_atom_safe(Group) of
        {ok, G} -> 
            case try_pg_leave(G, Pid) of
                ok -> ok;
                Error -> Error
            end;
        Error -> Error
    end;
leave(_Group, _NotPid) ->
    {error, <<"not_a_pid">>}.

members(Group) ->
    case to_atom_safe(Group) of
        {ok, G} ->
            case try_pg_get_members(G) of
                {ok, Members} -> {ok, Members};
                Error -> Error
            end;
        Error -> Error
    end.

broadcast(Group, Msg) ->
    case to_atom_safe(Group) of
        {ok, G} ->
            case try_pg_get_members(G) of
                {ok, Members} ->
                    lists:foreach(fun(Pid) -> Pid ! Msg end, Members),
                    ok;
                Error -> Error
            end;
        Error -> Error
    end.

%% Broadcast binary message to all members of a group (for typed messaging)
broadcast_binary(Group, BinaryMsg) when is_binary(BinaryMsg) ->
    case to_atom_safe(Group) of
        {ok, G} ->
            case try_pg_get_members(G) of
                {ok, Members} ->
                    %% Validate binary size (max 10MB for safety)
                    case byte_size(BinaryMsg) > 10485760 of
                        true -> {error, <<"message_too_large">>};
                        false ->
                            %% Filter out dead processes before sending
                            AliveMembers = lists:filter(fun is_process_alive/1, Members),
                            try
                                %% Wrap in {nil, Msg} to match Subject(Pid, Nil)
                                lists:foreach(fun(Pid) -> Pid ! {nil, BinaryMsg} end, AliveMembers),
                                ok
                            catch
                                _:Reason -> {error, iolist_to_binary(io_lib:format("broadcast_failed: ~p", [Reason]))}
                            end
                    end;
                Error -> Error
            end;
        Error -> Error
    end;
broadcast_binary(_Group, _Msg) ->
    {error, <<"invalid_binary">>}.

%% Helpers for Gleam FFI
is_ok_atom(ok) -> true;
is_ok_atom(_) -> false.

get_error_reason({error, Reason}) when is_binary(Reason) -> Reason;
get_error_reason({error, Reason}) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
get_error_reason({error, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_error_reason(_) -> <<"unknown_error">>.

unwrap_members({ok, Members}) when is_list(Members) -> Members;
unwrap_members(_) -> [].

%% Internal helpers
to_atom_safe(Bin) when is_list(Bin) -> to_atom_safe(list_to_binary(Bin));
to_atom_safe(Bin) when is_binary(Bin) -> 
    Allow = persistent_term:get(distribute_allow_atom_creation, false),
    case catch binary_to_existing_atom(Bin, utf8) of
        {'EXIT', _} -> 
            case Allow of
                true -> {ok, binary_to_atom(Bin, utf8)};
                false -> {error, <<"atom_not_existing">>} 
            end;
        Atom -> {ok, Atom}
    end;
to_atom_safe(Atom) when is_atom(Atom) -> {ok, Atom};
to_atom_safe(_) -> {error, <<"badarg">>}.

%% Internal helpers using try/catch to avoid crashing when :pg isn't running
try_pg_join(G, Pid) ->
    try 
        pg:join(G, Pid)
    catch
        Class:Reason -> {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

try_pg_leave(G, Pid) ->
    try 
        pg:leave(G, Pid)
    catch
        Class:Reason -> {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

try_pg_get_members(G) ->
    try 
        Members = pg:get_members(G),
        {ok, Members}
    catch
        Class:Reason -> {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.
