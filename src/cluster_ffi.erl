%% Minimal Erlang FFI for Gleam distribute library
-module(cluster_ffi).
-export([start_node/2, connect/1, nodes/0, self_node/0, ping/1,
         is_ok_atom/1, get_error_reason/1, is_true/1, is_false/1, is_ignored/1]).

%% start_node(NameListOrBinary, CookieListOrBinary) -> ok | {error, Reason}
start_node(Name, Cookie) ->
    NameAtom = to_atom_force(Name),
    %% Determine if we should use shortnames or longnames based on the host part
    NameStr = atom_to_list(NameAtom),
    Type = case string:tokens(NameStr, "@") of
        [_, Host] ->
            case lists:member($., Host) of
                true -> longnames;
                false -> shortnames
            end;
        _ -> shortnames %% Fallback
    end,
    try
        net_kernel:start([NameAtom, Type]),
        erlang:set_cookie(node(), to_atom_force(Cookie)),
        ok
    catch
        Class:Reason -> {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

connect(Node) ->
    case to_atom_safe(Node) of
        {ok, A} -> 
            case net_kernel:connect_node(A) of
                true -> true;
                false -> false;
                ignored -> ignored
            end;
        _ -> false
    end.

nodes() ->
    [atom_to_binary(N, utf8) || N <- erlang:nodes()].

self_node() ->
    atom_to_binary(node(), utf8).

ping(Node) ->
    case to_atom_safe(Node) of
        {ok, A} -> case net_adm:ping(A) of
                        pong -> true;
                        _ -> false
                    end;
        _ -> false
    end.

%% Helpers for Gleam FFI
is_ok_atom(ok) -> true;
is_ok_atom(_) -> false.

get_error_reason({error, Reason}) when is_binary(Reason) -> Reason;
get_error_reason({error, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_error_reason(_) -> <<"unknown_error">>.

%% Helper functions for checking connect result
is_true(true) -> true;
is_true(_) -> false.

is_false(false) -> true;
is_false(_) -> false.

is_ignored(ignored) -> true;
is_ignored(_) -> false.

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

to_atom_force(Bin) when is_binary(Bin) -> binary_to_atom(Bin, utf8);
to_atom_force(Bin) when is_list(Bin) -> list_to_atom(Bin);
to_atom_force(Atom) when is_atom(Atom) -> Atom.
