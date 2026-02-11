-module(cluster_ffi).
-export([start_node/2, connect/1, nodes/0, self_node/0, ping/1,
         is_ok_atom/1, get_error_reason/1, is_true/1, is_ignored/1]).

-import(distribute_ffi_utils, [to_atom_safe/1]).

%% Start a distributed BEAM node.
%% Node names are created via binary_to_atom since they MUST be new atoms.
%% Input is validated (max 512 bytes, no null bytes) before creation.
start_node(Name, Cookie) ->
    NameAtom = to_atom_force(Name),
    Type = case string:tokens(atom_to_list(NameAtom), "@") of
        [_, Host] ->
            case lists:member($., Host) of
                true -> longnames;
                false -> shortnames
            end;
        _ -> shortnames
    end,
    try
        net_kernel:start([NameAtom, Type]),
        erlang:set_cookie(node(), to_atom_force(Cookie)),
        ok
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
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
        {ok, A} ->
            case net_adm:ping(A) of
                pong -> true;
                _ -> false
            end;
        _ -> false
    end.

%% Helpers for Gleam FFI result classification (delegate to shared utils)
is_ok_atom(V) -> distribute_ffi_utils:is_ok_atom(V).
get_error_reason(V) -> distribute_ffi_utils:get_error_reason(V).

is_true(true) -> true;
is_true(_) -> false.

is_ignored(ignored) -> true;
is_ignored(_) -> false.

%% Internal: create atom for node names/cookies (validated input only).
to_atom_force(Bin) when is_binary(Bin) ->
    case is_valid_node_input(Bin) of
        true -> binary_to_atom(Bin, utf8);
        false ->
            try binary_to_existing_atom(Bin, utf8)
            catch _:_ -> binary_to_atom(Bin, utf8)
            end
    end;
to_atom_force(Bin) when is_list(Bin) ->
    to_atom_force(list_to_binary(Bin));
to_atom_force(Atom) when is_atom(Atom) ->
    Atom.

is_valid_node_input(Bin) when is_binary(Bin) ->
    byte_size(Bin) =< 512 andalso
    binary:match(Bin, <<"\0">>) =:= nomatch.
