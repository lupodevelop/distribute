%% RPC FFI for Gleam distribute library
-module(rpc_ffi).
-export([to_atom/1, is_badrpc/1, get_badrpc_reason/1, to_dynamic/1, call_with_timeout/5, call_binary_with_timeout/5]).

to_atom(Bin) when is_binary(Bin) ->
	case catch binary_to_existing_atom(Bin, utf8) of
		{'EXIT', _} -> binary_to_atom(Bin, utf8);
		Atom -> Atom
	end;
to_atom(List) when is_list(List) -> to_atom(list_to_binary(List));
to_atom(Atom) when is_atom(Atom) -> Atom.

is_badrpc({badrpc, _Reason}) -> true;
is_badrpc(_) -> false.

get_badrpc_reason({badrpc, Reason}) when is_binary(Reason) -> Reason;
get_badrpc_reason({badrpc, Reason}) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
get_badrpc_reason({badrpc, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_badrpc_reason(_) -> <<"unknown_error">>.

to_dynamic(Value) -> Value.

%% Call with timeout wrapper for RPC: call with timeout in milliseconds
call_with_timeout(Node, Module, Function, Args, TimeoutMs) ->
	try
		rpc:call(Node, Module, Function, Args, TimeoutMs)
	catch
		Class:Reason -> {badrpc, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
	end.

%% Call with timeout wrapper for RPC that expects binary results
%% Used for typed RPC where result must be a binary (envelope-wrapped)
call_binary_with_timeout(Node, Module, Function, Args, TimeoutMs) ->
	try
		case rpc:call(Node, Module, Function, Args, TimeoutMs) of
			{badrpc, _} = BadRpc -> BadRpc;
			Result when is_binary(Result) -> {ok, Result};
			Result -> {error, iolist_to_binary(io_lib:format("expected binary, got: ~p", [Result]))}
		end
	catch
		Class:Reason -> {badrpc, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
	end.
