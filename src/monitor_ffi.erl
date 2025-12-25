%% Monitor FFI for Gleam distribute library
-module(monitor_ffi).
-export([to_atom/1]).

to_atom(Bin) when is_binary(Bin) ->
	case catch binary_to_existing_atom(Bin, utf8) of
		{'EXIT', _} -> binary_to_atom(Bin, utf8);
		Atom -> Atom
	end;
to_atom(List) when is_list(List) -> to_atom(list_to_binary(List));
to_atom(Atom) when is_atom(Atom) -> Atom.
