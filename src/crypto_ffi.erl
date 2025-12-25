%% Crypto FFI for generating random base64 strings

-module(crypto_ffi).
-export([rand_base64/1, rand_base64_safe/1, is_ok/1, get_error/1]).
rand_base64(N) when is_integer(N), N >= 0 ->
    try
        Bin = crypto:strong_rand_bytes(N),
        base64:encode_to_string(Bin)
    catch
        _:_ ->
            % Fallback: return a unique integer string in case crypto is not available
            integer_to_list(erlang:unique_integer([positive]))
    end.
rand_base64_safe(N) when is_integer(N), N >= 0 ->
    try
        Bin = crypto:strong_rand_bytes(N),
        {ok, base64:encode_to_string(Bin)}
    catch
        Class:Reason -> {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

is_ok({ok, _}) -> true;
is_ok(ok) -> true;
is_ok(_) -> false.

get_error({error, Reason}) when is_binary(Reason) -> Reason;
get_error({error, Reason}) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
get_error({error, Reason}) -> iolist_to_binary(io_lib:format("~p", [Reason]));
get_error(_) -> <<"unknown_error">>.
