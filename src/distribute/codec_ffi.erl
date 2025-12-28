-module(codec_ffi).
-export([safe_binary_to_term/1, unsafe_coerce/1, make_tuple/2, make_subject/2]).

safe_binary_to_term(Binary) ->
    try
        {ok, binary_to_term(Binary, [safe])}
    catch
        _:_ -> {error, invalid_binary}
    end.

unsafe_coerce(X) -> X.

make_tuple(A, B) -> {A, B}.

make_subject(Pid, Tag) -> {subject, Pid, Tag}.
