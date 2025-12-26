-module(env_ffi).
-export([get_env/1]).

get_env(Key) ->
    case os:getenv(binary_to_list(Key)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.
