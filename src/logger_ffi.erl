%% Simple logger backend FFI for Gleam 'log' module
%% Simple logger backend FFI for Gleam 'log' module
% Module declaration
-module(logger_ffi).
-export([log/3, set_backend/1]).

%% Set backend name in persistent_term (string): "console" or "erlang_logger"
set_backend(BackendBin) when is_binary(BackendBin) ->
    persistent_term:put("distribute_log_backend", BackendBin),
    ok.

%% Log the message. Map level string to erlang logger level if set to "erlang_logger"
log(LevelBin, MsgBin, MetaBin) when is_binary(LevelBin), is_binary(MsgBin), is_binary(MetaBin) ->
    Backend = persistent_term:get("distribute_log_backend", <<"console">>),
    case Backend of
        <<"console">> -> io:put_chars(MsgBin);
        <<"erlang_logger">> -> 
            LevelAtom = case LevelBin of
                <<"DEBUG">> -> debug;
                <<"INFO">> -> info;
                <<"WARN">> -> warning;
                <<"ERROR">> -> error;
                _ -> info
            end,
            MetaMap = parse_meta(MetaBin),
            logger:log(LevelAtom, #{message => MsgBin, meta => MetaMap});
        _ -> io:put_chars(MsgBin)
    end.

parse_meta(<<>>) -> #{};
parse_meta(MetaBin) when is_binary(MetaBin) ->
    RawList = string:tokens(binary_to_list(MetaBin), ","),
    parse_tokens(RawList, #{}).

parse_tokens([], Map) -> Map;
parse_tokens([Token | Rest], Map) ->
    case string:tokens(Token, "=") of
        [K, V] -> NewMap = maps:put(list_to_binary(string:trim(K)), list_to_binary(string:trim(V)), Map),
                  parse_tokens(Rest, NewMap);
        _ -> parse_tokens(Rest, Map)
    end.

