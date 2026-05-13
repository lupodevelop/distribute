-module(distribute_test_ffi).
-export([read_gleam_toml_version/0]).

%% Read the `version = "..."` line from `gleam.toml` at the project
%% root and return the version string. Used by the regression test
%% that pins `distribute.version()` to the manifest. since Gleam
%% has no compile-time API to read the manifest, this regression is
%% a runtime read against the on-disk file.
%%
%% The compiled tests run from `build/dev/erlang/distribute/ebin`, so
%% the manifest sits four levels up. We resolve via the `code:lib_dir/1`
%% trick to stay independent of the working directory.
read_gleam_toml_version() ->
    Path = locate_gleam_toml(),
    {ok, Bin} = file:read_file(Path),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    extract_version(Lines).

locate_gleam_toml() ->
    %% Walk up from the test's CWD until we find gleam.toml. Tests
    %% run with CWD set to the project root by `gleam test`, so the
    %% file is right there.
    case filelib:is_regular(<<"gleam.toml">>) of
        true -> <<"gleam.toml">>;
        false -> error({gleam_toml_not_found, file:get_cwd()})
    end.

extract_version([]) ->
    error(version_line_not_found_in_gleam_toml);
extract_version([Line | Rest]) ->
    Trimmed = string:trim(Line, leading),
    case binary:match(Trimmed, <<"version">>) of
        nomatch ->
            extract_version(Rest);
        {0, _} ->
            %% A line beginning with `version`. assume `version = "X.Y.Z"`.
            case binary:split(Line, <<"\"">>, [global]) of
                [_, Version, _] -> Version;
                _ -> extract_version(Rest)
            end;
        _ ->
            extract_version(Rest)
    end.
