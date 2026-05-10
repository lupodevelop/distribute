-module(config_ffi).
-export([put_config/1, get_config/1, reset_config/0,
         put_atom_budget/1,
         put_conflict_resolver_timeout/1,
         get_conflict_resolver_timeout/0]).

%% Atom keys in persistent_term are never exposed to the Gleam side.
-define(KEY, distribute_config).
-define(BUDGET_VALUE_KEY, distribute_atom_budget).
-define(CONFLICT_TIMEOUT_KEY, distribute_conflict_resolver_timeout).
-define(DEFAULT_CONFLICT_TIMEOUT_MS, 1000).

%% Store config. persistent_term:put/2 is O(1) to read but
%% invalidates per-process heap copies on write. Call once at startup.
put_config(Config) ->
    case persistent_term:get(?KEY, nil) of
        nil ->
            persistent_term:put(?KEY, Config),
            {ok, nil};
        _ ->
            {error, already_configured}
    end.

%% Read config, returning Default if not yet configured.
get_config(Default) ->
    persistent_term:get(?KEY, Default).

%% Atom budget setter written as a separate persistent_term slot
%% so the FFI atom helpers can read it without decoding the Gleam
%% Config tuple. Called from Gleam-side `configure` after the main
%% Config write succeeds.
put_atom_budget(N) when is_integer(N), N > 0 ->
    persistent_term:put(?BUDGET_VALUE_KEY, N),
    nil.

%% Conflict-resolver timeout setter uses a separate persistent_term slot
%% so `conflict_ffi:safe_invoke/5` can read the deadline without
%% decoding the Gleam Config tuple. Called from Gleam-side
%% `configure` after the main Config write succeeds.
put_conflict_resolver_timeout(N) when is_integer(N), N > 0 ->
    persistent_term:put(?CONFLICT_TIMEOUT_KEY, N),
    nil.

%% Read the configured conflict-resolver deadline. Returns the
%% built-in default (1 000 ms) if `configure/1` has not been
%% called. Mirrors `config:default()` so an unconfigured node
%% has the same timeout as a freshly-configured one.
get_conflict_resolver_timeout() ->
    persistent_term:get(?CONFLICT_TIMEOUT_KEY, ?DEFAULT_CONFLICT_TIMEOUT_MS).

%% Test utility: clear configuration AND the atom budget counter so
%% we can reconfigure and rerun budget-bounded scenarios.
reset_config() ->
    persistent_term:erase(?KEY),
    persistent_term:erase(?BUDGET_VALUE_KEY),
    persistent_term:erase(?CONFLICT_TIMEOUT_KEY),
    distribute_ffi_utils:atom_budget_reset(),
    nil.
