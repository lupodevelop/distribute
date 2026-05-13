-module(telemetry_test_ffi).
-export([spawn_short_lived/0, exit_self_shutdown/0,
         silence_logger/0, restore_logger/1]).

%% Spawn a process that exits immediately after returning its Pid.
%% Used by the call_target_down test to fabricate a dead PID for
%% `global.from_pid` without resorting to brittle `process.kill`
%% timing.
spawn_short_lived() ->
    erlang:spawn(fun() -> ok end).

%% Synchronously exit the calling process with reason `shutdown`.
%% Used by the `call_isolated` proxy-crash test to simulate the
%% "proxy died before sending its result" branch without producing
%% an error_logger CRASH REPORT in the test output. The
%% `IsolatedProxyDown` branch fires for any DOWN reason, so a clean
%% `shutdown` exit exercises exactly the same code path as a real
%% panic from user `make_request` would, minus the SASL noise.
%% Declared with return type Int on the Gleam side so it can stand
%% in for a `make_request` body; at runtime it never returns.
exit_self_shutdown() ->
    erlang:exit(self(), shutdown).

%% Disable the primary logger so warning/error reports do not pollute
%% the test stdout while we deliberately exercise a panicking sink.
%% Returns the previous level so the test can restore it. In
%% production the warning *is* the point: it is the operator's
%% signal that a sink is misbehaving. In tests we want green output.
silence_logger() ->
    Old = logger:get_primary_config(),
    PrevLevel = maps:get(level, Old, notice),
    logger:set_primary_config(level, none),
    PrevLevel.

%% Restore the primary logger level captured by silence_logger/0.
restore_logger(PrevLevel) ->
    logger:set_primary_config(level, PrevLevel),
    nil.
