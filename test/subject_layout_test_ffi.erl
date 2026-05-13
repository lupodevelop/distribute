-module(subject_layout_test_ffi).
-export([check_layout/1]).

%% Validates that the Gleam core library (gleam_erlang) still uses the
%% `{subject, Pid, Tag}` layout. If this fails, it means the layout has changed
%% in an upstream update, which would break our distribute_ffi_utils.erl boundary.
check_layout({subject, Pid, _Tag}) when is_pid(Pid) -> true;
check_layout(_) -> false.
