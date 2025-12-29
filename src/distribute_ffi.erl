-module(distribute_ffi).
-export([wrap_subject/1, unwrap_subject/1, to_dynamic/1]).

%% Wrap a Subject into a dynamic value
wrap_subject(Subject) -> Subject.

%% Unwrap a dynamic value into a Subject
unwrap_subject(Dynamic) -> {ok, Dynamic}.

%% Convert Int to Dynamic
to_dynamic(Value) -> Value.
