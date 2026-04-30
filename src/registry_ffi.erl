-module(registry_ffi).
-export([register/2, unregister/1, whereis/1]).

%% Uses binary names directly with global:register_name/2.
%% No atom conversion -- no atom table exhaustion risk.
%% global:register_name accepts any Erlang term as a name.
%%
%% All functions return tagged tuples that map directly to Gleam Result/custom types.

%% ----------------------------------------------------------------------------
%% Local-ownership ACL: stateless `node(Pid) =:= node()` check
%%
%% `:global.unregister_name/1` has no built-in access control: any node
%% may unregister any globally-registered name. To stop the "registry
%% wipe" attack via unvalidated input, our `unregister/1` only forwards
%% to `:global` when the name resolves to a PID running on THIS VM.
%%
%% An earlier draft kept a local ETS mirror of "names we registered";
%% that introduces two flaws in a distributed system:
%%   1. the table never gets pruned when a registered process dies,
%%      causing an unbounded memory leak under churn;
%%   2. when a name is reclaimed by a remote node after a local crash,
%%      our stale ETS entry would falsely report ownership and let an
%%      attacker drop a foreign actor's registration -- an ACL bypass.
%%
%% The single source of truth in a distributed system *is* :global.
%% We ask :global where the PID lives and compare its node with ours.
%% No state to leak, no staleness to bypass.
%% ----------------------------------------------------------------------------

register(Name, Pid) ->
    case erlang:is_pid(Pid) of
        true ->
            case global:register_name(Name, Pid) of
                yes -> {ok, nil};
                no  -> {error, already_exists}
            end;
        false ->
            {error, invalid_process}
    end.

unregister(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            %% Name not registered at all -- nothing to do, but report
            %% so the caller can distinguish "I removed it" from
            %% "it wasn't there" when that distinction matters.
            {error, not_found};
        Pid ->
            case node(Pid) =:= node() of
                true ->
                    global:unregister_name(Name),
                    {ok, nil};
                false ->
                    %% Owner runs on another node -- ACL refuses.
                    {error, not_owned}
            end
    end.

whereis(Name) ->
    case global:whereis_name(Name) of
        undefined -> {error, nil};
        Pid       -> {ok, Pid}
    end.
