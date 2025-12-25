%% Raft-lite leader election with term-based voting and heartbeats.
%% This is a simplified Raft implementation for leader election only (no log replication).
-module(raft_ffi).
-export([start/0, stop/0, request_vote/2, request_vote_rpc/2, heartbeat/1, heartbeat/2, current_term/0,
         get_state/0, start_election/0, vote_for/2, am_i_leader/0,
         get_leader/0, get_voted_for/0]).

-define(STATE_TABLE, raft_state).
-define(PROC_NAME, raft_manager).
-define(ELECTION_TIMEOUT_MIN, 150).
-define(ELECTION_TIMEOUT_MAX, 300).
-define(HEARTBEAT_INTERVAL, 50).

%% State record stored in ETS:
%% {state, CurrentTerm, VotedFor, Leader, Role, VotesReceived, LastHeartbeat}
%% Role: follower | candidate | leader

start() ->
    case whereis(?PROC_NAME) of
        undefined ->
            ensure_state_table(),
            init_state(),
            Pid = spawn(fun() -> election_loop() end),
            register(?PROC_NAME, Pid),
            ok;
        _ -> ok
    end.

stop() ->
    case whereis(?PROC_NAME) of
        undefined -> ok;
        Pid ->
            unregister(?PROC_NAME),
            exit(Pid, kill),
            catch ets:delete(?STATE_TABLE),
            ok
    end.

ensure_state_table() ->
    case ets:info(?STATE_TABLE) of
        undefined -> ets:new(?STATE_TABLE, [named_table, public, set]);
        _ -> ok
    end.

init_state() ->
    ensure_state_table(),
    Self = atom_to_list(node()),
    ets:insert(?STATE_TABLE, {term, 0}),
    ets:insert(?STATE_TABLE, {voted_for, none}),
    ets:insert(?STATE_TABLE, {leader, none}),
    ets:insert(?STATE_TABLE, {role, follower}),
    ets:insert(?STATE_TABLE, {votes, []}),
    ets:insert(?STATE_TABLE, {last_heartbeat, erlang:monotonic_time(millisecond)}),
    ets:insert(?STATE_TABLE, {self, Self}),
    ok.

get_val(Key, Default) ->
    ensure_state_table(),
    case ets:lookup(?STATE_TABLE, Key) of
        [{_, V}] -> V;
        [] -> Default
    end.

set_val(Key, Val) ->
    ensure_state_table(),
    ets:insert(?STATE_TABLE, {Key, Val}).

current_term() ->
    get_val(term, 0).

get_voted_for() ->
    case get_val(voted_for, none) of
        none -> "";
        V -> V
    end.

get_leader() ->
    case get_val(leader, none) of
        none -> "";
        L -> L
    end.

am_i_leader() ->
    get_val(role, follower) == leader.

get_state() ->
    #{
        term => current_term(),
        voted_for => get_voted_for(),
        leader => get_leader(),
        role => get_val(role, follower),
        votes => get_val(votes, [])
    }.

%% Election loop: handles timeouts and state transitions
election_loop() ->
    Timeout = random_timeout(),
    receive
        {request_vote, Term, Candidate, From} ->
            handle_vote_request(Term, Candidate, From),
            election_loop();
        {heartbeat, Term, Leader} ->
            handle_heartbeat(Term, Leader),
            election_loop();
        {votes_collected, Term, Votes} ->
            %% Process collected votes
            MyTerm = current_term(),
            if Term == MyTerm ->
                set_val(votes, Votes),
                check_majority();
               true -> ok
            end,
            election_loop();
        start_election_msg ->
            do_start_election(),
            election_loop();
        stop ->
            ok
    after Timeout ->
        Role = get_val(role, follower),
        case Role of
            leader ->
                send_heartbeats(),
                election_loop();
            _ ->
                %% Election timeout: start election if no heartbeat received
                LastHB = get_val(last_heartbeat, 0),
                Now = erlang:monotonic_time(millisecond),
                Elapsed = Now - LastHB,
                if Elapsed > ?ELECTION_TIMEOUT_MAX ->
                    do_start_election();
                   true -> ok
                end,
                election_loop()
        end
    end.

random_timeout() ->
    ?ELECTION_TIMEOUT_MIN + rand:uniform(?ELECTION_TIMEOUT_MAX - ?ELECTION_TIMEOUT_MIN).

%% Start an election: become candidate, increment term, vote for self, request votes
do_start_election() ->
    OldTerm = current_term(),
    NewTerm = OldTerm + 1,
    Self = get_val(self, atom_to_list(node())),
    set_val(term, NewTerm),
    set_val(voted_for, Self),
    set_val(role, candidate),
    set_val(votes, [Self]),
    set_val(leader, none),
    %% Request votes from all connected nodes in a separate process to avoid blocking
    Nodes = nodes(),
    Parent = self(),
    spawn(fun() ->
        CollectedVotes = lists:foldl(fun(N, Acc) ->
            try
                case rpc:call(N, ?MODULE, request_vote_rpc, [NewTerm, Self], 150) of
                    {ok, true} -> [atom_to_list(N) | Acc];
                    _ -> Acc
                end
            catch _:_ -> Acc
            end
        end, [Self], Nodes),
        Parent ! {votes_collected, NewTerm, CollectedVotes}
    end),
    ok.

start_election() ->
    case whereis(?PROC_NAME) of
        undefined -> {error, not_started};
        Pid -> Pid ! start_election_msg, ok
    end.

%% Handle incoming vote request
vote_for(Term, Candidate) ->
    case whereis(?PROC_NAME) of
        undefined -> false;
        Pid ->
            Pid ! {request_vote, Term, Candidate, self()},
            receive
                {vote_response, Granted} -> Granted
            after 100 -> false
            end
    end.

handle_vote_request(Term, Candidate, From) ->
    MyTerm = current_term(),
    MyVote = get_val(voted_for, none),
    Response = if
        Term < MyTerm ->
            false;
        Term > MyTerm ->
            %% Higher term: update term, vote for candidate
            set_val(term, Term),
            set_val(voted_for, Candidate),
            set_val(role, follower),
            set_val(leader, none),
            true;
        Term == MyTerm ->
            case MyVote of
                none ->
                    set_val(voted_for, Candidate),
                    true;
                Candidate -> true;
                _ -> false
            end
    end,
    From ! {vote_response, Response},
    %% If we received a vote, record it (we're the candidate)
    ok.

check_majority() ->
    Votes = get_val(votes, []),
    Total = length(nodes()) + 1,  %% All nodes including self
    Needed = (Total div 2) + 1,
    case length(Votes) >= Needed of
        true ->
            %% Become leader
            Self = get_val(self, atom_to_list(node())),
            set_val(role, leader),
            set_val(leader, Self),
            %% Immediately send heartbeats to announce leadership
            send_heartbeats(),
            {ok, leader};
        false -> 
            {ok, candidate}
    end.

%% Leader sends heartbeats to all followers
send_heartbeats() ->
    Term = current_term(),
    Self = get_val(self, atom_to_list(node())),
    Nodes = nodes(),
    lists:foreach(fun(N) ->
        catch rpc:cast(N, ?MODULE, heartbeat, [Term, Self])
    end, Nodes).

heartbeat(Term, Leader) when is_integer(Term), is_list(Leader) ->
    case whereis(?PROC_NAME) of
        undefined -> ok;
        Pid -> Pid ! {heartbeat, Term, Leader}, ok
    end;
heartbeat(Term, Leader) when is_integer(Term), is_binary(Leader) ->
    heartbeat(Term, binary_to_list(Leader));
heartbeat(_, _) ->
    ok.

heartbeat(Leader) when is_list(Leader) ->
    %% Compatibility: assume current term
    heartbeat(current_term(), Leader);
heartbeat(Leader) when is_binary(Leader) ->
    heartbeat(binary_to_list(Leader));
heartbeat(_) ->
    ok.

handle_heartbeat(Term, Leader) ->
    MyTerm = current_term(),
    if
        Term >= MyTerm ->
            set_val(term, Term),
            set_val(leader, Leader),
            set_val(role, follower),
            set_val(voted_for, none),
            set_val(last_heartbeat, erlang:monotonic_time(millisecond));
        true -> ok
    end.

%% Request vote RPC entry point (for external callers)
request_vote(Term, Candidate) when is_integer(Term), is_list(Candidate) ->
    vote_for(Term, Candidate);
request_vote(Term, Candidate) when is_integer(Term), is_binary(Candidate) ->
    vote_for(Term, binary_to_list(Candidate));
request_vote(_, _) ->
    false.

%% Direct RPC for vote collection - returns {ok, true/false}
request_vote_rpc(Term, Candidate) when is_integer(Term), is_list(Candidate) ->
    MyTerm = current_term(),
    MyVote = get_val(voted_for, none),
    Result = if
        Term < MyTerm ->
            false;
        Term > MyTerm ->
            %% Higher term: update term, vote for candidate
            set_val(term, Term),
            set_val(voted_for, Candidate),
            set_val(role, follower),
            set_val(leader, none),
            set_val(votes, []),
            true;
        Term == MyTerm ->
            case MyVote of
                none ->
                    set_val(voted_for, Candidate),
                    true;
                Candidate -> true;
                _ -> false
            end
    end,
    {ok, Result};
request_vote_rpc(Term, Candidate) when is_integer(Term), is_binary(Candidate) ->
    request_vote_rpc(Term, binary_to_list(Candidate));
request_vote_rpc(_, _) ->
    {ok, false}.
