-module (linkmon).
-export([start_critic/0, judge/3, critic/0,
         start_critic2/0, restarter/0,
         restarter2/0, judge2/2,
         judge3/2, critic2/0, restarter3/0, start_critic3/0]).

start_critic() ->
    spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic() ->
    receive
        {From,
            {"Rage Against the Turing Machine",
              "Unit Testify"}} ->
                From ! {self(), "They are great!"};
        {From, {"System of a Downtime", "Memoize"}} ->
            From ! {self(),
                "They're not Johnny Crash but they're good."};
        {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {self(), "Simply incredible."};
        {From, {_Band, _Album}} ->
            From ! {self(), "They are terrible!"}
    end,
    critic().

% To keep the critic alive, we'll write a basic 'supervisor'
% process whose only role is to restart it when it goes down:

start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual termination
            ok;
        {'EXIT', Pid, _} ->
            restarter()
    end.

% The problem with our approach is that there is no way to find
% the Pid of the critic, and thus we can't call him to have his
% opinion. One of the solutions Erlang has to solve this is to
% give names to processes.

restarter2() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual termination
            ok;
        {'EXIT', Pid, _} ->
            restarter()
    end.

judge2(Band, Album) ->
    critic ! {self(), {Band, Album}},
    % the line Pid = whereis(critic) is used to find the
    % critic's process identifier in order to pattern match
    % against it in the receive expression
    Pid = whereis(critic),
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

% We want to match with this pid, because it makes sure we will
% match on the right message (there could be 500 of them in the
% mailbox as we speak!) This can be the source of a problem
% though.
% The code above assumes that the critic's pid will remain the
% same between the first two lines of the function.
% The possibility that things go wrong in a different process
% can make another one go wrong if we don't do things right.
% In this case, the value of the critic atom can be seen from
% multiple processes. This is known as shared state

judge3(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        % key difference here. We don't match the critic
        % Pid, but we send a unique ref to it and match
        % a message containing it. Pid can change.
        % Ref doesn't.
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic2() ->
    receive
        {From,
         Ref,
         {"Rage Against the Turing Machine",
          "Unit Testify"}} ->
                From ! {Ref, "They are great!"};
        {From,
         Ref,
         {"System of a Downtime", "Memoize"}} ->
                From ! {Ref,
                    "They're not Johnny Crash but they're good."};
        {From,
         Ref,
         {"Johnny Crash", "The Token Ring of Fire"}} ->
                From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic2().

start_critic3() ->
    spawn(?MODULE, restarter3, []).

restarter3() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual termination
            ok;
        {'EXIT', Pid, _} ->
            restarter3()
    end.