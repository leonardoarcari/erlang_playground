-module(pot).
% The clean way:
% -export([add/2, hello/0, greet_and_add_two/1]).
% Dirty shit:
-compile(export_all).

% The syntax of a function follows the form
% Name(Args) -> Body,
% Name is an atom, body is a comma separated list of
% expressions.
add(A, B) ->
	A + B.

-define(sub(X, Y), add(X, -Y)).

hello() ->
	io:format("Hello, world!~n").

greet_and_add_two(X) ->
	hello(),
	add(X, 2).

%% Greet people based on their gender
%% We pattern match on the args instead of having big
%% body with many if-then-else statements.
% function(X) ->
% Expression;
% function(Y) ->
% Expression;
% function(_) ->
% Expression.
greet(male, Name) ->
	io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
	io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
	io:format("Hello, ~s!", [Name]).

head([H|_]) -> H.
second([_, X|_]) -> X.

%% Now an interesting usage of pattern matching:
%% If you call same(a, a), the first X is bound to a. Then
%% the second X is compared with a. Because the value of X
%% is a, the match is ok and the first pattern of same is 
%% chosen. In any other case the match with second X would
%% fail and the second pattern of same would be chosen. 
same(X, X) -> true;
same(_, _) -> false.

% Note that it is possible to use the = operator in the
% function head, allowing us to match both the content inside
% a tuple ({Y,M,D}) and the tuple as a whole (Date).
% In Haskell this would be expressed with syntax: Date@(Y, M, D)
valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
	io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
	io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
	io:format("Stop feeding me wrong data!~n").

% Playing around with guards
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

% The comma (,) acts like operator andalso, while the semicolon
% (;) acts like orelse:
% if the first guard fails, it then tries the second,
% and then the next one, until either one guard succeeds
% or they all fail.
right_age(X) when X >= 16, X =< 104 -> true;
right_age(_) -> false.

wrong_age(X) when X < 16; X > 104 -> true;
wrong_age(_) -> false.

%% Pattern-guards: IF
% Remember, in Erlang, everything has to return something,
% and if expressions are no exception to the rule. As such,
% when Erlang can't find a way to have a guard succeed, 
% it will crash: it cannot not return something.
heh_fine() ->
	if 1 =:= 1 -> works
	end,
	if 1 =:= 2; 1 =:= 1 -> works
	end,
	if 1 =:= 2, 1 =:= 1 -> fails
	end.

% Let's try again
oh_god(N) ->
	if N =:= 2 -> might_succeed;
	   true -> always_does %% this is Erlang's
	   					   %% 'else' branch
	end.

help_me(Animal) ->
	Talk = if Animal == cat -> "meow";
		      Animal == beef -> "mooo";
		      Animal == dog -> "bark";
		      Animal == tree -> "bark";
		      true -> "fdkjwefkwg"
		   end,
	{Animal, "says " ++ Talk ++ "!"}.

beach(Temperature) ->
	case Temperature of
		{celsius, N} when N >= 20, N =< 45 ->
			'favorable';
		{kelvin, N} when N >= 293, N =< 318 ->
			'scientifically favorable';
		{fahrenheit, N} when N >= 68, N =< 113 ->
			'favorable in the US';
		_ -> 'avoid beach'
	end.

%% Recursion
fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

% Trivial length
len([]) -> 0;
len([_|T]) -> 1 + len(T).

% Tail-recursive length
% Note that we can define functions with same name and
% different arity and they don't clash. A function is
% fully-specified by the triple {module, funcName, arity}.
tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).

% This function takes an integer as its first parameter and
% then any other term as its second parameter. It will then
% create a list of as many copies of the term as specified
% by the integer.
duplicate(N, Term) ->
	duplicate(N, Term, []).
% Helper tail-recursive implementation
duplicate(0, _, Acc) ->
	Acc;
duplicate(N, Term, Acc) when N > 0 ->
	duplicate(N-1, Term, [Term|Acc]).

% Reverse a list
reverse(L) -> tail_reverse(L).
tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).

% Sublist
% takes a list L and an integer N, and returns the N first
% elements of the list
sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H|T], N) -> [H|sublist(T, N-1)].

%% Tail recursive implementation
tail_sublist(L, N) -> reverse(tail_sublist(L, N, [])).

tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([], _, Acc) -> Acc;
tail_sublist([H|T], N, Acc) ->
	tail_sublist(T, N-1, [H|Acc]).

zip([], _) -> [];
zip(_, []) -> [];
zip([H1|T1], [H2|T2]) -> [{H1, H2}|zip(T1, T2)].

tail_zip(Xs, Ys) -> reverse(tail_zip(Xs, Ys, [])).

tail_zip([], _, Acc) -> Acc;
tail_zip(_, [], Acc) -> Acc;
tail_zip([X|Xs], [Y|Ys], Acc) ->
	tail_zip(Xs, Ys, [{X, Y}|Acc]).