-module(kitchen).
-export([fridge1/0, fridge2/1,
         store/2, take/2, start/1,
         store2/2, take2/2]).

% The process will allow two operations: storing food in the
% fridge and taking food from the fridge. It should only be
% possible to take food that has been stored beforehand.

% When we ask to store the food, the process should reply with
% ok, but there is nothing actually storing the food; 
% fridge1() is called and then the function starts from
% scratch, without state

fridge1() ->
    receive
        {From, {store, _Food}} ->
            From ! {self(), ok},
            fridge1();
        {From, {take, _Food}} ->
            %% uh...
            From ! {self(), not_found},
            fridge1();
        terminate ->
            ok
    end.

% The function uses lists:member/2 to check whether Food is
% part of FoodList or not. Depending on the result, the item
% is sent back to the calling process (and removed from
% FoodList) or not_found is sent back otherwise

fridge2(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge2([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true -> From ! {self(), {ok, Food}},
                        fridge2(lists:delete(Food, FoodList));
                false -> From ! {self(), not_found},
                         fridge2(FoodList)
            end;
        terminate ->
            ok
    end.


% Something annoying with the previous example is that the
% programmer who's going to use the fridge has to know about
% the protocol that's been invented for that process. That's a
% useless burden. A good way to solve this is to abstract
% messages away with the help of functions dealing with
% receiving and sending them:

store(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

take(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

% One thing left to do would be to hide that whole part about
% needing to spawn a process. We dealt with hiding messages,
% but then we still expect the user to handle the creation of
% the process. I'll add the following start/1 function:

start(FoodList) ->
    spawn(?MODULE, fridge2, [FoodList]).

% In general, anything dealing with asynchronous operations
% (which is how message passing is done in Erlang) needs a way
% to give up after a certain period of time if it gets no sign
% of receiving data.
% Erlang certainly has an appropriate mechanism for that,
% and it's part of the receive construct:
%
% receive
%     Match -> Expression1
% after Delay ->
%     Expression2
% end.

store2(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.

take2(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.