-module(tree).
-export([empty/0, insert/3, lookup/2]).

% A tagged tuple for representing a Node of a Tree
% {node, {Key, Value, Smaller, Larger}}
% {node, 'nil'}

empty() -> {node, 'nil'}.

insert(NewKey, NewValue, {node, 'nil'}) ->
    {node, {NewKey, NewValue, {node, 'nil'}, {node, 'nil'}}};

insert(NewKey, NewValue, {node, {Key, Value, Smaller, Larger}}) ->
    if NewKey < Key -> {node, {Key, Value, insert(NewKey, NewValue, Smaller), Larger}}
     ; NewKey > Key -> {node, {Key, Value, Smaller, insert(NewKey, NewValue, Larger)}}
     ; NewKey =:= Key -> {node, {NewKey, NewValue, Smaller, Larger}}
    end.

lookup(_, {node, nil}) -> undefined;
lookup(Key, {node, {Key, Value, _, _}}) -> {ok, Value};
lookup(SearchKey, {node, {Key, _, Smaller, Larger}}) ->
    if SearchKey < Key -> lookup(SearchKey, Smaller)
     ; SearchKey > Key -> lookup(SearchKey, Larger)
    end.