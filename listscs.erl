-module(listscs).
-compile(export_all).

head_tail() ->
    hd([1,2,3,4]) = 1,
    tl([1,2,3,4]) = [2,3,4].

proplists() ->
%%    They're more of a common pattern that appears when using lists and tuples to represent some object or item
    L = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],
    proplists:get_value(a, L), % 1
    proplists:get_value(b, L), % 2
    proplists:get_value(e, L), % undefined
    proplists:get_value(e, L, undefined). % undefined

list_hof() ->
    lists:seq(1, 10),
    lists:map(fun(X) -> X * 10 end, [1, 2, 3]),  %[10,20,30]
    lists:flatmap(fun(X) -> [X * 10] end, [1, 2, 3]),
    lists:filter(fun(X) -> lists:member(X, [1, 2, 3, 4]) end, [1, 2, 3, 5]).%[1,2,3]

a() ->
    T = [{<<"foo">>, foo}, {<<"bar">>, bar}],
    O = [[{<<"foo">>, foo}, {<<"fum">>, fum}]],
    InTypes = fun(X) -> lists:member(X, T) end,
    lists:filter(fun(R) -> [] == lists:filter(InTypes, R) end, O).


%%   NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN]
comps() ->

    [2 * N || N <- [1, 2, 3, 4]].
%% [2,4,6,8]

comprehension_filter() ->
    [X || X <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], X rem 2 =:= 0],
%% [2,4,6,8,10]
    [X || [X] <- [[1], [2], [3], 4]],
%% [1,2,3]
    [X || [_, X] <- [[1, a], [2, b], [3]], not lists:member(X, [a])],
%% [b]
    %filtering generators
    Weather = [{london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}],
    FoggyPlaces = [X || {X, fog} <- Weather],
    FoggyPlaces.
%% [london,boston]

comprehension_multiple_generators() ->
    [X + Y || X <- [1, 2], Y <- [2, 3]].
%%

subtract() ->
    A = lists:seq(1, 8),
    B = lists:seq(1, 4),
    C = lists:subtract(A, B),
%% [5,6,7,8]
    A -- B,
%% [5,6,7,8]
    [a, b, c, d] --[a, b, c],
%% [d]
    [a, b, c] -- [a, b, c, d].
%% []
