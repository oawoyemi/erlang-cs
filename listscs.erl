-module(listscs).
-compile(export_all).

binaries() ->
    H = <<"hello">>, W = <<"world">>,
    <<"hello world">> = <<H/binary," ", W/binary>>,
    <<1,2,4>> = << <<X>> || X <- [1,2,3] >>,
    [ X || <<X>> <= <<1,2,3>> ], %[1,2,3]
    [ X || <<X:32/big-signed-integer>> <= <<0,0,0,1,0,0,0,2>>], % [1,2]

% map atom list to escaped binary string
    A = [blah, hey, atom, 'load-b'],
    lists:foldl(fun(X, Acc)->
                        Z = erlang:atom_to_binary(X,utf8),
                        E = <<"\"", Z/binary, "\",">>, <<Acc/binary,E/binary>>
                end,
                <<"[">>, A).

%% more concise
atoms_to_escaped_string2(F) ->
string:join(["\"" ++ atom_to_list(X) ++ "\"" || X <- L], ",").

%% more concise version above
atom_to_escaped_string(L) ->
    atom_to_escaped_string(L, []).

atom_to_escaped_string([], Acc) ->
    lists:reverse(Acc);
atom_to_escaped_string([A], Acc)  ->
    S = ["\"",atom_to_list(A),"\""],
    atom_to_escaped_string([], [S|Acc]);
atom_to_escaped_string([H|T], Acc) ->
    A = ["\"",atom_to_list(H),"\","],
    atom_to_escaped_string(T, [A|Acc]).


%    F = fun(X)-> S = erlang:atom_to_list(X), E = io_lib:format("\"~s\"",[S], Y = list_to_binary(E)) end,
 %   lists:map(F, A).

head_tail() ->
    1 = hd([1,2,3,4]),
    [2,3,4] = tl([1,2,3,4]).

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
list_comps() ->

    [2 * N || N <- [1, 2, 3, 4]].
%% [2,4,6,8]

list_comprehension_filter() ->
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

list_comprehension_multiple_generators() ->
    [X + Y || X <- [1, 2], Y <- [2, 3]].
%%

list_diff() ->
    A = lists:seq(1, 8),
    B = lists:seq(1, 4),
    lists:subtract(A, B),
%% [5,6,7,8]
    A -- B,
%% [5,6,7,8]
    [a, b, c, d] --[a, b, c],
%% [d]
    [a, b, c] -- [a, b, c, d].
%% []

%% linear time operation
list_length() ->
  L = [x,y],
  length(L).
