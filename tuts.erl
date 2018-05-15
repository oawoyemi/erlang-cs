-module(tuts).
-export([factorial/1, sum/1, sumtr/1, factorialtr/1, sublist/2]).

%% recursive factorial
factorial(X) when X == 0 ->
    1;
factorial(X) when X > 0 ->
    X * factorial(X-1).

factorialtr(X) ->
    factorialtr(X, 0).

%% tail recursive factorial
factorialtr(X, Acc) when X == 0 ->
    Acc + 1;
factorialtr(X, Acc)when X > 0 ->
    X * factorialtr(X-1, Acc).

%% sum recursive
sum([]) ->
    0;
sum([H|T]) ->
    H + sum(T).

%% tail rec sum
sumtr(X) ->
    sumtr(X, 0).

%% base case
sumtr([], Acc) ->
    Acc;
%%converge to base case
sumtr([H|T], Acc) ->
    sumtr(T, Acc + H).

sublist(L, N) ->
    lists:reverse(sublist(L, N, [])).

sublist([], _N, Acc) ->
    Acc;
sublist(_L, 0, Acc) ->
    Acc;
sublist([H|T], N, Acc) ->
    sublist(T, N-1, [H|Acc]).
