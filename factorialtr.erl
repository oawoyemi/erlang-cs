-module(factorialtr).

-export([calc/1]).

calc(X) ->
    calc(X, 0).

calc(X, Acc) when X == 0 ->
    Acc + 1;
calc(X, Acc) when X > 0 ->
    X * calc(X-1, Acc).
