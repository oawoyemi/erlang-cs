-module(eefberl).

-export([fizzbuzz/2]).

fizzbuzz(Start, End) when Start < End,
                          is_integer(Start),
                          is_integer(End) ->
    L = lists:seq(Start, End),
    R = lists:reverse(L),
    fizzbuzz(R, []);
fizzbuzz([H|T], Acc) when is_integer(H), H rem 15 == 0 ->
    fizzbuzz(T, ["fizzbuzz"|Acc]);
fizzbuzz([H|T], Acc) when is_integer(H), H rem 3 == 0 ->
    fizzbuzz(T, ["fizz"|Acc]);
fizzbuzz([H|T], Acc) when is_integer(H), H rem 5 == 0 ->
    fizzbuzz(T, ["buzz"|Acc]);
fizzbuzz([H|T], Acc) when is_integer(H) ->
    fizzbuzz(T, [H|Acc]);
fizzbuzz([], Acc) ->
    Acc.
