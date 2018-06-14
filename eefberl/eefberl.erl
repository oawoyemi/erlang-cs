-module(eefberl).

-export([fizzbuzz/2, has3Digit/1]).

fizzbuzz(Start, End)
    when Start < End, is_integer(Start), is_integer(End) ->
    L = lists:seq(Start, End),
    R = lists:reverse(L),
    fizzbuzz(R, [], true).

fizzbuzz(L = [H | T], Acc, _match = true)
    when is_integer(H) ->
    case has3Digit(H) of
      true -> fizzbuzz(T, ["lucky" | Acc], true);
      _ -> fizzbuzz(L, Acc, false)
    end;
fizzbuzz([H | T], Acc, _match = false)
    when is_integer(H), H rem 15 == 0 ->
    fizzbuzz(T, ["fizzbuzz" | Acc], true);
fizzbuzz([H | T], Acc, _match = false)
    when is_integer(H), H rem 3 == 0 ->
    fizzbuzz(T, ["fizz" | Acc], true);
fizzbuzz([H | T], Acc, false)
    when is_integer(H), H rem 5 == 0 ->
    fizzbuzz(T, ["buzz" | Acc], true);
fizzbuzz([H | T], Acc, false) when is_integer(H) ->
    fizzbuzz(T, [H | Acc], true);
fizzbuzz([], Acc, _match) -> Acc.

has3Digit(N) when is_integer(N), N < 10 -> N == 3;
has3Digit(N) when is_integer(N) ->
    case N rem 10 of
      3 -> true;
      _ -> has3Digit(N div 10)
    end.
