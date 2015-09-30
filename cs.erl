-module(cs).
-compile(export_all).

ls() ->
    lists:seq(1,10).

p() ->
  spawn(fun()->io:format("~p~n",[2+2]) end).

p2() ->
  G = fun(X) -> timer:sleep(10), io:format("~p~n", [X]) end,
  [spawn(fun() -> G(X) end) || X <- lists:seq(1,10)].
