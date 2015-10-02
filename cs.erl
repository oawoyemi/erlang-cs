-module(cs).
-compile(export_all).

s() ->
    lists:seq(1,10).

m() ->
  lists:map(fun(X) -> X*10 end, [1,2,3]).
  %[10,20,30]

fm() ->
  lists:flatmap(fun(X) -> [X*10] end, [1,2,3]).

p() ->
  spawn(fun()->io:format("~p~n",[2+2]) end).

p1() ->
  % processes
  G = fun(X) ->
      timer:sleep(10), io:format("~p~n", [X]) end,
  [spawn(fun() -> G(X) end) || X <- lists:seq(1,10)].

tc() ->
  %type conversion
  binary_to_atom(<<"Erlang">>, utf8).
% 'Erlang'

in() ->
  % infix append operator
  [1] ++ [2] ++ [3].
% [1,2,3]
