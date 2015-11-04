-module(cs).
-compile(export_all).

file() -> {ok ,Xml} = file:read_file("x.xml").
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

process_links() ->
  %A link is a specific kind of relationship that can be created between two processes.
  % When that relationship is set up and one of the processes dies from an unexpected throw,
  % error or exit (see Errors and Exceptions),
  ok.
process_monitor() ->
  erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
% #Ref<0.0.4.29>
%2> flush().
% Shell got {'DOWN',#Ref<0.0.4.29>,process,<0.35.0>,normal}
tc() ->
  %type conversion
  binary_to_atom(<<"Erlang">>, utf8).
% 'Erlang'

in() ->
  % infix append operator
  [1] ++ [2] ++ [3].
% [1,2,3]

messages() ->
  %When there is no way to match a given message, it is put in a save queue and the next message is tried.
  %If the second message matches, the first message is put back on top of the mailbox to be retried later.
  ok.
