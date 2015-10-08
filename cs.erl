-module(cs).
-compile(export_all).

references() ->
  Ref = make_ref().
  % source: http://www.erlang.org/course/advanced.html
  % references are unique may be compared for equality,
  % system guarantees that no two references created by different calls to make_ref will ever match
%   ask(Server, Question) ->
%     Ref = make_ref(),
%     Server ! {self(), Ref, Question},
%     receive
%         {Ref, Answer} ->
% 	    Answer
%     end.
%
% server(Data) ->
%     receive
% 	{From, Ref, Question} ->
%             Reply = func(Question, Data),
%             From ! {Ref, Reply},
%             server(Data);
% 	...
%     end.


lists() ->
    lists:seq(1,10),
    lists:map(fun(X) -> X*10 end, [1,2,3]),  %[10,20,30]
    lists:flatmap(fun(X) -> [X*10] end, [1,2,3]).


processes() ->
  spawn(fun()->io:format("~p~n",[2+2]) end).

p1() ->
  % processes
  G = fun(X) ->
      timer:sleep(10),
spawn(fun() -> G(X) end) || X <- lists:seq(1,10).

process_links() ->
  %A link is a specific kind of relationship that can be created between two processes.
  % When that relationship is set up and one of the processes dies from an unexpected throw,
  % error or exit (see Errors and Exceptions),

  %killing processes - if you wanted to kill another process from the shell,
  %                    you could use the function exit/2, which is called this way: exit(Pid, Reason).

  % restarting processes - System processes are basically normal processes,
  %  except they can convert exit signals to regular messages.
  % This is done by calling process_flag(trap_exit, true) in a running process.
  ok.
type_conversion() ->
  %type conversion
  binary_to_atom(<<"Erlang">>, utf8).
% 'Erlang'

infix() ->
  % infix append operator
  [1] ++ [2] ++ [3].
% [1,2,3]

messages() ->
  %When there is no way to match a given message, it is put in a save queue and the next message is tried.
  %If the second message matches, the first message is put back on top of the mailbox to be retried later.
  ok.
