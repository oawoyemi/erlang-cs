-module(cs).
-compile(export_all).

console_print() ->
  io:format("~s~n", ["hello"]),
  io:format("~p~n", [[1, 2, 3]]).

processes() ->
  spawn(fun() -> io:format("~p~n", [2 + 2]) end).



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

references() ->
  Ref = make_ref(),
  Ref.
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

anonymous_vars() ->
  %% Variables starting with underscore (_), for example, _Height, are normal variables, not anonymous.
  %% They are however ignored by the compiler in the sense that they do not generate any warnings for unused variables.
  [H | _T] = [1, 2, 3],
  H.
