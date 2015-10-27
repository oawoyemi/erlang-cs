-module(cs).
-compile(export_all).

stringEquality() ->
  string:equal("ABC", "abc").

string_binaryToChars(L) ->
  C = [X || <<X:1/binary>> <= L].


console_print() ->
  io:format("~s~n", ["hello"]),
  io:format("~p~n", [[1, 2, 3]]).

concat_bin_strings() ->
  list_to_binary([<<"foo">>, <<"bar">>]).

lists() ->
  lists:seq(1, 10),
  lists:map(fun(X) -> X * 10 end, [1, 2, 3]),  %[10,20,30]
  lists:flatmap(fun(X) -> [X * 10] end, [1, 2, 3]).

list_comps1() ->
%%   NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN]

  [2 * N || N <- [1, 2, 3, 4]].
%% [2,4,6,8]

list_comp_filter() ->
  [X || X <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], X rem 2 =:= 0].
%% [2,4,6,8,10]

list_comp_filter2() ->
  %filtering generators
  Weather = [{london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}],
  FoggyPlaces = [X || {X, fog} <- Weather],
  FoggyPlaces.
%% [london,boston]

list_comp_multiple_generators() ->
  [X + Y || X <- [1, 2], Y <- [2, 3]].
%%

list_subtract() ->
  A = lists:seq(1, 8),
  B = lists:seq(1, 4),
  C = lists:subtract(A, B),
%% [5,6,7,8]
  A -- B,
%% [5,6,7,8]
  [a, b, c, d] --[a, b, c],
%% [d]
  [a, b, c] -- [a, b, c, d].
%% []


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
