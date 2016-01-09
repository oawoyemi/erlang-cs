-module(cs).
-compile(export_all).

console_print() ->
  io:format("~s~n", ["hello"]),
  io:format("~p~n", [[1, 2, 3]]).
% (==) compares values; the latter (=:=) compares values and types
eq() ->
    _X = 1 =:= 1.0, % false
    1 == 1.0. % true

eq(N) when N == 1 ->
    true.

file() ->
    {ok, _Xml} = file:read_file("x.xml").

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

process_monitor() ->
  erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
% #Ref<0.0.4.29>
%2> flush().
% Shell got {'DOWN',#Ref<0.0.4.29>,process,<0.35.0>,normal}


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
    _BS = erlang:atom_to_binary(atom, utf8), %<<"atom">>
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

  xml_sax() ->
    {ok, Xml} = file:read_file("x.xml"),
    erlsom:parse_sax(Xml, [], fun(Event, Acc) -> io:format("~p~n", [Event]), Acc end).

xml_sax2() ->
    {ok, Xml} = file:read_file("x.xml"),
    PrintTitles1 = fun(Event, _) ->
        case Event of {characters, Title} ->
            io:format("~p~n", [Title]); _ ->
            "" end end,
    CountEntries = fun(Event, Acc) ->
        case Event of {startElement, _, "entry", _, _} ->
            Acc + 1; _ ->
            Acc end end,
    erlsom:parse_sax(Xml, 0, CountEntries),
    erlsom:parse_sax(Xml, 0, PrintTitles1).
