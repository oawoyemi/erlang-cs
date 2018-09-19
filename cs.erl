-module(cs).
-compile(export_all).
%% https://gist.github.com/aabs/45518ff1c30b51bdf9dca428136e0f56

let_it_crash() ->
%% Here we have written down the intention that the file might not exist. However
%% We only worry about non existence.
%% We crash on enoent which means an access error due to permissions.
%% Likewise for eisdir, enotdir, enospc.
    case file:open("filetest.txt", [raw, read, binary]) of
        {ok, _Fd} -> ok;
        {error, enoent} -> error
    end.

%% tests
%% https://github.com/richcarl/eunit/blob/master/examples/eunit_examples.erl


%% Advanced info http://www.erlang.org/course/advanced.html#dict


error_m([], Arg) ->
                   Arg;
error_m([Fun | Funs], Arg) ->
    case Fun(Arg) of
        {ok, V} ->
            error_m(Funs, V);
        {error, E} ->
            {error, E}
    end.

%% functional composition
compose(F,G) -> fun(X) -> F(G(X)) end.

multicompose(Fs) ->
    lists:foldl(fun compose/2, fun(X) -> X end, Fs).

write_http_body_to_file() ->
{ok, {_Status, _Headers, Body}} = httpc:request("http://testing.com/123"),
Data = list_to_binary(Body),
file:write_file("testing123.xml", Data).

case_expression_guards(X) ->
    case X of
        {answer, N} when N =:= 42 -> true;
        _ -> false
    end.

%% ets - ETS tables store tuples, with access to the elements given through a key field in the tuple.
%% default setup when an empty list of options is passed to the ets:new/2 function is to create a set,
%% with the key in position 1, and providing protected access
etstables() ->

TabId = ets:new(tab,[named_table]),
    ets:tab2list(TabId),%% return all rows as list


true = ets:delete(TabId),

ets:info(TabId),

ets:insert(tab,{haskell, lazy}),

%%tab
ets:lookup(tab,haskell),

%% [{haskell,lazy}]
ets:insert(tab,{haskell, ghci}),

%% true
ets:lookup(tab,haskell),

%% [{haskell,ghci}]
ets:lookup(tab,racket),

%% []

%% the wildcard matches a value but it's not reported in the result
ets:match(tab,{'$1','$0'}), %% [[strict,ocaml],[ghci,haskell],[strict,racket]]

ets:match(tab,{'$1','_'}), %% [[ocaml],[haskell],[racket]]
ets:match(tab, {strict, '$1'}), %%[[ocaml,racket]]

ets:delete(tab, ocaml).

%% retry with backoff
retry(F, MaxTries, Backoff) ->
    retry(F, MaxTries, Backoff, F()).

retry(_F, 0, _Backoff, _R) ->
    fail;
retry(_F, _Count, _Backoff, {V, ok}) ->
    V;
retry(F, Count, Backoff, _R) ->
    timer:sleep(Backoff),
    retry(F, Count - 1, Backoff,  F()).


try_catch(X) ->
    try
        string:to_integer(X)
    catch
        _:_ ->
            X
    end.

io() ->
    io:format("~s~n", ["hello"]),
    io:format("~p~n", [[1, 2, 3]]).

equality() ->
% (==) compares values; the latter (=:=) compares values and types
% `andalso` lazily evaluates the expression
_X = 1 =:= 1.0, % false
1 == 1.0, % true
1 =:= 1 andalso 2 =:= 2.

eq(N) when N == 1 ->
    true.

file_read() ->
    {ok, _Xml} = file:read_file("x.xml").

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



%% Variables starting with underscore (_), for example, _Height, are normal variables.
%% They are however ignored by the compiler in the sense that they do not generate any warnings for unused variables.
[H | _T] = [1, 2, 3],
H.



%% dicts
D = lists:foldl(fun(V, Dict) -> Dict:append(a, V) end, dict:new(), [1,2,3]).
dict:to_list(D).
%%> [{a,[1,2,3]}]
D0 = lists:foldl(fun(V, Dict) -> Dict:append(b, V) end, D, [1,2,3]).
dict:to_list(D0).
%%> [{a,[1,2,3]},{b,[1,2,3]}]

%% more concise
atoms_to_escaped_string2(F) ->
string:join(["\"" ++ atom_to_list(X) ++ "\"" || X <- L], ",").

%% more concise version above
atom_to_escaped_string(L) ->
    atom_to_escaped_string(L, []).

atom_to_escaped_string([], Acc) ->
    lists:reverse(Acc);
atom_to_escaped_string([A], Acc)  ->
    S = ["\"",atom_to_list(A),"\""],
    atom_to_escaped_string([], [S|Acc]);
atom_to_escaped_string([H|T], Acc) ->
    A = ["\"",atom_to_list(H),"\","],
    atom_to_escaped_string(T, [A|Acc]).


%    F = fun(X)-> S = erlang:atom_to_list(X), E = io_lib:format("\"~s\"",[S], Y = list_to_binary(E)) end,
 %   lists:map(F, A).

head_tail() ->
    1 = hd([1,2,3,4]),
    [2,3,4] = tl([1,2,3,4]).


list_hof() ->

    lists:filter(fun(X) -> lists:member(X, [1, 2, 3, 4]) end, [1, 2, 3, 5]).%[1,2,3]

a() ->
    T = [{<<"foo">>, foo}, {<<"bar">>, bar}],
    O = [[{<<"foo">>, foo}, {<<"fum">>, fum}]],
    InTypes = fun(X) -> lists:member(X, T) end,
    lists:filter(fun(R) -> [] == lists:filter(InTypes, R) end, O).


%%   NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN]
comps() ->
    [2 * N || N <- [1, 2, 3, 4]].
%% [2,4,6,8]

comprehension_filter() ->
    [X || X <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], X rem 2 =:= 0],
%% [2,4,6,8,10]
    [X || [X] <- [[1], [2], [3], 4]],
%% [1,2,3]
    [X || [_, X] <- [[1, a], [2, b], [3]], not lists:member(X, [a])],
%% [b]
    %filtering generators
    Weather = [{london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}],
    FoggyPlaces = [X || {X, fog} <- Weather],
    FoggyPlaces.
%% [london,boston]

comprehension_multiple_generators() ->
    [X + Y || X <- [1, 2], Y <- [2, 3]].
%%

subtract() ->
    A = lists:seq(1, 8),
    B = lists:seq(1, 4),
    lists:subtract(A, B),
%% [5,6,7,8]
    A -- B,
%% [5,6,7,8]
    [a, b, c, d] --[a, b, c],
%% [d]
    [a, b, c] -- [a, b, c, d].

binary_processing() ->
    H = <<"hello">>, W = <<"world">>,
    <<"hello world">> = <<H/binary," ", W/binary>>,
    <<1,2,4>> = << <<X>> || X <- [1,2,3] >>,
    [ X || <<X>> <= <<1,2,3>> ], %[1,2,3]
    [ X || <<X:32/big-signed-integer>> <= <<0,0,0,1,0,0,0,2>>], % [1,2]

 %% extract the first 32 bits from the binary, complete matching statement:
<<B1:32, _/binary>> = B.

% map atom list to escaped binary string
    A = [blah, hey, atom, 'load-b'],
    lists:foldl(fun(X, Acc)->
                        E = <<"\"", (atom_to_binary(X, utf8))/binary, "\",">>, <<Acc/binary,E/binary>>
                end,
                <<"[">>, A).

 binaryToChars(L) ->
     _C = [X || <<X:1/binary>> <= L].

concat_binary() ->
    list_to_binary([<<"foo">>, <<"bar">>]).


%%nested list comp
L = lists:seq(1, 10).
[Y || Y <- [X*2 || X <- L], Y<10]. %% [2,4,6,8]

%% higher order functions
lists:seq(1, 10).
lists:map(fun(X) -> [X,X,1,1] end, [1, 2, 3]). %[[1,1,1,1],[2,2,1,1],[3,3,1,1]]
lists:flatmap(fun(X) -> [X,X,1,1] end, [1, 2, 3]). %[1,1,1,1,2,2,1,1,3,3,1,1]

% update or replace key
lists:keystore(<<"blah">>, 1, E, {<<"blah">>,x}).
%%[{<<"timestamp">>,123},
 %%{<<"reported_by">>,<<"source">>},
 %%{<<"details">>,<<"status is great">>},
 %%{<<"blah">>,x}]

%% partition lists into sublists and a remainder by key
proplists:split([{a,1}, {b, 5}, {c,1}, {d,6}], [a, b]). %{[[{a,1}],[{b,5}]],[{c,1},{d,6}]}

%% merge 2 proplists
merge_proplists(OldPlist, NewPlist) ->
    orddict:merge(fun(_K, _Old, New) -> New end,
                  orddict:from_list(OldPlist),
                  orddict:from_list(NewPlist)).
proplists() ->
%%    They're more of a common pattern that appears when using lists and tuples to represent some object or item
    L = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],
    proplists:get_value(a, L), % 1
    proplists:get_value(b, L), % 2
    proplists:get_value(e, L), % undefined
    proplists:get_value(e, L, undefined). % undefined
