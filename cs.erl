%% rebar3 - rebar3 getting started
%% rebar3 new app <app_name>
%% add hex plugin: {plugins, [rebar3_hex]}.
%% rebar3 update
%% rebar3 shell
application:start(<app_name>).
application:stop(<app_name>).

%% tests
https://github.com/richcarl/eunit/blob/master/examples/eunit_examples.erl

%% processes

%%registered/named processes for a node erlang:processes() %%list all Pids on a node
rp(registered()).


%% Advanced info http://www.erlang.org/course/advanced.html#dict
%% returns a list of all processes currently know to the
processes()
system.process_info(Pid) %% returns a dictionary containing information about
Pid.Module:module_info() %% returns a dic tionary containing information about
%% the code in module Module.


%% monadish
error_m([], Arg) ->
    Arg;
error_m([Fun | Funs], Arg) ->
    case Fun(Arg) of
        {ok, V} ->
            error_monad(Funs, V);
        {error, E} ->
            {error, E}
    end.

%% functional composition
compose(F,G) -> fun(X) -> F(G(X)) end.

multicompose(Fs) ->
    lists:foldl(fun compose/2, fun(X) -> X end, Fs).

%% write http body to file
{ok, {_Status, _Headers, Body}} = httpc:request("http://testing.com/123"),
Data = list_to_binary(Body),
file:write_file("testing123.xml", Data),

%% case expression guards
case X of
   {answer, N} when N =:= 42 -> true;
   _ -> false
 end.

%% ets - ETS tables store tuples, with access to the elements given through a key field in the tuple.
%% default setup when an empty list of options is passed to the ets:new/2 function is to create a set,
%% with the key in position 1, and providing protected access
ets:info(TabId).
true = ets:delete(TabId).
ets:tab2list(TabId)%% return all rows as list

TabId = ets:new(tab,[named_table]).

ets:insert(tab,{haskell, lazy}).
%%tab
ets:lookup(tab,haskell).
%% [{haskell,lazy}]
ets:insert(tab,{haskell, ghci}).
%% true
ets:lookup(tab,haskell).
%% [{haskell,ghci}]
ets:lookup(tab,racket)
%% []

%% the wildcard matches a value but it's not reported in the result
ets:match(tab,{'$1','$0'}). %% [[strict,ocaml],[ghci,haskell],[strict,racket]]
ets:match(tab,{'$1','_'}). %% [[ocaml],[haskell],[racket]]
 ets:match(tab, {strict, '$1'}). %%[[ocaml,racket]]
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

io:format("~s~n", ["hello"]),
io:format("~p~n", [[1, 2, 3]]).
% (==) compares values; the latter (=:=) compares values and types
% `andalso` lazily evaluates the expression
_X = 1 =:= 1.0, % false
1 == 1.0. % true
1 =:= 1 andalso 2 =:= 2.

eq(N) when N == 1 ->
    true.

file() ->
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
