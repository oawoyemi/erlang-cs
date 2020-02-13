% -module(factorialtr).

% -export([calc/1]).

% calc(X) ->
%     calc(X, 0).

% calc(X, Acc) when X == 0 ->
%     Acc + 1;
% calc(X, Acc) when X > 0 ->
%     X * calc(X-1, Acc).

-module(factorialtr).

-export([fac/1]).

fac(0) -> 1;
fac(1) -> 1;
fac(N) when N > 1 -> go(N, 1).

go(0, Acc) -> Acc;
go(N, Acc) ->  go(N-1, N*Acc).