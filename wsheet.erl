-module(wsheet).
-compile(export_all).

%% CaseA = [[1,2,3], [3,4,5]],
%% CaseB = [[1,2,3], [4,5,6]],

test([]) -> [].
test(L = [_]) -> L.
test(L) -> unimplemented.
%%     [X || X <- L].
