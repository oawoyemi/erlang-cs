-module(eefberl_tests).

-include_lib("eunit/include/eunit.hrl").

fizzbuzz_test_() ->
    [{setup,
     fun start/0,
     fun(R) ->
             [{"Return fizz for multiples of 3", multiple_3(R)},
              {"Return buzz for multiples of 5", multiple_5(R)},
              {"Return fizzbuzz for multiples of 15",  multiple_15(R)},
              {"Return the original number if not multiple of 3, 5 or 15",  original_number(R)}]
     end},
     {"Throw an error if not a valid range", invalid_range_error()},
     {"Throws an error if invalid type", invalid_type_error()}].

start() ->
    eefberl:fizzbuzz(1,45).

multiple_3(R) ->
    [?_assertEqual(lists:nth(X, R), "fizz") || X <- [3, 6, 9]].

multiple_5(R) ->
    [?_assertEqual(lists:nth(X, R), "buzz") || X <- [5, 10, 20]].

multiple_15(R) ->
    [?_assertEqual(lists:nth(X, R), "fizzbuzz") || X <- [15, 30, 45]].

original_number(R) ->
    [?_assertEqual(lists:nth(X, R), X) || X <- [1, 2, 4]].

invalid_type_error() ->
    ?_assertException(error, function_clause, eefberl:fizzbuzz(a,c)).

invalid_range_error() ->
    ?_assertException(error, function_clause, eefberl:fizzbuzz(1,1)).
