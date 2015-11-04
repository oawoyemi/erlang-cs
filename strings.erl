-module(strings).

-compile(export_all).

equality() ->
  string:equal("ABC", "abc").

binaryToChars(L) ->
  C = [X || <<X:1/binary>> <= L].

concat_binary() ->
    list_to_binary([<<"foo">>, <<"bar">>]).
