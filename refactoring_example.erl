%%11 Don't allow private data structure to "leak" out of a module

%This is best illustrated by a simple example. We define a simple module called queue - to implement queues:

-module(queue).
-export([add/2, fetch/1]).

add(Item, Q) ->
  lists:append(Q, [Item]).

fetch([H|T]) ->
  {ok, H, T};
fetch([]) ->
  empty.
%This implements a queue as a list, unfortunately to use this the user must know that the queue is represented as a list. A typical program to use this might contain the following code fragment:

NewQ = [], % Don't do this
Queue1 = queue:add(joe, NewQ),
Queue2 = queue:add(mike, Queue1), ....
%This is bad - since the user a) needs to know that the queue is represented as a list and b) the implementer cannot change the internal representation of the queue (this they might want to do later to provide a better version of the module).

Better is:

-module(queue).
-export([new/0, add/2, fetch/1]).

new() ->
  [].

add(Item, Q) ->
  lists:append(Q, [Item]).

fetch([H|T]) ->
  {ok, H, T};
fetch([]) ->
  empty.
Now we can write:

NewQ = queue:new(),
Queue1 = queue:add(joe, NewQ),
Queue2 = queue:add(mike, Queue1), ...
%Which is much better and corrects this problem. Now suppose the user needs to know the length of the queue, they might be tempted to write:

Len = length(Queue) % Don't do this
%since they know that the queue is represented as a list. Again this is bad programming practice and leads to code which is very difficult to maintain and understand. If they need to know the length of the queue then a length function must be added to the module, thus:

-module(queue).
-export([new/0, add/2, fetch/1, len/1]).

new() -> [].

add(Item, Q) ->
  lists:append(Q, [Item]).

fetch([H|T]) ->
  {ok, H, T};

fetch([]) ->
  empty.

len(Q) ->
  length(Q).
Now the user can call queue:len(Queue) instead.

%Here we say that we have "abstracted out" all the details of the queue (the queue is in fact what is called an "abstract data type").

%Why do we go to all this trouble? - the practice of abstracting out internal details of the implementation allows us to change the implementation without changing the code of the modules which call the functions in the module we have changed. So, for example, a better implementation of the queue is as follows:

-module(queue).
-export([new/0, add/2, fetch/1, len/1]).

new() ->
  {[],[]}.

add(Item, {X,Y}) -> % Faster addition of elements
  {[Item|X], Y}.

fetch({X, [H|T]}) ->
  {ok, H, {X,T}};

fetch({[], []) ->
  empty;

fetch({X, []) ->
  % Perform this heavy computation only sometimes.
  fetch({[],lists:reverse(X)}).

len({X,Y}) ->
  length(X) + length(Y).
