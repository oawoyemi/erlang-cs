![enter image description here](https://dl.dropboxusercontent.com/u/811440/screenshot.png)


#Links
1. http://www.erlangpatterns.org/patterns.html
2. The [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html) is a static analysis tool that identifies software discrepancies



#Akka vs Erlang
Erlang does copy-on-send - Akka uses shared memory (immutable objects) for in-VM sends
Erlang does per-process GC - Akka uses JVM GCs
Erlang has OTP - Akka integrates with the entire Java ecosystem (Apache Camel, JAX-RS, etc etc)
Erlang does the process scheduling for you - Akka allows you to use many different Dispatchers with endless configuration opportunities
Erlang does hot code reload - Akka can support it, but it's less flexible because of JVM classloading

##VM
Whenever the VM first starts up, a process called the **application controller** is started (with the name application_controller). It starts all other applications and sits on top of most of them. In fact, you could say the application controller acts a bit like a supervisor for all applications.

##Concurrent vs Functional

# Getting Started
- use binaries inplace of strings
- strings are lists
- atoms atom-atom is not an _atom_ 'atom-atom'
- `list_to_binary(list)`
- pattern matching
- `Foo = 1` is always a pattern match (no assignment)
- processes
	-  `spawn(f)` - creates a new process
	-  process dies when function completes
	-  spawn returns a proccessid for tracking or callbacks?
- tail calls
- anonymous functions vs named functions
- OTP
	- rebar build tool
		- src/foo.app.src
			- foo_app.erl returns a pid for the app
			-
		- ebin/foo/app
	- assets.erl has a start() function
		- application:start(assets)
		- start dependencies recursively
```erlang
% error handling pattern
try
	....
	....
	catch
	class:error ->
		% log error
		erlang:error(Error)
```

---
#Libs
[Erlson](https://github.com/alavrik/erlson) (Erlang Simple Object Notation)  is a dynamic name-value dictionary data type for Erlang. Erlson dictionaries come with a convenient syntax and can be directly converted to and from JSON.

```erlang
Operations = erlson:get_value('operations', Body),
source_json = erlson:from_json(SourceJson),
```
# OTP
The OTP framework is also a set of modules and standards designed to help you build applications.
Erlang OTP is best known for its Actor implementation with Supervision Trees: that provides the semantics for self-healing.

- application
 - When you have written code implementing some specific functionality you might want to make the code into an application, that is, a component that can be started and stopped as a unit, and which can also be reused in other systems.
- **gen_server**
	- On the other hand, if you do expect the process to take a long time before getting a reply and are worried about memory, you can add the **hibernate** atom to the tuple. Hibernation basically reduces the size of the process' state until it gets a message, at the cost of some processing power.

	- `init([])` - If name registration succeeds, the new gen_server process calls the callback function :init([]). init is expected to return `{ok, State}`, where State is the internal state of the gen_server.
	- Asynchronous Requests - Cast

- Supervision
	- Why should every process be supervised? Well the idea is simple: if for some reason you're spawning unsupervised processes, how can you be sure they are gone or not? If you can't measure something, it doesn't exist.

Supervision means that normal requests and responses (including negative ones such as validation errors) flow separately from failures: while the former are exchanged between the user and the service, the latter travel from the service to its supervisor.

- one-for-one
	- . It basically means that if your supervisor supervises many workers and one of them fails, only that one should be restarted. You should use one_for_one whenever the processes being supervised are independent and not really related to each other, or when the process can restart and lose its state without impacting its siblings.
-  rest_for_one
		- What a rest_for_one restarting strategy does, basically, is make it so if a process dies, all the ones that were started after it (depend on it) get restarted, but not the other way around.
- Dynamic Supervision
	 - usually there on a per-demand basis. Think of a web server that would spawn a process per connection it receives. In this case, you would want a dynamic supervisors to look over all the different processes you'll have.
##Takeover
 important concept of distributed OTP applications is the takeover. Taking over is the act of a dead node coming back from the dead, being known to be more important than the backup nodes (maybe it has better hardware), and deciding to run the application again. This is usually done by gracefully terminating the backup application and starting the main one instead.
 t({takeover, _OtherNode}, []) ->
    m8ball_sup:start_link().
The {takeover, OtherNode} argument is passed to start/2 when a more important node takes over a backup(redundancy) node.

>it's sometimes simpler to scale just have many instances running at simultaneously and **synchronising data** rather than forcing an application to run only at a single place. If you need some failover/takeover mechanism, distributed OTP applications might be just what you need.

##Config
- `{env, [{Key, Val}]}`
 - list of key/values that can be used as a configuration for your application. They can be obtained at run time by calling `application:get_env(Key)` or application:get_env(AppName, Key)

Designed for programming real-time control systems. The combination of lightweight isolated processes, asynchronous message passing with pattern matching, and controlled error propagation has been proven to be very effective. One of our main contributions lies in the integration of Erlang’s programming model into a full-fledged object-oriented and functional language.


Erlang's easy to learn. It is not “Object Oriented,” it does not have “Mutable state,” and it’s a “Functional programming Language.”

It’s really a very small and simple language.

You might be wondering what Erlang code looks like.
Erlang makes heavy use of a pattern matching syntax; here’s a small example of Erlang code (from the new Erlang book):

#Testing
https://github.com/zkessin/testing-erlang-book

Then we have the function all/0. That function returns a list of test cases.
It's basically what tells **Common Test** "hey, I want to run these test cases!".
EUnit would do it based on the name (*_test() or *_test_()).
```./rebar ct -suite <Name>.```

##eunit

- ?assertMatch(Pattern, Expression)
	- This allows us to match in a form similar to Pattern = Expression, without variables ever binding.
- ?assertEqual(A, B)
	- Does a strict comparison (equivalent to `=:=`) between two expressions, A and B.
```erlang
%fixtures
%The foreach fixture will then take each of the instantiators and run the setup and teardown function for each of them.
ome_test2_() ->
    {foreach
     fun start/0,
     fun stop/1,
     [fun some_instantiator1/1,
      fun some_instantiator2/1,
      ...
	  fun some_instantiatorN/1]}.

%Test Descriptions
%You can give descriptions of tests in a neat way. Check this out:
%Nice, huh? You can wrap a fixture by doing {Comment, Fixture} in order to get readable tests.
double_register_test_() ->
    {"Verifies that the registry doesn't allow a single process to "
     "be registered under two names. We assume that each pid has the "
     "exclusive right to only one name",
     {setup,
      fun start/0,
      fun stop/1,
      fun two_names_one_pid/1}}.

```

When you run the tests, Common Test will find some place to log stuff (usually the current directory, but we'll see how to configure it later). When doing so, it will create a unique directory where you can store your data. That directory (Priv Dir above), along with the data directory, will be passed as part of some initial state to each of your tests. You're then free to write whatever you want in that private directory, and then inspect it later, without running the risk of overwriting something important or the results of former test runs

shuffle
Runs the test in a random order. The random seed (the initialization value) used for the sequence will be printed in the HTML logs, of the form {A,B,C}.

to integrate EUnit tests to a Common Test suite, all you need to do is have a function a bit like this:

run_eunit(_Config) ->
    ok = eunit:test(TestsToRun).
And all your EUnit tests that can be fo

**parallel tests**
groups() -> [{clients,
              [parallel, {repeat, 10}],
              [carla, mark, dog]}].
This creates a clients group of tests, with the individual tests being carla, mark, and dog. They're going to run in parallel, 10 times each.

# Language Features
##Mnesia
Mnesia is based on records and tables (ETS and DETS). To be exact, you can define an Erlang record and tell Mnesia to turn its definition into a table. Basically, if we decided to have our record take the form:

-record(recipe, {name, ingredients=[], instructions=[]}).
We can then tell Mnesia to create a recipe table, which would store any number of #recipe{} records as table rows. I could thus have a recipe for pizza noted as:

```erlang
#recipe{name=pizza,
        ingredients=[sauce,tomatoes,meat,dough],
        instructions=["order by phone"]}
```
**c_copies**
This option means that the data is stored both in ETS and on disk, so both memory and the hard disk. disc_copies tables are not limited by DETS limits, as Mnesia uses a complex system of transaction logs and checkpoints that allow to create a disk-based backup of the table in memory.

Mnesia tables let you have indexes on top of the basic ETS and DETS functionality. This is useful in cases where you are planning to build searches on record fields other than the primary key.

When Mnesia queries get to be complex, match specifications are usually going to be part of your solution. They let you run basic Erlang functions and they thus prove invaluable when it comes to specific result generation.

Query List Comprehensions are basically a compiler trick using parse transforms that let you use list comprehensions for any data structure that can be searched and iterated through. They're implemented for Mnesia, DETS, and ETS, but can also be implemented for things like gb_trees.

Once you add `-include_lib("stdlib/include/qlc.hrl"). `
#Type Declaration
The notation to represent the union of types is the pipe (|). Basically, this lets us say that a given type TypeName is represented as the union of `Type1 | Type2 | ... | TypeN.` As such, the `number()` type, which includes integers and floating point values, could be represented as `integer() | float().` A boolean value could be defined as `'true' | 'false'.` It is also possible to define types where only one other type is used. Although it looks like a union type, it is in fact an alias.
Syntax for type declaration in a module is:
```erlang
-type TypeName() :: TypeDefinition.
%%As such, our tree could have been defined as:

-type tree() :: {'node', tree(), tree(), any(), any()}.
```
Or, by using a special syntax that allows to use variable names as type comments:
```erlang
-type tree() :: {'node', Left::tree(), Right::tree(), Key::any(), Value::any()}.
```
**Type signatures** are of the form `-spec FunctionName(ArgumentTypes) -> ReturnTypes..` In the specification above we say that the kind/1 function accepts cards as arguments, according to the card() type we created. It also says the function either returns the atom face or number.
```erlang
-spec convert(list() | tuple()) -> list() | tuple().
-type food(A) :: fun(() -> A).
```




#Dialyzer
Dialyzer's main goal is to make uncover the implicit type information in Erlang code and make it explicitly available in programs.
Dialyzer is optimistic. It has figurative faith in your code, and because there is the possibility that the function call to convert/1 succeeds at some point, Dialyzer will keep silent.

Dialyzer can deal with unknown functions when looking for type errors. We'll see why when we get to discuss how its **type inference** algorithm works.
Dialyzer's type system thus made the decision not to prove that a program is error-free when it comes to types, but only to find as many errors as possible.⧸⧸##Process Dictionary
Each process has a local store called the "Process Dictionary". The following BIFs are used to manipulate the process dictionary:

- get() returns the entire process dictionary.
- get(Key) returns the item associated with Key (Key is any Erlang data structure), or, returns the special atom undefined if no value is associated with Key.
- put(Key, Value) associate Value with Key. Returns the old value associated with Key, or, undefined if no such association exists.
- erase() erases the entire process dictionary. Returns the entire process diction before it was erased.
- erase(Key) erases the value associated with Key. Returns the old value associated with Key, or, undefined if no such association exists.
- get_keys(Value) returns a list of all keys whose associated value is Value.
Note that using the Process Dictionary:
- Destroys referencial transparency
- Makes debugging difficult
- Survives Catch/Throw

So:
- Use with care
- Do not over use - try the clean version first
⧸⧸
##Macros

```erlang
-ifdef(Macro). %Evaluate the following lines only if Macro is defined.
-ifndef(Macro).%Evaluate the following lines only if Macro is not defined.
```

## ets
This module is an interface to the Erlang built-in term storage BIFs. These provide the ability to store very large quantities of data in an Erlang runtime system, and to have **constant access time** to the data. (In the case of ordered_set, see below, access time is proportional to the logarithm of the number of objects stored).

 > ETS table (in-memory database table, native to the VM.)

Process State - If we have a lot of message passing going on, process risks getting busier and busier, and if the demand is high enough our whole system will become sequential and slow.

The few ways we'd have to get around that would be to either split the process into subprocesses to make lookups faster by sharding the data, or find a way to store the data in some database that will allow for parallel and concurrent access of the data. While the first way to do it would be very interesting to explore, we'll go through an easier path by doing the latter - ETS.

 (functional data structures usually tend to flirt with logarithmic access time) and to have such storage look as if it were implemented as processes in order to keep their use simple and idiomatic.
 By default, only the owner of the table can write to it, but everyone can read from it. This is known as the protected level of permissions
If the process dies, the table disappears (and so does all of its content).

* Because ETS has no transactions whatsoever, all unsafe operations should be performed by the process that owns the table.
	* The safe ones should be allowed to be public, done outside of the owner process.

- `{write_concurrency, true | false}`
Usually, writing to a table will lock the whole thing and nobody else can access it, either for reading or writing to it, until the write is done. Setting this option to 'true' lets both reads and writes be done concurrently, without affecting the ACID properties of ETS.

As you may (and should) recall, ETS tables work by storing tuples. The Position parameter holds an integer from 1 to N telling which of each tuple's element shall act as the primary key of the database table. The default key position is set to 1.

##Data types
###Atoms
There is a reason why variables names can't begin with a lowercase character: atoms.
  - Atoms are literals, constants with their own name for value.
  - An atom should be enclosed in single quotes (') if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore _ , or @.
  - atoms should not be generated dynamically for whatever reason,  The atom table is not garbage collected, and so atoms will accumulate until the system tips over (8 bytes/atom in a 64 bit system)
  -
###Strings
There is no such thing as a real string in Erlang!

###Atoms vs Strings
Do not use atoms to replace strings because they are lighter. Strings can be manipulated (splitting, regular expressions, etc) while atoms can only be compared and nothing else.


###orddicts/dicts/gbtrees/records
If you do want a more complete key-value store for small amounts of data, the orddict module is what you need. Orddicts (ordered dictionaries) are proplists with a taste for formality. Each key can be there once, the whole list is sorted for faster average lookup, etc.
* dicts
Dicts are thus very good choices to scale orddicts up whenever it is needed.
Orddicts are a generally good compromise between complexity and efficiency up to about 75 elements.

Starting with version 17.0, the language supports a new native key-value data type, described in ```Postscript: Maps```.  They should be the new de-facto replacement for dicts.

## Error handling
You can also manually get a stack trace by calling
```erlang
 erlang:get_stacktrace/0
```
# Conditionals
'Else' or 'true' branches should be "avoided" altogether: ifs are usually easier to read when you cover all logical ends rather than relying on a "catch all" clause.
replace:
```erlang
    % bad practice      % more idiomatic
	if X > Y -> a()		if X > Y  -> a()
	 ; true  -> b()		 ; X =< Y -> b()
	end		     	    end

	if X > Y -> a()		if X > Y -> a()
	 ; X < Y -> b()		 ; X < Y -> b()
	 ; true  -> c()		 ; X ==Y -> c()
	end			        end
```
## Functions/Anonymous functions
* formatting - io:format's formatting is done with the help of tokens - `io:format("~s!~n",["Hello"]).`

* Anonymous functions
You're most likely to use anonymous functions to carry state around when you have functions defined that take many arguments, but you have a constant one:
##Modules
avoid circular dependencies!
* Module metadata
```erlang
1>useless:module_info().
[{module,useless},
 {exports,[{add,2},
           {hello,0},
           {greet_and_add_two,1},
           ...
           ....
```
## Invariant variables/Unbound variables
 - note that variables begin with an uppercase letter
* Unbound variables
 First, X and Y had no value and were thus considered unbound variables
##Processes
Erlang forces you to write actors (processes) that will share no information with other bits of code unless they pass messages to each other.
    - Every communication is explicit, traceable and safe.

## Macros
They are simple expressions represented by text that will be replaced before the code is compiled for the VM.
Such macros are mainly useful to avoid having magic values floating around your modules.
A macro is defined as a module attribute of the form: ```-define(MACRO, some_value)```. and is used as ```?MACRO``` inside any function defined in the module. A 'function' macro could be written as ```-define(sub(X,Y), X-Y).``` and used like ```?sub(23,47)```, later replaced by 23-47 by the compiler.

- ***Guard expressions*** - Note that a basic rule for guard expression is they must return true to succeed.
    - The comma (,) acts in a similar manner to the operator and also and the semicolon (;) acts a bit like or else
- One negative point about guards is that they will not accept user-defined functions because of side effects. Erlang is not a purely functional programming language (like Haskell is) because it relies on side effects

## BIFs
built-in functions (BIFs) are usually functions that could not be implemented in pure Erlang, and as such are defined in C, or whichever language Erlang happens to be implemented on (it was Prolog in the 80's). There are still some BIFs that could be done in Erlang but were still implemented in C in order to provide more speed to common operations.

##Lists
The | we used is named the cons operator (constructor). In fact, any list can be built with only cons and values:
#### List Comprehension
The recipe for list comprehensions in Erlang is therefore:
     ` NewList = [Expression || Pattern <- List, Condition1, Condition2…]`

  - The part ```Pattern <- List``` is named a Generator expression which may have a cardinality of 0..N.
- Modules are a bunch of functions regrouped in a single file, under a single name. Additionally, all functions in Erlang must be defined in **modules**.
```
 List = [2,3,4].
80> [Head|Tail] = List.
[1,2,3,4]
81> Head.
1
82> Tail.
[2,3,4]
```
The | we used is named the cons operator (constructor). In fact, any list can be built with only cons and values:
```
21> [2 | [1 | []]].
[2,1]
```

##Best practice
### Don't allow private data structure to "leak" out of a module

This is best illustrated by a simple example. We define a simple module called queue - to implement queues:
```erlang
-module(queue).
-export([add/2, fetch/1]).

add(Item, Q) ->
  lists:append(Q, [Item]).

fetch([H|T]) ->
  {ok, H, T};
fetch([]) ->
  empty.
```

This implements a queue as a list, unfortunately to use this the user must know that the queue is represented as a list. A typical program to use this might contain the following code fragment:
```erlang
NewQ = [], % Don't do this
Queue1 = queue:add(joe, NewQ),
Queue2 = queue:add(mike, Queue1), ....
```
This is bad - since the user a) needs to know that the queue is represented as a list and b) the implementer cannot change the internal representation of the queue (this they might want to do later to provide a better version of the module).

Better is:
```erlang
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
```

Now we can write:
```
NewQ = queue:new(),
Queue1 = queue:add(joe, NewQ),
Queue2 = queue:add(mike, Queue1), ...
```

Which is much better and corrects this problem. Now suppose the user needs to know the length of the queue, they might be tempted to write:
```erlang
Len = length(Queue) % Don't do this
```
since they know that the queue is represented as a list. Again this is bad programming practice and leads to code which is very difficult to maintain and understand. If they need to know the length of the queue then a length function must be added to the module, thus:
```erlang
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
```
Now the user can call `queue:len(Queue) `instead.

Here we say that we have "abstracted out" all the details of the queue (the queue is in fact what is called an "abstract data type").

Why do we go to all this trouble? - the practice of abstracting out internal details of the implementation allows us to change the implementation without changing the code of the modules which call the functions in the module we have changed. So, for example, a better implementation of the queue is as follows:

```erlang
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
```
## Shell
```f(variable).``` erase variable
```f().``` erase all session variables

kerl list releases
kerl build 18.1 18.1


erlang:system_info(otp_release).
