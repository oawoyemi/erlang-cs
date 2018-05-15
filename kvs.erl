-module(kvs).

-behavior(gen_server).

-export([start/0, store/2, retrieve/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, #{}, []).

store(K, V) -> gen_server:cast(?MODULE, {store, K, V}).

retrieve(K) -> gen_server:call(?MODULE, {retrieve, K}).

init(#{}) -> {ok, #{}}.

handle_cast({store, K, V}, State) -> {noreply, State#{K => V}}.

handle_call({retrieve, K}, _From, State) -> {reply, maps:get(K, State, notfound), State}.
