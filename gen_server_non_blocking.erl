call(Name, Tr_id, Request) ->
    gen_server:call(Name, {request, Tr_id, Request}, ?timeout).
...
%% Notice that the handle_call/3 function returns {noreply, State}. The calling worker process is left waiting, 
%%but this process is immediately ready to handle the next message.
handle_call({request, Tr_id, Req}, From, State) ->
    Data = encode(Tr_id, Req),
    gen_tcp:send(State#state.socket, Data),
    ets:store(State#state.ets, {Tr_id, From}),
    {noreply, State};
    
%%The worker process simply waits until a response with the matching transaction id arrives from the back end server. 
%% The original 'From' reference is retrieved from the ets table based on the transaction id, and the gen_server call protocol 
%% is fulfilled with the gen_server:reply(From, Result) call.
handle_info({tcp, _Socket, Data}, State) ->
    {Tr_id, Result} = decode(Data),
    [{Tr_id, From}] = ets:lookup(State#state.ets, Tr_id),
    ets:delete(State#state.ets, Tr_id),
    gen_server:reply(From, Result),
    {noreply, State};