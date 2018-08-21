%% Name is either the server pid or the registered name of the server process
%% Message is an Erlang term that gets forwarded as part of the request to the server
gen_server:call(Name, message).
