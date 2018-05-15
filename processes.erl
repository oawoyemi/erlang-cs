%%print all registered processes in the console without truncation
rp(registered()).

%% registered processes
Pid = spawn(fun() -> receive X -> X end end),
%% <0.48.0>
true = register(mypr, Pid),
whereis(mypr).
%% <0.48.0>

register_process_names() ->
    true = register(tdd, self()),
    tdd ! blimey,
    flush(),
    %% Shell got blimey
    Pid = spawn(fun()-> receive {X, From} -> From ! "thanks!!!" end end),
    %% <0.40.0>
    true = register(pr, Pid),
    pr ! "hey",
    %% "hey"
    pr ! {hey, self()},
    %% {hey,<0.32.0>}
    flush().
%% Shell got "thanks!!!"

process_links() ->
    %A link is a specific kind of relationship that can be created between two processes.
    % When that relationship is set up and one of the processes dies from an unexpected throw,
    % error or exit (see Errors and Exceptions),

    %killing processes - if you wanted to kill another process from the shell,
    %                    you could use the function exit/2, which is called this way: exit(Pid, Reason).

    % restarting processes - System processes are basically normal processes,
    %  except they can convert exit signals to regular messages.
    % This is done by calling process_flag(trap_exit, true) in a running process.
    Pid = spawn_link(fun() -> timer:sleep(10000) end).
    erlang:register(blah, Pid).
    %% try to register same process again yields an error
    erlang:register(blah, Pid).

    %% wait 10 seconds for the process to complete
    timer:sleep(10000).
    %% the process has to be active to successfully register so create a new process
    Pid2 = spawn_link(fun() -> timer:sleep(10000) end).
    erlang:register(blah, Pid2).

%% monitoring
  erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)).
% #Ref<0.0.4.29>
flush().
%% Shell got {'DOWN',#Ref<0.0.4.29>,process,<0.35.0>,normal}


references() ->
    Ref = make_ref(),
    Ref.
% source: http://www.erlang.org/course/advanced.html
% references are unique may be compared for equality,
% system guarantees that no two references created by different calls to make_ref will ever match

