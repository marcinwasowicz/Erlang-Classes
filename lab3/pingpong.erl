-module(pingpong).
-export[(start/0), (stop/0), (play/1), (sender/2), (catcher/0)].

sender(0,S) ->
    io:format("waiting and got "++integer_to_list(S)++" \n"),
    receive
        {start,N} -> sender(N, 0);
        stop -> ok
    after 
        20000 -> ok
    end;

sender(N, S) ->
    io:format("Sent, and got " ++ integer_to_list(S) ++ " till now\n"),
    pong ! message,
    receive
        answer -> sender(N-1, S+1);
        stop -> ok
    after 
        20000 -> ok
    end.

catcher() ->
    receive
        message -> 
            io:format("recieved\n"),
            ping ! answer,
            catcher();
        stop -> ok
    after 
        20000 -> ok
    end.

start() -> 
    Pid2 = spawn(pingpong, catcher, []),
    Pid1 = spawn(pingpong, sender, [0, 0]),
    register(ping, Pid1),
    register(pong, Pid2).

stop() ->
    ping ! stop,
    pong ! stop.

play(N) ->
    ping ! {start, N}.