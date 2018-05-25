% hello world program: created here => http://www.tutorialspoint.com/compile_erlang_online.php
-module(helloworld).
-export([start/0, myactor/1]).

start() ->
    MyActor = spawn(helloworld, myactor, [0]),
    MyActor ! {msg, "hello"},
    MyActor ! {msg, "world"},
    MyActor ! {msg, "test"}.
    
myactor(Count) -> %% can have state, such as Counter here
    receive
        {msg, Msg} ->
            io:fwrite(Msg ++ " ("),
            io:write(Count),
            io:fwrite(")\n"),
            myactor(Count + 1)
    end.