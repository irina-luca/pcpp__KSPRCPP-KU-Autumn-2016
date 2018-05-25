% hello world program
-module(helloworld).
-export([start/0, person/0, ecco/0]).

person() ->
    receive
        {start, Pid} ->
            S = "hvad drikker Moller",
            io:fwrite("[says]: " ++ S ++ "\n"),
            Pid ! {self(), {message, S}};
        {message, S} ->
            io:fwrite("[hears]: " ++ S ++ "\n")
    end,
    person().


ecco()  ->
    receive
        {Sender, {message, S}}   ->
            Sub = string:substr(S, 15),
            Sender ! {message, Sub},
            Sender ! {message, Sub},
            Sender ! {message, Sub},
            ecco()
    end.

start() ->
    Person = spawn(helloworld, person, []),
    Ecco = spawn(helloworld, ecco, []),
    Person ! {start, Ecco}.
