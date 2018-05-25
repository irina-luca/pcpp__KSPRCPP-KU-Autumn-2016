% hello world program => must match filename == modulename
-module(helloworld).
-export([start/0]).

start() ->
    io:fwrite("Hello, world!\n").
