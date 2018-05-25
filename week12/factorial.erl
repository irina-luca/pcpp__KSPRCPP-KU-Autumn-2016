% factorial program: Try in the repl (by typing 'erl') => factorial:factorial(6).
-module(factorial).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).