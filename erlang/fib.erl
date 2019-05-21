-module(fib).
-export([fibonaccis_seq/1, fibonaccis_par/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibonaccis_seq(Xs) -> lists:map(fun(X) -> fib(X) end, Xs).

fibonaccis_par(Xs) -> pmap(fun(X) -> fib(X) end, Xs).

pmap(F, Xs) -> do
  , Parent = self()
  , OneMinute = 60000
  , Running = [
      spawn_monitor(fun() -> Parent ! {self(), F(X)} end)
      ||
      X <- Xs
    ]
  , collect(Running, OneMinute)
  .

collect([], _Timeout) -> [];
collect([{Pid, MRef} | Next], Timeout) -> do
  , receive dummy -> dummy
    ; {Pid, Res} -> do
      , erlang:demonitor(MRef, [flush])
      , [{ok, Res} | collect(Next, Timeout)]
    ; {'DOWN', MRef, process, Pid, Reason} -> do
      , [{error, Reason} | collect(Next, Timeout)]
    after Timeout -> do
      , exit(pmap_timeout)
    end
  .