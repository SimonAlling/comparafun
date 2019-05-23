-module(fib).
-export([fibonaccis_seq/1, fibonaccis_par/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibonaccis_seq(Xs) -> lists:map(fun fib/1, Xs).

fibonaccis_par(Xs) -> pmap(fun fib/1, Xs).

pmap(F, Xs) -> do
  , Parent = self()
  , Timeout = 60000 % milliseconds
  , Processes =
      [
        spawn_monitor(fun() -> Parent ! {self(), F(X)} end)
      ||
        X <- Xs
      ]
  , collect(Processes, Timeout)
  .

collect([], _) -> [];
collect([{Pid, MRef} | Next], Timeout) -> do
  , receive dummy -> dummy
    ; {Pid, Result} -> do
      , erlang:demonitor(MRef, [flush])
      , [{ok, Result} | collect(Next, Timeout)]
    ; {'DOWN', MRef, process, Pid, Reason} -> do
      , [{error, Reason} | collect(Next, Timeout)]
    after Timeout -> do
      , exit(pmap_timeout)
    end
  .