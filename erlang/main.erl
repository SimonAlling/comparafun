-module(main).
-export(
  [ fibBenchmark/1
  , fibBenchmark/3
  , facBenchmark/1
  , facBenchmark/3
  , kmeansBenchmark/1
  , kmeansBenchmark/3
  ]
).

fibBenchmark(Width, Depth, Repetitions) -> do
  , Xs = lists:duplicate(Width, Depth)
  , Threads = erlang:system_info(schedulers)
  , F_seq = fun() -> fib:fibonaccis_seq(Xs) end
  , F_par = fun() -> fib:fibonaccis_par(Xs) end
  , measureEither(F_seq, F_par, Repetitions, Threads)
  , halt()
  .
fibBenchmark([W_str, D_str, Repetitions_str]) -> do
  , { W, _ } = string:to_integer(W_str)
  , { D, _ } = string:to_integer(D_str)
  , { R, _ } = string:to_integer(Repetitions_str)
  , fibBenchmark(W, D, R)
  ;
fibBenchmark(_) -> printUsage().

facBenchmark(Width, Depth, Repetitions) -> do
  , Xs = lists:duplicate(Width, Depth)
  , Threads = erlang:system_info(schedulers)
  , F_seq = fun() -> fac:factorials_seq(Xs) end
  , F_par = fun() -> fac:factorials_par(Xs) end
  , measureEither(F_seq, F_par, Repetitions, Threads)
  , halt()
  .
facBenchmark([W_str, D_str, Repetitions_str]) -> do
  , { W, _ } = string:to_integer(W_str)
  , { D, _ } = string:to_integer(D_str)
  , { R, _ } = string:to_integer(Repetitions_str)
  , facBenchmark(W, D, R)
  ;
facBenchmark(_) -> printUsage().

kmeansBenchmark(N, K, Repetitions) -> do
  , Filename = "kmeans-erlang.testdata"
  , MaxIterations = 100000
  , ConsultResult = file:consult(Filename)
  , case ConsultResult of dummy -> dummy
    ; { ok, Consulted } -> do
      , Points = takeExactly(N, Consulted)
      , Initial = lists:zip(lists:seq(0,K-1), takeK(K, Points))
      , Threads = erlang:system_info(schedulers)
      , F_seq = fun() -> kmeans:kmeans_seq(MaxIterations, Points, Initial) end
      , F_par = fun() -> kmeans:kmeans_par(MaxIterations, Points, Initial, Threads) end
      , measureEither(F_seq, F_par, Repetitions, Threads)
    ; { error, enoent } -> do
      , println("File not found: " ++ Filename)
    ; _ -> do
      , println("Could not read test data.")
    end
  , halt()
  .
kmeansBenchmark([N_str, K_str, Repetitions_str]) -> do
  , { N, _ } = string:to_integer(N_str)
  , { K, _ } = string:to_integer(K_str)
  , { R, _ } = string:to_integer(Repetitions_str)
  , kmeansBenchmark(N, K, R)
  ;
kmeansBenchmark(_) -> printUsage().


printUsage() -> do
  , println("Usage")
  , println("erl +S 24:24 -noinput -run main fibBenchmark WIDTH DEPTH REPETITIONS")
  , println("erl +S 24:24 -noinput -run main kmeansBenchmark N K REPETITIONS")
  .


% Assumes that Repetitions is odd:
measureEither(F_seq, F_par, Repetitions, Threads) -> do
  , F = if Threads > 1 -> F_par; true -> F_seq end
  , Time = measure(Repetitions, F)
  , println("")
  , println("Threads: " ++ integer_to_list(Threads))
  , println("Median: " ++ integer_to_list(microToMilli(Time)) ++ " ms")
  .

% Assumes that Repetitions is odd:
measure(Repetitions, F) -> do
  , TimesAndResults = [ timer:tc(F) || _ <- lists:seq(1, Repetitions) ]
  , Times = [ Time || { Time, _ } <- TimesAndResults ]
  % Median (if Repetitions is odd):
  , lists:nth(1 + Repetitions div 2, lists:sort(Times))
  .

takeK(K, Points) -> do
  , if dummy -> dummy
    ; K < 1              -> erlang:error(too_small_k)
    ; true               -> takeExactly(K, Points)
    end
  .

takeExactly(N, Items) -> do
  , if dummy -> dummy
    ; N > length(Items) -> erlang:error(takeExactly_list_too_short)
    ; true              -> lists:sublist(Items, N)
    end
  .

println(Line) -> io:fwrite(Line ++ "\n").

microToMilli(Micro) -> Micro div 1000.
