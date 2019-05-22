fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 2) + fib (n - 1)

fun fibonaccis_seq width depth =
  let
    val xs = Array.tabulate (width, (fn _ => depth))
  in
    Array.map (fn x => fib x) xs
  end

fun fibonaccis_par width depth =
  let
    val rope = Rope.tabulateSequential (fn _ => depth) (1, width)
    val xs = PArray.fromRope rope
  in
    PArray.map (fn x => fib x) xs
  end

val _ = fibonaccis_par 1000 30