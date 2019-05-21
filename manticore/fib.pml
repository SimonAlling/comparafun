val width = 1000
val depth = 30

fun fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 2) + fib (n - 1)

(*val xs = PArray.fromRope (Rope.tabulateSequential (fn _ => depth) (1, width))
*)
val xs = Array.tabulate (width, (fn _ => depth))

val _ = Array.map (fn x => fib x) xs
