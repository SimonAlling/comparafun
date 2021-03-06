fun fac 0 = 1
  | fac n = n * fac (n - 1)

fun factorials_seq width depth =
  let
    val xs = Array.tabulate (width, (fn _ => depth))
  in
    Array.map (fn x => fac x) xs
  end

fun factorials_par width depth =
  let
    val rope = Rope.tabulateSequential (fn _ => depth) (1, width)
    val xs = PArray.fromRope rope
  in
    PArray.map (fn x => fac x) xs
  end

val success = 0
val failure = 1

fun badUsage () =
  let
    val _ = Print.printLn "Usage:"
    val _ = Print.printLn "./fac WIDTH DEPTH 1        (sequential)"
    val _ = Print.printLn "./fac WIDTH DEPTH 2        (parallel)"
  in failure end

fun withParams (w : int) (d : int) (p : int) =
  case p of
      1 => let val _ = factorials_seq w d in success end
    | 2 => let val _ = factorials_par w d in success end
    | _ => badUsage ()

fun main (width :: depth :: parallelism :: _) =
      let
        val w = Int.fromString width
        val d = Int.fromString depth
        val p = Int.fromString parallelism
      in case (w, d, p) of
          (SOME(w), SOME(d), SOME(p)) => withParams w d p
        | _ => badUsage ()
      end
  | main _ = badUsage ()

val _ = main (CommandLine.arguments ())
