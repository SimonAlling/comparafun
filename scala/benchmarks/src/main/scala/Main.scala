object Main {
  def bind[A, B](m: Option[A], f: A => Option[B]): Option[B] = {
    m match {
      case Some(x) => f(x)
      case None => None
    }
  }

  def headMay[A](xs: List[A]): Option[A] = {
    xs match {
      case x :: _ => Some(x)
      case Nil => None
    }
  }

  def readMaybeInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

  def replicate[A](n: Int, x: A): List[A] = {
    if (n < 1) Nil else x :: replicate(n - 1, x)
  }

  def fib(n: Int): Int = {
    if (n == 0) 0 else if (n == 1) 1 else fib (n - 2) + fib (n - 1)
  }

  def fibonaccis_seq(xs: List[Int]) = {
    xs.map(fib)
  }

  def fibonaccis_par(xs: List[Int]) = {
    xs.par.map(fib)
  }

  def printHelp(): Unit = {
    println("Usage: ")
  }

  def mainWith(bigNumber: Int): Unit = {
    val numberOfItems = 32 * 32
    val ints = replicate(numberOfItems, bigNumber)
    fibonaccis_par(ints)
  }

  def main(args: Array[String]): Unit = {
    val maybeBigNumber = bind(headMay(args.toList), readMaybeInt)
    maybeBigNumber match {
      case Some(n) => mainWith(n)
      case None => printHelp
    }
  }
}
