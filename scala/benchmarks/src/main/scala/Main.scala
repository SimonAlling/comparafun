import fib.{fibonaccis_par, fibonaccis_seq}

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

  def printHelp(): Unit = {
    println("Usage: ")
    println("main DEPTH WIDTH")
  }

  def mainWith(depth: Int, width: Int): Unit = {
    val ints = replicate(width, depth)
    fibonaccis_seq(ints)
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case depth :: width :: rest => {
        val maybeDepth = readMaybeInt(depth)
        val maybeWidth = readMaybeInt(width)
        (maybeDepth, maybeWidth) match {
          case (Some(d), Some(w)) => mainWith(d, w)
          case _ => printHelp
        }
      }
      case _ => printHelp
    }
  }
}
