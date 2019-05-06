import scala.collection.parallel._

package object fib {

def fib(n: Int): Int = {
  if (n == 0) 0 else if (n == 1) 1 else fib (n - 2) + fib (n - 1)
}

def fibonaccis_seq(xs: List[Int]) = {
  xs.map(fib)
}

def fibonaccis_par(xs: ParSeq[Int]) = {
  xs.map(fib)
}

}
