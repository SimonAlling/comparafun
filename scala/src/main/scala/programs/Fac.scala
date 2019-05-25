import scala.collection.parallel._
import scala.concurrent.forkjoin.ForkJoinPool
import scala.annotation.tailrec

package object fac {

def fac(n: Int): Int = {
  @tailrec
  def withAcc(acc: Int, x: Int): Int =
    if (x == 0) acc else withAcc(acc * x, x - 1)
  withAcc(1, n)
}

def factorials_seq(xs: Seq[Int]) = {
  xs.map(fac)
}

def factorials_par(xs: ParSeq[Int]) = {
  xs.map(fac)
}

}
