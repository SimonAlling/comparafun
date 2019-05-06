import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import scala.collection.parallel._
import scala.concurrent.forkjoin.ForkJoinPool

import fib.{fibonaccis_par, fibonaccis_seq}

object FibBenchmark
extends Bench.OfflineReport {
  val width: Int = 1000
  val depth: Int = 30
  val maxThreads: Int = 20

  val unit = Gen.unit("dummy")
  val threads = Gen.range("threads")(1, maxThreads, 1)

  val xs = List.tabulate(width)(_ => depth)
  val xs_par = xs.par

  def withThreads(t: Int) = new ForkJoinTaskSupport(new ForkJoinPool(t))

  measure method "fibonaccis_seq" in {
    using (unit) in { _ => fibonaccis_seq(xs) }
  }

  measure method "fibonaccis_par" in {
    using (threads) in {
      t => {
        xs_par.tasksupport = withThreads(t)
        fibonaccis_par(xs_par)
      }
    }
  }
}
