import scala.collection.parallel._
import vector.{chunkInto}
import scala.collection.immutable._
import scala.concurrent.forkjoin.ForkJoinPool

package object fib {

def fib(n: Int): Int = {
  if (n == 0) 0 else if (n == 1) 1 else fib (n - 2) + fib (n - 1)
}

def fibonaccis_seq(xs: Seq[Int]) = {
  xs.map(fib)
}

def fibonaccis_par(xs: ParSeq[Int]) = {
  xs.map(fib)
}

def fibonaccis_parChunk(n: Int, xs: Vector[Int]) = {
  val chunks = chunkInto(n, xs)
  val chunks_par = chunks.par
  val pool = new ForkJoinPool(n)
  chunks_par.tasksupport = new ForkJoinTaskSupport(pool)
  val mappedChunks = chunks_par.map(chunk => chunk.map(fib))
  pool.shutdown()
  mappedChunks.flatten
}

}
