import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import scala.collection.parallel._
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.matching.Regex._
import argonaut._, Argonaut._
import scala.io.Source
import java.io.{FileNotFoundException, IOException}

import fib.{fibonaccis_par, fibonaccis_seq}
import KMeansStuff.{runKmeans}


object KMeansBenchmark
extends Bench.OfflineReport {
  val pattern_k = raw"testdata\-kmeans\-\d+\-(\d+)\-\d+\.txt".r

  def extractK(filename: String): Int = {
    filename match {
      case pattern_k(k) => k.toInt
      case _ => throw new Error("Could not extract k from this filename: "+filename)
    }
  }

  val FILENAME = "testdata-kmeans-20000-200-3.txt"
  try {
    val bufferedSource = Source.fromFile(FILENAME)
    val fileContent = bufferedSource.getLines.mkString
    bufferedSource.close
    val parsedTestData: List[List[Double]] = fileContent.decodeOption[List[List[Double]]].getOrElse(Nil)
    val vectorizedTestData = parsedTestData.map(_.toVector).toVector
    val k = extractK(FILENAME)
    println("k = " + k)

    val unit = Gen.unit("dummy")
    measure method "kmeans_seq" in {
      using (unit) in { _ => runKmeans(vectorizedTestData, k) }
    }
  } catch {
    case (e: IOException) => { println("*** ERROR! ***"); println(e) }
  }
}

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
