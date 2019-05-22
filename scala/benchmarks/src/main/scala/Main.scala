import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import scala.collection.parallel._
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.matching.Regex._
import scala.collection.mutable._
import argonaut._, Argonaut._
import scala.io.Source
import java.io.{FileNotFoundException, IOException}

import fib.{fibonaccis_par, fibonaccis_parChunk, fibonaccis_seq}
import KMeansStuff.{runKMeans}
import Lloyd.Sequential.{Point}

object Config {
  val DIMENSIONS = 2
  val filename = (n: Int, k: Int, seed: Int) => (extension: String) => s"kmeans-correctness-${DIMENSIONS}D-$n-$k-$seed.$extension"
  val EXT_TESTDATA = "testdata"
  val EXT_HASKELL = "haskell"
  val EXT_EXPECTED = "expected"
  val EXT_ACTUAL = "actual"
  val OUTPUT_OK = "OK"
}

object KMeansTools {
  def serializeResult(result: Vector[Vector[Point]]): String = {
    val showPoint = (point: Point) => "[" + point.mkString(", ") + "]"
    val showCluster = (cluster: Vector[Point]) => "[" + cluster.map(showPoint).mkString(",\n") + "]"
    "[" + result.map(showCluster).mkString(",\n\n") + "]"
  }
}

object KMeansCorrectness
extends App {
  val n = args(0).toInt
  val k = args(1).toInt
  val seed = args(2).toInt
  val filename = Config.filename(n, k, seed)
  // Test data
  val testDataFile = filename(Config.EXT_TESTDATA)
  val fileContent = File.read(testDataFile)
  val parsedTestData: List[List[Double]] = fileContent.decodeOption[List[List[Double]]].getOrElse(Nil)
  val vectorizedTestData = parsedTestData.map(_.toVector).toVector
  // Expected result
  val fileContentHaskell = File.read(filename(Config.EXT_HASKELL))
  val parsedHaskell: List[List[List[Double]]] = fileContentHaskell.decodeOption[List[List[List[Double]]]].getOrElse(Nil)
  val vectorizedExpected = parsedHaskell.map(_.map(_.toVector).toVector).toVector
  // Run
  val resultSeq = runKMeans(vectorizedTestData, k, 1)
  val TODO_PARALLELISM = 2
  val resultPar = runKMeans(vectorizedTestData, k, TODO_PARALLELISM)
  val showSomeOf = (l: Int, showable: Any) => { val s = showable.toString; if (s.length > l) s.take(l) + "..." else s }
  val MAX_LENGTH = 1000
  val serialize = KMeansTools.serializeResult _
  if (resultSeq.toString == vectorizedExpected.toString) {
    if (resultSeq == resultPar) {
      println(Config.OUTPUT_OK)
      File.write(filename(Config.EXT_EXPECTED), serialize(resultSeq))
      File.write(filename(Config.EXT_ACTUAL), serialize(resultPar))
    } else {
      println("Incorrect parallel implementation.")
      println("")
      println("Expected:")
      println("")
      println(showSomeOf(MAX_LENGTH, serialize(resultSeq)))
      println("")
      println("")
      println("Actual:")
      println("")
      println(showSomeOf(MAX_LENGTH, serialize(resultPar)))
      File.write(filename(Config.EXT_EXPECTED), serialize(resultSeq))
      File.write(filename(Config.EXT_ACTUAL), serialize(resultPar))
    }
  } else {
    println("Incorrect sequential implementation.")
    println("")
    println("Expected:")
    println("")
    println(showSomeOf(MAX_LENGTH, serialize(vectorizedExpected)))
    println("")
    println("")
    println("Actual:")
    println("")
    println(showSomeOf(MAX_LENGTH, serialize(resultSeq)))
    File.write(filename(Config.EXT_EXPECTED), serialize(vectorizedExpected))
    File.write(filename(Config.EXT_ACTUAL), serialize(resultSeq))
  }
}

object kmeansBenchmark
extends Bench.OfflineReport {
  val n = 20000
  val k = 200
  val seed = 3
  val maxThreads: Int = 24
  val fileContent = File.read(Config.filename(n, k, seed)(Config.EXT_TESTDATA))
  val parsedTestData: List[List[Double]] = fileContent.decodeOption[List[List[Double]]].getOrElse(Nil)
  val vectorizedTestData = parsedTestData.map(_.toVector).toVector
  val unit = Gen.unit("dummy")
  val threads = Gen.range("threads")(2, maxThreads, 1)
  measure method "kmeans_seq" in {
    using (unit) in { _ => runKMeans(vectorizedTestData, k, 1) }
  }
  measure method "kmeans_par" in {
    using (threads) in {
      t => {
        runKMeans(vectorizedTestData, k, t)
      }
    }
  }
}

object fibBenchmark
extends Bench.OfflineReport {
  val width: Int = 1000
  val depth: Int = 30
  val maxThreads: Int = 24

  val unit = Gen.unit("dummy")
  val threads = Gen.range("threads")(2, maxThreads, 1)

  val xs = List.tabulate(width)(_ => depth)
  val xs_par = xs.par
  val xs_vec = xs.toVector

  def withThreads(t: Int) = new ForkJoinTaskSupport(new ForkJoinPool(t))

  measure method "fibonaccis_seq" in {
    using (unit) in (_ => fibonaccis_seq(xs))
  }

  measure method "fibonaccis_par" in {
    using (threads) in (t => {
      xs_par.tasksupport = withThreads(t)
      fibonaccis_par(xs_par)
    })
  }

  measure method "fibonaccis_parChunk" in {
    using (threads) in (t => fibonaccis_parChunk(t, xs_vec))
  }
}


object File {
  def read(filename: String): String = {
    try {
      val bufferedSource = Source.fromFile(filename)
      val fileContent = bufferedSource.getLines.mkString
      bufferedSource.close
      fileContent
    } catch {
      case (e: IOException) => { println("*** ERROR! ***"); println(e); throw e }
    }
  }

  def write(filename: String, content: String): Unit = {
    try {
      reflect.io.File(filename).writeAll(content)
    } catch {
      case (e: IOException) => { println("*** ERROR! ***"); println(e); throw e }
    }
  }
}