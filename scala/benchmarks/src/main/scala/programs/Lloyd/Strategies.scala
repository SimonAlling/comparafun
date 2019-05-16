package Lloyd

/*
This is almost a direct translation of Algorithms.Lloyd.Strategies from a
modified version of the Haskell package kmeans-par.
*/

import scala.collection.parallel._
import scala.collection.parallel.immutable._
import scala.collection.immutable._
import Sequential.{Point, Cluster, PointSum, ExpectDivergent, Metric, assign, assignPS, combinePointSums, emptyPointSum, eqClusters, makeNewClusters}
import vector.{chunkInto}
import scala.concurrent.forkjoin.ForkJoinPool

package object Strategies {

def withThreads(t: Int) = new ForkJoinTaskSupport(new ForkJoinPool(t))

// Semigroup instance
def combineVectors(a: Vector[PointSum], b: Vector[PointSum]): Vector[PointSum] = {
  a.zip(b).map((combinePointSums _).tupled)
}

def step(
  metric: Metric[Point],
  clusters: Vector[Cluster],
  pointChunks: Vector[Vector[Point]],
): Vector[Cluster] = {
  val pointChunks_par = pointChunks.par
  pointChunks_par.tasksupport = withThreads(pointChunks_par.length)
  val assigned = pointChunks.par.map(assignPS(metric, clusters, _))
  // We emulate the semantics of foldr1 here because a regular fold crashes:
  makeNewClusters(assigned.tail.fold(assigned.head)(combineVectors))
}

def computeClusters(
  expectDivergent: ExpectDivergent,
  metric: Metric[Point],
  partitions: Int,
  points: Vector[Point],
  clusters: Vector[Cluster],
): Vector[Cluster] = {
  computeClusters_(expectDivergent, metric, 0, chunkInto(partitions, points), clusters)
}

def computeClusters_(
  expectDivergent: ExpectDivergent,
  metric: Metric[Point],
  iterations: Int,
  points: Vector[Vector[Point]],
  clusters: Vector[Cluster],
): Vector[Cluster] = {
  val newClusters = step(metric, clusters, points)
  Unit match {
    case _ if iterations >= expectDivergent     => clusters
    case _ if eqClusters(newClusters, clusters) => clusters
    case _                                      =>
      computeClusters_(expectDivergent, metric, iterations+1, points, newClusters)
  }
}

def kmeans(
  expectDivergent: ExpectDivergent,
  metric: Metric[Point],
  partitions: Int,
  points: Vector[Point],
  initial: Vector[Cluster],
): Vector[Vector[Point]] = {
  val clusters = computeClusters(expectDivergent, metric, partitions, points, initial)
  assign(metric, clusters, points).map(_.toVector)
}


// end of package object
}
