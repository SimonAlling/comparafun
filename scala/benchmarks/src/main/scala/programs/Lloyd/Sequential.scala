package Lloyd

/*
This is almost a direct translation of Algorithms.Lloyd.Sequential from a
modified version of the Haskell package kmeans-par.
*/

import scala.collection.parallel._
import scala.collection.mutable
import vector._


package object Sequential {

type Point = Vector[Double]
type Metric[T] = (T, T) => Double

// Semigroup instance
def combinePoints(a: Point, b: Point): Point = {
  val add = (a: Double, b: Double) => a + b
  a.zip(b).map(add.tupled)
}

case class Cluster(
  identifier: Int,
  centroid: Point,
)

def eqClusters(as: Vector[Cluster], bs: Vector[Cluster]): Boolean = {
  val eq = (a: Cluster, b: Cluster) => a.centroid == b.centroid
  as.length == bs.length && as.zip(bs).forall(eq.tupled)
}

case class PointSum(
  count: Int,
  sum: Point,
)

// Semigroup instance
def combinePointSums(a: PointSum, b: PointSum): PointSum =
  PointSum(a.count+b.count, combinePoints(a.sum, b.sum))

def emptyPointSum(length: Int): PointSum =
  PointSum(0, Vector.tabulate(length)(_ => 0.0))

def toCluster(pointSum: PointSum, cid: Int): Cluster =
  Cluster(cid, pointSum.sum.map(_ / pointSum.count))

def closestCluster(
  metric: Metric[Point],
  clusters: Vector[Cluster],
  point: Point
): Cluster = {
  val clustersAndDistances = clusters.map(c => (c, metric(point, c.centroid)))
  clustersAndDistances.minBy(_._2)._1 // compare on snd; return fst
}

def assign(
  metric: Metric[Point],
  clusters: Vector[Cluster],
  points: Vector[Point],
): Vector[Vector[Point]] = {
  val array: Array[List[Point]] = Array.tabulate(clusters.length)(_ => Nil)
  points.foreach(point => {
    val cluster = closestCluster(metric, clusters, point)
    val position = cluster.identifier
    array(position) = point :: array(position)
  })
  array.map(_.toVector).toVector
}

def assignPS(
  metric: Metric[Point],
  clusters: Vector[Cluster],
  points: Vector[Point],
): Vector[PointSum] = {
  val z = emptyPointSum(points.head.length)
  val reduce = (_: Vector[Point]).map(PointSum(1, _)).fold(z)(combinePointSums)
  assign(metric, clusters, points).map(reduce)
}

def makeNewClusters(vector: Vector[PointSum]): Vector[Cluster] =
  // MODIFIED: map instead of flatMap and singleton
  vector.zipWithIndex.map((toCluster _).tupled)

def step(
  metric: Metric[Point],
  clusters: Vector[Cluster],
  points: Vector[Point],
): Vector[Cluster] =
  makeNewClusters(assignPS(metric, clusters, points))

type ExpectDivergent = Int

def computeClusters(
  expectDivergent: ExpectDivergent,
  metric: Metric[Point],
  points: Vector[Point],
  clusters: Vector[Cluster],
): Vector[Cluster] =
  computeClusters_(expectDivergent, metric, 0, points, clusters)

def computeClusters_(
  expectDivergent: ExpectDivergent,
  metric: Metric[Point],
  iterations: Int,
  points: Vector[Point],
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
  points: Vector[Point],
  initial: Vector[Cluster],
): Vector[Vector[Point]] = {
  val clusters = computeClusters(expectDivergent, metric, points, initial)
  assign(metric, clusters, points)
}


// end of package object
}
