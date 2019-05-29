import Lloyd.Sequential._
import Lloyd.Sequential.{kmeans => kmeansSeq}
import Lloyd.Strategies.{kmeans => kmeansPar}

package object KMeansStuff {

  def squareDistance(a: Point, b: Point): Double = {
    val square = (x: Double) => x*x
    val subtract = (a: Double, b: Double) => a - b
    a.zip(b).map(subtract.tupled andThen square).sum
  }
  
  val expectDivergent = 100000

  def runKMeans(points: Vector[Point], k: Int, parallelism: Int): Vector[Vector[Point]] = {
    val initial =
      if (points.length < k)
        throw new Error(points.length + " points, but k = " + k)
      else
        points.take(k).zipWithIndex.map(tup => Cluster(tup._2, tup._1))
    if (parallelism > 1)
      kmeansPar(expectDivergent, squareDistance, parallelism, points, initial)
    else
      kmeansSeq(expectDivergent, squareDistance, points, initial)
  }

}
