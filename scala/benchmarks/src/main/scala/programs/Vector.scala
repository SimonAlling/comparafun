import scala.collection.immutable._

package object vector {

def chunkInto[A](n: Int, vector: Vector[A]): Vector[Vector[A]] =
  vector.grouped(vector.length / n).toVector

}
