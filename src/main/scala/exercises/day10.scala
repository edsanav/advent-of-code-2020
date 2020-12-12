package exercises

import cats.effect.Sync

object day10 {

  def countPossibilities(deltas: LazyList[Int]): Long =
    deltas match {
      case 1 #:: 1 #:: 1 #:: xs => 3 * countPossibilities(1 #:: xs) + countPossibilities(xs)
      case 1 #:: 1 #:: xs       => 2 * countPossibilities(1 #:: xs)
      case _ #:: xs             => countPossibilities(xs)
      case LazyList()           => 1
    }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val outlets      = in.to(LazyList).flatMap(_.toIntOption).sorted
    val outletsFixed = 0 #:: outlets :+ outlets.last + 3
    val deltas       = outletsFixed.drop(1).lazyZip(outletsFixed).map(_ - _)
    val differences  = deltas.foldLeft(Map.empty[Int, Int]) { case (z, b) => z + (b -> (z.getOrElse(b, 0) + 1)) }
    val result1      = differences.getOrElse(1, 0) * differences.getOrElse(3, 0)
    val result2      = countPossibilities(deltas)
    Sync[F].delay(println((result1, result2)))
  }
}
