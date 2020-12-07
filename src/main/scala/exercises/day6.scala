package exercises

import cats.effect.Sync

object day6 {

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input   = in.mkString(" ").split("  ").to(LazyList)
    val inSet   = input.map(s => s.split(" ").toList.map(_.toSet))
    val result1 = inSet.map(lSets => lSets.foldLeft(Set.empty[Char]) { case (z, s) => z.union(s) }.size).sum
    val result2 = inSet.map(lSets => lSets.tail.foldLeft(lSets.head) { case (z, s) => z.intersect(s) }.size).sum
    Sync[F].delay(println((result1, result2)))
  }

}
