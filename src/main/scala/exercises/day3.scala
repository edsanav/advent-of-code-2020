package exercises

import cats.effect.Sync

object day3 {

  val moves: List[(Int, Int)] = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

  def countTrees(ll: LazyList[String], right: Int = 3, down: Int = 1): Int = {
    val filtered = ll.zipWithIndex.filter { case (_, i) => i % down == 0 }.map(_._1) // only visited "lines"
    filtered.zipWithIndex.map { case (l, idx) => if ((l * 999)(right * idx) == '#') 1 else 0 }.sum
  }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input   = in.to(LazyList)
    val result1 = countTrees(input)
    val result2 = moves.map { case (right, down) => BigInt(countTrees(input, right, down)) }.product
    Sync[F].delay(println((result1, result2)))
  }

}
