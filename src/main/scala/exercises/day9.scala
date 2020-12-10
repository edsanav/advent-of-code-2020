package exercises

import cats.effect.Sync

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object day9 {

  def loadPreamble(in: LazyList[Int]): Queue[(Int, List[Int])] =
    in.foldLeft(Queue.empty[(Int, List[Int])]) { case (z, b) => z :+ ((b, in.filter(_ != b).map(b + _).toList)) }

  def contains(p: Queue[(Int, List[Int])], x: Int): Boolean = p.flatMap(_._2.toSet).toSet contains x

  def movePreamble(q: Queue[(Int, List[Int])], x: Int): Queue[(Int, List[Int])] =
    q.tail :+ ((x, q.tail.map(_._1 + x).toList))

  @tailrec
  def findNonCompliant(input: LazyList[Int], preamble: Queue[(Int, List[Int])]): Option[Int] = input match {
    case LazyList() => None
    case x #:: xs   => if (!contains(preamble, x)) Some(x) else findNonCompliant(xs, movePreamble(preamble, x))
  }

  def findCombination(in: LazyList[Int], target: Int): List[Int] = {
    @tailrec
    def go(inner: List[Int], current: List[Int]): List[Int] = {
      val currentSum = current.sum
      if (currentSum == target) current
      else if (currentSum < target) {
        if (current.size >= inner.size) List()
        else go(inner, inner.take(current.size + 1))
      } else {
        if (inner.isEmpty) List()
        else go(inner.tail, List())
      }
    }
    go(in.toList, List.empty[Int])
  }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val instructions           = in.to(LazyList).flatMap(_.toIntOption)
    val (rawPreamble, initial) = instructions.splitAt(25)
    val preamble               = loadPreamble(rawPreamble)
    val result1                = findNonCompliant(initial, preamble)
    val result2                = result1.map(findCombination(initial, _)).map(x => x.min + x.max)
    Sync[F].delay(println((result1, result2)))
  }

}
