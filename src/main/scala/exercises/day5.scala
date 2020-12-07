package exercises

import cats.data.NonEmptyList
import cats.effect.Sync

import scala.annotation.tailrec
import scala.util.matching.Regex

object day5 {

  val CODE_PATTERN: Regex = "^([F|B]{7})([L|R]{3})$".r

  case class Bounds(lower: Int, upper: Int) {
    val midpoint: Int = (upper - lower) / 2

    def next(keep: Int): Bounds =
      if (keep == lower) Bounds(lower, lower + midpoint)
      else Bounds(lower + midpoint + 1, upper)
  }

  def rowF(b: Bounds, c: Char): Int = if (c == 'F') b.lower else b.upper

  def columnF(b: Bounds, c: Char): Int = if (c == 'L') b.lower else b.upper

  @tailrec
  def choose(current: Bounds, code: NonEmptyList[Char], f: (Bounds, Char) => Int): Int =
    code match {
      case NonEmptyList(l, Nil) => f(current, l)
      case NonEmptyList(l, xs) =>
        choose(current.next(f(current, l)), NonEmptyList.fromListUnsafe(xs), f) //xs can not be empty, thus the unsafe
    }

  def splitCode(code: String): Option[(String, String)] = code match {
    case CODE_PATTERN(rc, cc) => Some((rc, cc))
    case _                    => None
  }

  def seatId(rowCode: String, columnCode: String): Int = {
    val row    = choose(Bounds(0, 127), NonEmptyList.fromListUnsafe(rowCode.toList), rowF)
    val column = choose(Bounds(0, 7), NonEmptyList.fromListUnsafe(columnCode.toList), columnF)
    row * 8 + column
  }

  def findSeatId(ids: LazyList[Int], maxId: Int): Option[Int] =
    (0 to maxId).filter(!ids.contains(_)).find(i => ids.contains(i - 1) && ids.contains(i + 1))

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input   = in.to(LazyList)
    val seatIds = input.flatMap(splitCode).map { case (rowCode, columnCode) => seatId(rowCode, columnCode) }
    val result1 = seatIds.maxOption.toRight("Unable to find max")
    val result2 = result1.flatMap(findSeatId(seatIds, _).toRight("Unable to find seat"))
    Sync[F].delay(println((result1, result2)))
  }

}
