package exercises

import cats.Monad

import scala.annotation.tailrec

object day1 {

  val EXPECTED: Int = 2020

  @tailrec
  def part1(ll: LazyList[Int], expected: Int = EXPECTED): Option[(Int, Int)] = ll match {
    case LazyList() => None
    case x #:: xs =>
      val y = expected - x
      xs.find(_ == y) match {
        case Some(_) => Some((x, y))
        case None    => part1(xs, expected)
      }
  }

  @tailrec
  def part2(ll: LazyList[Int], expected: Int = EXPECTED): Option[(Int, Int, Int)] = ll match {
    case LazyList() => None
    case x #:: xs =>
      part1(xs, expected - x) match {
        case Some((y, z)) => Some((x, y, z))
        case None         => part2(xs, expected)
      }
  }

  def run[F[_]](in: Iterator[String])(implicit m: Monad[F]): F[Unit] = {
    val input   = in.to(LazyList).map(_.toInt)
    val result1 = part1(input).map { case (x, y) => x * y }
    val result2 = part2(input).map { case (x, y, z) => println((x, y, z)); x * y * z }
    m.pure(println((result1, result2)))
  }

}
