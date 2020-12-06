package exercises

import cats.effect.Sync

import scala.util.matching.Regex

object day2 {

  //"1-8 : tstztmttgtttfvt"
  val pattern: Regex = raw"(\d{1,2})-(\d{1,2})\s(.):\s(.*)".r

  case class PolicyData(letter: Char, i: Int, j: Int)

  def countPolicyFactory(p: PolicyData)(s: String): Boolean = {
    val total = s.foldLeft(0) { case (z, l) => if (p.letter == l) z + 1 else z }
    (p.i <= total) && (total <= p.j)
  }

  def positionPolicyFactory(p: PolicyData)(s: String): Boolean = {
    val vec = s.toVector
    vec.length >= p.j &&
    ((vec(p.i - 1) == p.letter && vec(p.j - 1) != p.letter) || (vec(p.i - 1) != p.letter && vec(p.j - 1) == p.letter))
  }

  def getData(s: String): Option[(PolicyData, String)] =
    s match {
      case pattern(min, max, letter, password) => Some((PolicyData(letter.charAt(0), min.toInt, max.toInt), password))
      case _                                   => None
    }

  def validPasswordCount(ll: LazyList[String], policyFactory: PolicyData => String => Boolean): Int =
    ll.flatMap(getData).count { case (p, pass) => policyFactory(p)(pass) }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input   = in.to(LazyList)
    val result1 = validPasswordCount(input, countPolicyFactory)
    val result2 = validPasswordCount(input, positionPolicyFactory)
    Sync[F].delay(println((result1, result2)))
  }

}
