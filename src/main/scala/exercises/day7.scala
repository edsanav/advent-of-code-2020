package exercises

import cats.effect.Sync
import cats.implicits._

import scala.util.matching.Regex

object day7 {

  val AMOUNT_BAG_PATTERN: Regex = "^(\\d+) (.*)$".r

  def parseContained(contStr: String): Set[(String, Int)] =
    contStr
      .split(" bag, | bags, | bags.| bag.")
      .flatMap {
        _ match {
          case AMOUNT_BAG_PATTERN(n, bagId) => Some((bagId, n.toInt))
          case _                            => None
        }
      }
      .toSet

  def parseLine(line: String): Option[(String, Set[(String, Int)])] =
    line.split(" bags contain ") match {
      case Array(bagId, "no other bags.") => Some((bagId, Set.empty[(String, Int)]))
      case Array(bagId, containedStr)     => Some((bagId, parseContained(containedStr)))
      case _                              => None
    }

  def containsBag(bagId: String, target: String, ref: Map[String, Set[(String, Int)]]): Boolean =
    ref(bagId).map(_._1).contains(target) || ref(bagId).exists {
        case (otherId, _) => containsBag(otherId, target, ref)
      }

  def sumBags(bagsList: Set[(String, Int)], ref: Map[String, Set[(String, Int)]]): Int =
    bagsList.toList.map { case (bagId, n) => n + n * sumBags(ref(bagId), ref) }.sum

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input   = in.to(LazyList)
    val bagsRef = input.flatMap(parseLine).toMap
    val result1 = bagsRef.keys.count(containsBag(_, "shiny gold", bagsRef))
    val result2 = sumBags(bagsRef("shiny gold"), bagsRef)
    Sync[F].delay(println((result1, result2)))
  }

}
