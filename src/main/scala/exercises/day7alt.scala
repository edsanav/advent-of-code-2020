package exercises

import cats.effect.Sync

import scala.util.matching.Regex

object day7alt {

  val AMOUNT_BAG_PATTERN: Regex = "^(\\d+) (.*)$".r

  case class Bag(id: String, content: List[(Int, Bag)]) {
    val bags: List[Bag] = content.map(_._2)

    def fill(ref: Map[String, Bag]): Bag =
      Bag(id, content.flatMap { case (n, bag) => ref.get(bag.id).map(b => (n, b.fill(ref))) })

    def contains(bagId: String): Boolean =
      (bags.map(_.id) contains bagId) || bags.exists(_ contains bagId)

    def totalBags: Int =
      content.map {
        case (n, b) => {
          n + n * b.totalBags
        }
      }.sum
  }

  def parseContained(contStr: String): List[(Int, Bag)] =
    contStr
      .split(" bag, | bags, | bags.| bag.")
      .flatMap {
        _ match {
          case AMOUNT_BAG_PATTERN(n, bagId) => Some((n.toInt, Bag(bagId, List.empty[(Int, Bag)])))
          case _                            => None
        }
      }
      .toList

  def parseLine(line: String): Option[Bag] =
    line.split(" bags contain ") match {
      case Array(bagId, "no other bags.") => Some(Bag(bagId, List.empty[(Int, Bag)]))
      case Array(bagId, containedStr)     => Some(Bag(bagId, parseContained(containedStr)))
      case _                              => None
    }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input      = in.to(LazyList)
    val bagsRef    = input.flatMap(parseLine).map(b => (b.id, b)).toMap
    val filledBags = bagsRef.map { case (id, bag) => (id, bag.fill(bagsRef)) }
    val result1    = filledBags.values.count(bag => bag contains "shiny gold")
    val result2    = filledBags.get("shiny gold").map(_.totalBags)
    Sync[F].delay(println((result1, result2)))
  }

}
