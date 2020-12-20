package exercises

import cats.effect.Sync
import cats.data.State

import cats.syntax.applicative._ // for pure

import scala.util.matching.Regex

object algebra {

  type ShipState[A] = State[Ship, A]

  case class Ship(location: Point, direction: Direction)

  case class Point(x: Int, y: Int) {
    lazy val manhattan: Int = math.abs(x) + math.abs(y)
  }
  object Point {
    def apply(tup: (Int, Int)): Point = new Point(tup._1, tup._2)
  }

  sealed trait Direction

  final case object North extends Direction
  final case object South extends Direction
  final case object East extends Direction
  final case object West extends Direction

  sealed trait Move {
    val n: Int
  }
  case class Forward(n: Int) extends Move {
    def translate(d: Direction): Move =
      d match {
        case North => MoveNorth(n)
        case South => MoveSouth(n)
        case East  => MoveEast(n)
        case West  => MoveWest(n)
      }
  }
  case class MoveNorth(n: Int) extends Move
  case class MoveSouth(n: Int) extends Move
  case class MoveEast(n: Int) extends Move
  case class MoveWest(n: Int) extends Move

  sealed trait Turn extends Move
  case class Left(n: Int) extends Turn
  case class Right(n: Int) extends Turn

  val letterToMove = Map(
    "N" -> MoveNorth,
    "S" -> MoveSouth,
    "E" -> MoveEast,
    "W" -> MoveWest,
    "L" -> Left,
    "R" -> Right,
    "F" -> Forward
  )

}

object day12 {

  import algebra._

  val pattern: Regex = "^(.)(\\d*)$".r

  def turn(d: Direction, t: Turn): Direction = {
    val vectorsToDir: Map[(Int, Int), Direction] = Map(
      (-1, 0) -> West,
      (0, 1) -> North,
      (1, 0) -> East,
      (0, -1) -> South
    )
    val angle = t match {
      case Left(n)  => math.toRadians(n.toDouble)
      case Right(n) => -math.toRadians(n.toDouble)
    }
    val dirToVector = vectorsToDir.map { case (k, v) => v -> k }
    val (x, y)      = dirToVector(d)
    val x2          = math.cos(angle) * x - math.sin(angle) * y
    val y2          = math.sin(angle) * x + math.cos(angle) * y
    vectorsToDir((x2.toInt, y2.toInt))
  }

  def moveOne(m: Move): State[Ship, Int] = {
    def go(ship: Ship, m: Move): Ship =
      (ship, m) match {
        case (Ship(p, dir), MoveNorth(n)) => Ship(Point(p.x, p.y + n), dir)
        case (Ship(p, dir), MoveSouth(n)) => Ship(Point(p.x, p.y - n), dir)
        case (Ship(p, dir), MoveEast(n))  => Ship(Point(p.x + n, p.y), dir)
        case (Ship(p, dir), MoveWest(n))  => Ship(Point(p.x - n, p.y), dir)
        case (sh: Ship, f: Forward)       => go(sh, f.translate(sh.direction))
        case (Ship(p, dir), t: Turn)      => Ship(p, turn(dir, t))
      }
    State(sh => { val newShip = go(sh, m); (newShip, newShip.location.manhattan) })
  }

  def navigate(instructions: LazyList[Move]): State[Ship, Int] =
    instructions.foldLeft(0.pure[ShipState]) { (z, b) =>
      z.flatMap(_ => moveOne(b))
    }

  // Convert this into Ship => Ship
  def parseInstruction(inst: String): Option[Move] =
    inst match {
      case pattern(l: String, n: String) => letterToMove.get(l).map(m => m(n.toInt))
      case _                             => None
    }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input   = in.to(LazyList).flatMap(parseInstruction)
    val result1 = navigate(input).runA(Ship(Point(0, 0), East)).value
    val result2 = ""
    Sync[F].delay(println((result1, result2)))
  }

}
