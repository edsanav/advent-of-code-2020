package exercises

import cats.effect.Sync
import cats.data.State
import cats.syntax.applicative._
import exercises.ops.rotateVector

import scala.util.matching.Regex

object algebra {

  type ShipState[A] = State[Ship, A]

  case class Ship(location: Point, direction: Direction)
  case class Waypoint(location: Point)

  case class Point(x: Int, y: Int) {
    lazy val manhattan: Int = math.abs(x) + math.abs(y)

    def -(p2: Point): (Int, Int) = (p2.x - x, p2.y - y)
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
  case class Forward(n: Int) extends Move
  case class MoveNorth(n: Int) extends Move
  case class MoveSouth(n: Int) extends Move
  case class MoveEast(n: Int) extends Move
  case class MoveWest(n: Int) extends Move

  sealed trait Turn extends Move
  case class LeftT(n: Int) extends Turn
  case class RightT(n: Int) extends Turn

  val letterToMove = Map(
    "N" -> MoveNorth,
    "S" -> MoveSouth,
    "E" -> MoveEast,
    "W" -> MoveWest,
    "L" -> LeftT,
    "R" -> RightT,
    "F" -> Forward
  )
}

object ops {
  import algebra._
  def rotateVector(vec: (Int, Int), t: Turn): (Int, Int) = {
    val (x, y) = vec
    val angle = t match {
      case LeftT(n)  => math.toRadians(n.toDouble)
      case RightT(n) => -math.toRadians(n.toDouble)
    }
    val x2 = math.cos(angle) * x - math.sin(angle) * y
    val y2 = math.sin(angle) * x + math.cos(angle) * y
    (x2.round.toInt, y2.round.toInt)
  }
}

object shipOnly {
  import algebra._
  def turn(d: Direction, t: Turn): Direction = {
    val vectorsToDir: Map[(Int, Int), Direction] = Map(
      (-1, 0) -> West,
      (0, 1) -> North,
      (1, 0) -> East,
      (0, -1) -> South
    )
    val dirToVector = vectorsToDir.map { case (k, v) => v -> k }
    vectorsToDir(rotateVector(dirToVector(d), t))
  }

  def move(m: Move): ShipState[Int] = {
    def go(ship: Ship, m: Move): Ship =
      (ship, m) match {
        case (Ship(p, dir), MoveNorth(n)) => Ship(Point(p.x, p.y + n), dir)
        case (Ship(p, dir), MoveSouth(n)) => Ship(Point(p.x, p.y - n), dir)
        case (Ship(p, dir), MoveEast(n))  => Ship(Point(p.x + n, p.y), dir)
        case (Ship(p, dir), MoveWest(n))  => Ship(Point(p.x - n, p.y), dir)
        case (sh: Ship, Forward(n)) => {
          val movement = ship.direction match {
            case North => MoveNorth(n)
            case South => MoveSouth(n)
            case East  => MoveEast(n)
            case West  => MoveWest(n)
          }
          go(sh, movement)
        }
        case (Ship(p, dir), t: Turn) => Ship(p, turn(dir, t))
      }
    State(sh => { val newShip = go(sh, m); (newShip, newShip.location.manhattan) })
  }
}

object shipAndWaypoint {
  import algebra._

  def move(m: Move): State[(Ship, Waypoint), Int] = {
    def go(ship: Ship, m: Move, wp: Waypoint): (Ship, Waypoint) =
      (ship, m, wp) match {
        case (s, MoveNorth(n), Waypoint(p)) => (s, Waypoint(Point(p.x, p.y + n)))
        case (s, MoveSouth(n), Waypoint(p)) => (s, Waypoint(Point(p.x, p.y - n)))
        case (s, MoveEast(n), Waypoint(p))  => (s, Waypoint(Point(p.x + n, p.y)))
        case (s, MoveWest(n), Waypoint(p))  => (s, Waypoint(Point(p.x - n, p.y)))
        case (Ship(sp, dir), f: Forward, w) => {
          val (relX, relY) = sp - w.location
          val fn           = (p: Point) => Point(p.x + relX * f.n, p.y + relY * f.n)
          (Ship(fn(sp), dir), Waypoint(fn(wp.location)))
        }
        case (s, t: Turn, w) => {
          val (x2, y2) = rotateVector(s.location - w.location, t)
          (s, Waypoint(Point(x2 + s.location.x, y2 + s.location.y)))
        }
      }

    State({
      case (sh, wp) =>
        val (newShip, newWp) = go(sh, m, wp);
        ((newShip, newWp), newShip.location.manhattan)
    })
  }

}

object day12 {

  import algebra._

  val pattern: Regex = "^(.)(\\d*)$".r

  def navigate[A](instructions: LazyList[Move], movement: Move => State[A, Int]): State[A, Int] =
    instructions.foldLeft(0.pure[State[A, *]]) { (z, b) =>
      z.flatMap(_ => movement(b))
    }

  // Convert this into Ship => Ship
  def parseInstruction(inst: String): Option[Move] =
    inst match {
      case pattern(l: String, n: String) => letterToMove.get(l).map(m => m(n.toInt))
      case _                             => None
    }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input   = in.to(LazyList).flatMap(parseInstruction)
    val result1 = navigate[Ship](input, shipOnly.move).runA(Ship(Point(0, 0), East)).value
    val result2 = navigate[(Ship, Waypoint)](input, shipAndWaypoint.move)
      .runA((Ship(Point(0, 0), East), Waypoint(Point(10, 1))))
      .value
    Sync[F].delay(println((result1, result2)))
  }

}
