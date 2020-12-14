package exercises

import cats.effect.Sync

import scala.annotation.tailrec

object day11 {

  case class Grid(points: Map[(Int, Int), Char]) {

    def available(neighbourFunc: (Int, Int) => List[((Int, Int), Char)]): List[(Int, Int)] =
      points.filter(_._2 == 'L').keys.toList.flatMap {
        case (row, column) => {
          neighbourFunc(row, column).find(c => c._2 == '#') match {
            case Some(_) => None
            case None    => Some((row, column))
          }
        }
      }

    def toBeEmpty(neighbourFunc: (Int, Int) => List[((Int, Int), Char)], limit: Int): List[(Int, Int)] =
      points.filter(_._2 == '#').keys.toList.flatMap {
        case (row, column) => {
          val ocuppiedNeighbours = neighbourFunc(row, column).count(c => c._2 == '#')
          if (ocuppiedNeighbours >= limit) Some((row, column)) else None
        }
      }

    def modify(seats: List[(Int, Int)], char: Char): Grid =
      seats.foldLeft(this) { case (g, seat) => Grid(g.points.updated(seat, char)) }

    def fill(seats: List[(Int, Int)]): Grid  = modify(seats, '#')
    def empty(seats: List[(Int, Int)]): Grid = modify(seats, 'L')

    def ==(grid: Grid): Boolean = this.points == grid.points

    override def toString: String = {
      val (rows, columns) = points.keys.unzip
      val sortedCols      = columns.toList.sorted
      rows.toList.sorted.map(r => sortedCols.map(c => points((r, c))).mkString).mkString("\n")
    }

  }

  def neighbours(points: Map[(Int, Int), Char])(row: Int, column: Int): List[((Int, Int), Char)] =
    (
      for {
        i <- row - 1 to row + 1
        j <- column - 1 to column + 1
      } yield points.get((i, j)).flatMap(c => if (c == '.' || (i == row && j == column)) None else Some(((i, j), c)))
    ).flatten.toList

  def farNeighbours(points: Map[(Int, Int), Char])(row: Int, column: Int): List[((Int, Int), Char)] = {
    @tailrec
    def findFirst(
        pointsIn: Map[(Int, Int), Char],
        f: (Int, Int) => (Int, Int),
        point: (Int, Int)
    ): Option[((Int, Int), Char)] = {
      val newPoint = f(point._1, point._2)
      pointsIn.get(newPoint) match {
        case Some(x) =>
          if (x == '.') {
            findFirst(pointsIn, f, newPoint)
          } else Some(((point._1, point._2), x))
        case None => None
      }
    }

    val closeNeighbours = (for {
      i <- row - 1 to row + 1
      j <- column - 1 to column + 1
    } yield (i, j)).toList.filter { case (x, y) => !(x == row && y == column) }
    // get function to iterate in each direction
    val iterFunctions = closeNeighbours.map { case (i, j) => (x: Int, y: Int) => (x + (i - row), y + (j - column)) }
    iterFunctions.flatMap(f => findFirst(points, f, (row, column)))
  }

  @tailrec
  def run(
      grid: Grid,
      neighbourFunc: Map[(Int, Int), Char] => (Int, Int) => List[((Int, Int), Char)],
      limit: Int
  ): Grid = {
    val filled = grid.fill(grid.available(neighbourFunc(grid.points)))
    if (filled == grid) filled
    else {
      val empty = filled.empty(filled.toBeEmpty(neighbourFunc(filled.points), limit = limit))
      if (empty == filled) empty
      else run(empty, neighbourFunc, limit)
    }
  }

  def loadGrid(in: Iterator[String]): Grid =
    Grid(in.zipWithIndex.flatMap {
      case (lineStr, row) => lineStr.toCharArray.zipWithIndex.map { case (point, column) => ((row, column), point) }
    }.toMap)

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val initialGrid = loadGrid(in)
    val result1     = run(initialGrid, neighbours, 4).points.values.count(_ == '#')
    val result2     = run(initialGrid, farNeighbours, 5).points.values.count(_ == '#')
    Sync[F].delay(println((result1, result2)))
  }

}
