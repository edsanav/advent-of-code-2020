package exercises

import cats.effect.Sync

object day13 {

  def findNext(start: BigInt, step: BigInt, busId: Int, offset: Int): (BigInt, BigInt) = {
    val myseq = LazyList.iterate(start)(_ + step)
    val num   = myseq.find(c => (c + offset) % busId == BigInt(0)).get
    (num, busId * step)
  }

  def findPoint(buses: List[(Int, Int)], start: BigInt = BigInt(0)): BigInt = {
    val (firstStep, _) = buses.head
    val netStart       = (start / firstStep) * firstStep
    buses.tail
      .foldLeft((netStart, BigInt(firstStep))) {
        case ((start, step), (busId, offset)) => findNext(start, step, busId, offset)
      }
      ._1
  }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val Array(estimateStr, freqsStr) = in.toArray
    val estimate                     = estimateStr.toInt
    val buses = freqsStr.split(",").toList.zipWithIndex.filter(_._1 != "x").map {
      case (x, idx) => (x.toInt, idx)
    }
    val freqs   = buses.map(_._1)
    val result1 = freqs.map(x => (x, x - estimate % x)).sortBy(_._2).headOption.map(x => x._1 * x._2)
    val result2 = findPoint(buses, BigInt("100000000000000"))
    Sync[F].delay(println((result1, result2)))
  }
}
