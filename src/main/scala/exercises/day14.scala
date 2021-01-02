package exercises

import cats.effect.Sync
import cats.{ Align, Foldable }
import cats.data.State
import cats.syntax.foldable._
import cats.syntax.applicative._

import scala.util.matching.Regex

object day14 {

  val mask: Regex   = "mask = (.*)".r
  val memory: Regex = raw"mem\[(\d*)\] = (\d*)".r

  case class Mask(raw: String)
  case class Memory(position: Long, value: Long)
  case class Block(mask: Mask, mems: List[Memory])

  case class Computer(addresses: Map[Long, Long])

  def parseInstructions(groupBlock: String): Option[List[(Memory, Mask)]] = {
    val mems = memory.findAllIn(groupBlock).map { case memory(pos, value) => Memory(pos.toLong, value.toLong) }.toList
    mask.findFirstIn(groupBlock) match {
      case Some(mask(value)) => Some(mems.map(m => (m, Mask(value))))
      case _                 => None
    }
  }

  def applyMask(
      mem: Memory,
      mask: Mask,
      // Ops to align both
      inBothF: (Char, Char) => Char,
      onlyInMaskF: Char => Char = identity,
      onlyInValueF: Char => Char = identity
  ): List[Char] = {
    val valBinRev = mem.value.toBinaryString.reverse.toList
    val maskRev   = mask.raw.reverse.toList
    Align[List].alignWith(valBinRev, maskRev)(_.fold(onlyInValueF, onlyInMaskF, inBothF)).reverse
  }

  def bitMask(mem: Memory, mask: Mask): List[(Long, Long)] = {
    val bothF: (Char, Char) => Char = (a, b) => if (b == 'X') a else b
    val finalVal                    = applyMask(mem, mask, bothF, onlyInMaskF = (b: Char) => if (b == 'X') '0' else b)
    List((mem.position, BigInt(finalVal.mkString, 2).toLong))
  }

  def applyChanges[A, F[_]: Foldable](vec: Vector[A], changes: F[(Int, A)]): Vector[A] =
    changes.foldLeft(vec) { case (z, (pos, value)) => z.updated(pos, value) }

  def memAddresDecoder(mem: Memory, mask: Mask): List[(Long, Long)] = {
    val bothF: (Char, Char) => Char = (a, b) => if (b == '0') a else b
    val masked                      = applyMask(mem, mask, bothF).toVector

    val xPositions = masked.zipWithIndex.filter(_._1 == 'X').map(_._2)
    val combinations =
      List.fill(xPositions.length)(List('0', '1')).flatten.combinations(xPositions.length).flatMap(_.permutations)
    val replacements = combinations.map(l => xPositions.zip(l)).toList

    replacements.map(v => (BigInt(applyChanges(masked, v).mkString, 2).toLong, mem.value))
  }

  def program(instructions: List[(Long, Long)]): State[Computer, Long] = {
    val updateProgram = instructions.foldLeft(().pure[State[Computer, *]]) {
      case (z, (pos, value)) => z.flatMap(_ => State.modify(c => Computer(c.addresses.updated(pos, value))))
    }
    updateProgram.inspect(computer => computer.addresses.values.sum)
  }

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val rawInstructions = in.mkString("\n").split("(?=mask)").flatMap(parseInstructions).flatten.toList
    val initialComputer = Computer(Map.empty[Long, Long])
    val instructions1   = rawInstructions.flatMap { case (me, ma) => bitMask(me, ma) }
    val instructions2   = rawInstructions.flatMap { case (me, ma) => memAddresDecoder(me, ma) }
    val result1         = program(instructions1).runA(initialComputer).value
    val result2         = program(instructions2).runA(initialComputer).value
    Sync[F].delay(println((result1, result2)))
  }
}
