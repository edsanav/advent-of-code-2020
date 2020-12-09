package exercises

import cats.data.State
import cats.effect.Sync

object day8 {

  sealed trait Instruction
  case class Jump(n: Int) extends Instruction
  case class Acc(z: Int) extends Instruction
  case class Nop(x: Int) extends Instruction

  case class Console(next: Int, visited: Set[Int], acc: Int)

  type ConsoleState = State[Console, Int]

  def toInstruction(moveStr: String): Option[Instruction] =
    moveStr.split(" ") match {
      case Array("nop", x) => x.toIntOption.map(Nop)
      case Array("jmp", n) => n.toIntOption.map(Jump)
      case Array("acc", z) => z.toIntOption.map(Acc)
      case _               => None
    }

  def parseInstruction(ins: Instruction): State[Console, Int] =
    State(c => {
      val visited = c.visited + c.next
      ins match {
        case Jump(n) => (Console(c.next + n, visited, c.acc), c.acc)
        case Acc(z)  => (Console(c.next + 1, visited, c.acc + z), c.acc + z)
        case Nop(_)  => (Console(c.next + 1, visited, c.acc), c.acc)
      }
    })

  def execute(all: Vector[Instruction]): State[Console, (Int, Boolean)] =
    for {
      current <- State.get
      _ <- parseInstruction(all(current.next))
      out <- State.get
    } yield {
      if (out.next == all.size) {
        (out.acc, true)
      } else if (out.visited contains out.next) {
        (out.acc, false)
      } else {
        execute(all).runA(out).value
      }
    }

  def retryWithReplacements(
      instructions: LazyList[Instruction],
      run: Vector[Instruction] => (Int, Boolean)
  ): Option[Int] = {
    val replacements = instructions.zipWithIndex
      .filter {
        case (Jump(_), _) | (Nop(_), _) => true
        case _                          => false
      }
      .map {
        case (Jump(x), idx) => (Nop(x), idx)
        case (Nop(x), idx)  => (Jump(x), idx)
        case x              => x
      }
    val variations = replacements.map { case (newInst, idx) => instructions.updated(idx, newInst).toVector }
    variations.find(insts => run(insts)._2).map(run(_)._1)
  }

  def runConsole: Vector[Instruction] => (Int, Boolean) = execute(_).runA(Console(0, Set.empty[Int], 0)).value

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val instructions = in.to(LazyList).flatMap(toInstruction)
    val result1      = runConsole(instructions.toVector)._1
    val result2      = retryWithReplacements(instructions, runConsole)
    Sync[F].delay(println((result1, result2)))
  }

}
