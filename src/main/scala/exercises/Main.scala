package exercises

import cats.effect.{ ExitCode, IO, IOApp }
import aux._

object Main extends IOApp {

  def execute(moduleId: String): List[String] => Unit =
    moduleId match {
      case "1" => day1.run
      case _   => (_) => println(s"Unable to find module ${moduleId}")
    }

  override def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
      case None => IO(System.err.println("Usage: run <week number>")).as(ExitCode.Error)
      case Some(module) =>
        loadResourceFile(s"inputs/day${module}.csv")
          .use(source => IO(execute(module)(lines(source))).as(ExitCode.Success))
    }

}
