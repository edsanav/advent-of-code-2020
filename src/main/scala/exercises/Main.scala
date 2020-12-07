package exercises

import cats.effect.{ ExitCode, IO, IOApp }
import aux._

object Main extends IOApp {

  //TODO Improve modules returned types

  def executeIO(moduleId: String): Iterator[String] => IO[Unit] =
    moduleId match {
      case "1" => day1.run[IO]
      case "2" => day2.run[IO]
      case "3" => day3.run[IO]
      case "4" => day4.run[IO]
      case "5" => day5.run[IO]
      case _   => (_) => IO(println(s"Unable to find module ${moduleId}"))
    }

  override def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
      case None => IO(System.err.println("Usage: run <week number>")).as(ExitCode.Error)
      case Some(module) =>
        loadResourceFile(s"inputs/day${module}.csv")
          .use(source => executeIO(module)(lines(source)).as(ExitCode.Success))
    }

}
