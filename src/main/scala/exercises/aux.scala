package exercises

import cats.effect.{ IO, Resource }

import scala.io.{ BufferedSource, Source }

object aux {

  //TODO abstract over F instead of having an explicit IO here
  def loadResourceFile(path: String): Resource[IO, BufferedSource] =
    Resource.make {
      IO(Source.fromResource(path))
    } { source =>
      IO(source.close()).handleErrorWith(_ => IO.unit)
    }

  def lines(source: BufferedSource): Iterator[String] = source.getLines()

}
