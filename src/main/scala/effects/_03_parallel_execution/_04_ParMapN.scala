package effects._03_parallel_execution

import cats.effect._
import cats.implicits._
import effects.debug._

object _04_ParMapN extends IOApp {
  private val hello = IO("Hello").debug
  private val world = IO("World").debug

  private val seq =
    (hello, world)
      .parMapN((h, w) => s"$h $w")
      .debug

  override def run(args: List[String]): IO[ExitCode] =
    seq.as(ExitCode.Success)
}
