package effects._03_parallel_execution

import cats.effect._
import cats.implicits._
import effects.debug._

import scala.concurrent.duration._

object _04_ParMapN extends IOApp {
  private val hello = IO("Hello").debug
  private val world = IO("World").debug
  private val lastChar = IO("!").delayBy(3.seconds).debug

  private val seq =
    (hello, world, lastChar)
      .parMapN((h, w, l) => s"$h $w$l")
      .debug

  override def run(args: List[String]): IO[ExitCode] =
    seq.as(ExitCode.Success)
}
