package books.essential.effects.chapter03

import cats.effect._
import cats.implicits._
import books.essential.effects.debug._

import scala.concurrent.duration._

object ParMapN extends IOApp {
  private val hello = IO("Hello").debug
  private val world = IO("World").debug
  private val lastChar = IO("!").delayBy(3.seconds).debug

  private val seq =
    (hello, world, lastChar)
      .parMapN((h, w, l) => s"parMapN result: $h $w and last char: $l")
      .debug

  override def run(args: List[String]): IO[ExitCode] =
    seq.as(ExitCode.Success)
}
