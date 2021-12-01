package effects.chapter03

import cats.effect._
import cats.implicits._
import effects.debug._

object InspectParallelism extends IOApp {
  private val hello = IO("Hello").debug
  private val world = IO("World").debug

  private val seq = (hello, world).mapN((h, w) => s"$h $w").debug

  override def run(args: List[String]): IO[ExitCode] =
    seq.as(ExitCode.Success)
}
