package effects.chapter06.exercise

import cats.effect.{ExitCode, IO, IOApp}
import effects.debug._

import java.util.concurrent.CompletableFuture
import scala.jdk.FunctionConverters._

object Never extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    never
      .guarantee(IO("I guess never is now").debug.void)
      .as(ExitCode.Success)

  val never: IO[Nothing] = IO.never
}
