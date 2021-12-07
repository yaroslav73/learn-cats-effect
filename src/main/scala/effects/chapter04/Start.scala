package effects.chapter04

import cats.effect.{ExitCode, IO, IOApp}
import effects.debug._

object Start extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      // start the effect to fork its execution from the current effect.
      // start signature:
      // def start(implicit cs: ContextShift[IO]): IO[Fiber[IO, A]]
      _ <- task.start
      _ <- IO("task was started").debug
    } yield ExitCode.Success

  val task: IO[String] = IO("task").debug
}
