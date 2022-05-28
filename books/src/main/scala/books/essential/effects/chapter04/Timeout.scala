package books.essential.effects.chapter04

import cats.effect.implicits.catsEffectSyntaxBracket
import cats.effect.{ExitCode, IO, IOApp}
import books.essential.effects.debug.DebugHelper

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

object Timeout extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      done <- IO.race(task, timeout)
      _ <- done match {
        case Left(_)  => IO("task: won").debug
        case Right(_) => IO("timeout: won")
      }
    } yield ExitCode.Success

  val task: IO[Unit] = annotatedSleep("task", 1000.millis)
  val timeout: IO[Unit] = annotatedSleep("timeout", 500.millis)

  def annotatedSleep(name: String, duration: FiniteDuration): IO[Unit] = {
    IO(s"$name: starting").debug *>
      IO.sleep(duration) *>
      IO(s"$name: done").debug
  }.onCancel(IO(s"$name: cancelled").debug.void).void
}
