package effects.chapter05

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import effects.debug._

object Blocking extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use { blocker =>
      withBlocker(blocker).as(ExitCode.Success)
    }

  def withBlocker(blocker: Blocker): IO[Unit] =
    for {
      _ <- IO("On default").debug
      _ <- blocker.blockOn(IO("On blocker").debug)
      _ <- IO("Where am I?").debug
    } yield ()
}
