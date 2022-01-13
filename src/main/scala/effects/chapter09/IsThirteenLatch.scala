package effects.chapter09

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import effects.debug._

import scala.concurrent.duration.DurationInt

object IsThirteenLatch extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      latch <- CountdownLatch(13)
      _ <- (beeper(latch), tickingClock(latch)).parTupled
    } yield ExitCode.Success

  def beeper(latch: CountdownLatch): IO[Unit] =
    for {
      _ <- IO("checking is 13...").debug
      _ <- latch.await
      _ <- IO("BEEP!").debug
    } yield ()

  def tickingClock(latch: CountdownLatch): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- latch.decrement
      _ <- IO(s"ticking...").debug
      _ <- tickingClock(latch)
    } yield ()
}
