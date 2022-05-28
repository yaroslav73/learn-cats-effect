package books.essential.effects.chapter09

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import books.essential.effects.debug._

import scala.concurrent.duration.DurationInt

object IsThirteen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0)
      is13 <- Deferred[IO, Unit]
      _ <- (beepWhen13(is13), tickingClock(ticks, is13)).parTupled
//      _ <- (beepWhen13(ticks), tickingClock(ticks)).parTupled
    } yield ExitCode.Success

  def beepWhen13(ticks: Ref[IO, Int]): IO[Unit] =
    for {
      value <- ticks.get
      _ <- IO(s"checking ticks... $value").debug
      _ <-
        if (value >= 13) IO("BEEP!").debug
        else IO.sleep(2.second) *> beepWhen13(ticks)
    } yield ()

  def tickingClock(ticks: Ref[IO, Int]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

  def beepWhen13(is13: Deferred[IO, Unit]): IO[Unit] =
    for {
      _ <- IO("checking is 13...").debug
      _ <- is13.get
      _ <- IO("BEEP!").debug
    } yield ()

  def tickingClock(ticks: Ref[IO, Int], is13: Deferred[IO, Unit]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      t <- ticks.updateAndGet(_ + 1)
      _ <- IO(s"ticking... $t").debug
      _ <- if (t >= 13) is13.complete(()) else IO.unit
      _ <- tickingClock(ticks, is13)
//      Code below stop tickingClock after t >= 13
//      _ <- if (t >= 13) is13.complete(()) else tickingClock(ticks, is13)
    } yield ()
}
