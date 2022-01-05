package effects.chapter09

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits.catsSyntaxTuple2Parallel
import effects.debug._

import scala.concurrent.duration._

object ConcurrentStateRef extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0L)
      _ <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ExitCode.Success

  def tickingClock(ticks: Ref[IO, Long]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- Clock[IO].instantNow.debug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

  def printTicks(ticks: Ref[IO, Long]): IO[Unit] =
    for {
      _ <- IO.sleep(5.seconds)
      n <- ticks.get
      _ <- IO(s"TICKS: $n").debug
      _ <- printTicks(ticks)
    } yield ()
}
