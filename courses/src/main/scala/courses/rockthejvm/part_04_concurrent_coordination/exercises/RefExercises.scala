package courses.rockthejvm.part_04_concurrent_coordination.exercises

import cats.syntax.parallel.*
import cats.effect.{IO, IOApp, Ref}

import java.time.LocalTime
import scala.concurrent.duration.DurationInt

object RefExercises extends IOApp.Simple {

  import courses.rockthejvm.utils._

  def tickingClockImpure: IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] =
      for {
        _ <- IO(LocalTime.now()).trace
        _ <- IO(ticks += 1) // Not thread safe
        _ <- IO.sleep(1.second)
        _ <- tickingClock
      } yield ()

    def printTicks: IO[Unit] =
      for {
        _ <- IO.sleep(5.seconds)
        _ <- IO(s"Current ticks: $ticks").trace
        _ <- printTicks
      } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  // This solution does not work
  def tickingClockWeird: IO[Unit] = {
    val ticks: IO[Ref[IO, Long]] = Ref[IO].of(0L)

    def tickingClock: IO[Unit] =
      for {
        t <- ticks
        _ <- IO.sleep(1.second)
        _ <- IO(LocalTime.now()).trace
        _ <- t.update(_ + 1L)
        _ <- tickingClock
      } yield ()

    def printTicks: IO[Unit] =
      for {
        ref <- ticks
        _   <- IO.sleep(5.seconds)
        t   <- ref.get
        _   <- IO(s"Current ticks: $t").trace
        _   <- printTicks
      } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure: IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] =
      for {
        _ <- IO.sleep(1.second)
        _ <- IO(LocalTime.now()).trace
        _ <- ticks.update(_ + 1L)
        _ <- tickingClock(ticks)
      } yield ()

    def printTicks(ticks: Ref[IO, Long]): IO[Unit] =
      for {
        _ <- IO.sleep(5.seconds)
        t <- ticks.get
        _ <- IO(s"Current ticks: $t").trace
        _ <- printTicks(ticks)
      } yield ()

    for {
      ref <- Ref[IO].of(0L)
      _   <- (tickingClock(ref), printTicks(ref)).parTupled
    } yield ()
  }

  override def run: IO[Unit] = tickingClockPure
}
