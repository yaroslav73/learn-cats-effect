package courses.rockthejvm.part_04_concurrent_coordination.exercises

import cats.syntax.either.*
import cats.syntax.parallel.*
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred

import scala.concurrent.duration.DurationInt
import cats.effect.kernel.Outcome
import cats.effect.kernel.Fiber
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

object DeferredExercises extends IOApp.Simple {
  import courses.rockthejvm.utils.*

  // Exercises:
  // 1. (medium) Write a small alarm notifications with two simultaneous IOs:
  //    - one that increments a counter every second (a clock)
  //    - one that waits for the counter to become 10, the prints a message "time's up!"
  //
  // 2. (mega hard) Implement racePair with Deferred:
  //    - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
  //    - start two fibers, one for each IO
  //    - on completion (with any status), each IO needs to complete that Deferred
  //      (hint 1: use a finalizer from the Resources lesson)
  //      (hint 2: use a guarantee call to make sure the fibers complete the Deferred)
  //    - what do you do in case of cancellation (the hardest part)?

  def timer: IO[Unit] = {
    def increment(counter: Ref[IO, Int], deferred: Deferred[IO, Int]): IO[Unit] =
      for {
        n <- counter.updateAndGet(_ + 1)
        _ <- IO(s"[increment]: $n").trace
        _ <- IO.sleep(1.second)
        _ <- if (n >= 10) deferred.complete(n) else increment(counter, deferred)
      } yield ()

    def notifier(deferred: Deferred[IO, Int]): IO[Unit] =
      for {
        _ <- IO("[notifier]: Start timer!").trace
        _ <- deferred.get
        _ <- IO("[notifier]: Time's up!").trace
      } yield ()

    for {
      deferred <- Deferred[IO, Int]
      counter  <- Ref[IO].of(0)
      _        <- (increment(counter, deferred), notifier(deferred)).parTupled.void
    } yield ()
  }

  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]),
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])
  ]

  type EitherOutcome[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  def racePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = {
    IO.uncancelable { poll =>
      for {
        deferred <- Deferred[IO, EitherOutcome[A, B]]
        _        <- deferred.get
        fiberA   <- ioa.guaranteeCase(outcomeA => deferred.complete(Left(outcomeA)).void).start
        fiberB   <- iob.guaranteeCase(outcomeB => deferred.complete(Right(outcomeB)).void).start
        result <- poll(deferred.get).onCancel { // Blocking call - should be cancelable.
          for {
            cancalingFiberA <- fiberA.cancel.start
            cancalingFiberB <- fiberB.cancel.start
            _               <- cancalingFiberA.join
            _               <- cancalingFiberB.join
          } yield ()
        }
      } yield {
        result match {
          case Left(outcomeA)  => Left(outcomeA, fiberB)
          case Right(outcomeB) => Right(fiberA, outcomeB)
        }
      }
    }
  }

  def run: IO[Unit] = racePair(IO(""), IO(13)).void
}
