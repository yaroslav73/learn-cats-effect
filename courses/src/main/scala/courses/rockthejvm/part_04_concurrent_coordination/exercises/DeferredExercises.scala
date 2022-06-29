package courses.rockthejvm.part_04_concurrent_coordination.exercises

import cats.syntax.parallel.*
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred

import scala.concurrent.duration.DurationInt

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
        _ <- IO(s"[increment]: $n").debug
        _ <- IO.sleep(1.second)
        _ <- if (n == 10) deferred.complete(n) else increment(counter, deferred)
      } yield ()

    def notifier(deferred: Deferred[IO, Int]): IO[Unit] =
      for {
        _ <- IO("[notifier]: Start timer!").debug
        _ <- deferred.get
        _ <- IO("[notifier]: Time's up!").debug
      } yield ()

    for {
      deferred <- Deferred[IO, Int]
      counter  <- Ref[IO].of(0)
      _        <- (increment(counter, deferred), notifier(deferred)).parTupled.void
    } yield ()
  }

  def run: IO[Unit] = timer
}
