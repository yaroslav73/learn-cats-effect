package courses.rockthejvm.part_03_concurrency.exercises

import cats.effect.{Fiber, FiberIO, IO, IOApp}
import cats.effect.kernel.Outcome

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt

object FibersExercises extends IOApp.Simple {
  import courses.rockthejvm.utils._

  // 1. Write the function that runs an IO on another thread,
  //    and depending on the result of fiber:
  //    - return the result of the IO
  //    - if error or cancelled, return a failed IO
  def processResultsFromFiber[A](io: IO[A]): IO[A] =
    io.start.flatMap(fiber => fiber.join).flatMap {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e)    => IO.raiseError(e)
      case Outcome.Canceled()    => IO.raiseError(new IllegalStateException("IO was cancel."))
    }

  // 2. Write a function that takes two IOs, runs them on different fibers
  //    and return an IO with a tuple containing both results:
  //    - if both IOs complete successfully, tuple their results
  //    - if the first IO results an error, raise that error (ignoring the second IO's result/error)
  //    - if the first IO doesn't error but second IO return an error, raise that error
  //    - if one or both cancelled, raise a RuntimeException
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val tuple = for {
      fa <- ioa.trace.start
      fb <- iob.trace.start
      a  <- fa.join
      b  <- fb.join
    } yield (a, b)

    tuple.flatMap {
      case (Outcome.Succeeded(aa), Outcome.Succeeded(bb))    => aa.flatMap(a => bb.map(b => (a, b)))
      case (Outcome.Errored(e), _)                           => IO.raiseError(e)
      case (_, Outcome.Errored(e))                           => IO.raiseError(e)
      case (Outcome.Canceled(), _) | (_, Outcome.Canceled()) => IO.raiseError(new RuntimeException("IO(s) was cancel."))
    }
  }

  // 3. Write a function that adds a timeout to an IO:
  //    - IO runs on a fiber
  //    - if the timeout duration passes, then the fiber is cancelled
  //    - the method returns an IO[A] which contains:
  //      - the original value if the computation is successful before the timeout signal
  //      - the exception if the computation is failed before the timeout signal
  //      - a RuntimeException if it times out (i.e. cancelled by the timeout)
  def timeout[A](io: IO[A], timeout: FiniteDuration): IO[A] = {
    val result = for {
      fa <- io.start
      _  <- IO.sleep(timeout) *> fa.cancel
      a  <- fa.join
    } yield a

    result.flatMap {
      case Outcome.Succeeded(fa) => fa
      case Outcome.Errored(e)    => IO.raiseError(e)
      case Outcome.Canceled()    => IO.raiseError(new RuntimeException("IO was cancel."))
    }
  }

  override def run: IO[Unit] = {
    val linesIO      = IO(println("----------------------------------------------------"))
    val successOneIO = IO("Success IO")
    val successTwoIO = IO(73)
    val failedIO     = IO.raiseError(new IllegalArgumentException("Wrong number!"))
    val cancelledIO  = IO.canceled

    processResultsFromFiber(successOneIO).trace *>
      linesIO *>
      tupleIOs(successOneIO, successTwoIO) *>
      linesIO *>
      timeout(successTwoIO *> IO.sleep(2.seconds), 1.second).trace.void
  }
}
