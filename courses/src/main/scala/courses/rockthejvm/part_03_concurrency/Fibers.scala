package courses.rockthejvm.part_03_concurrency

import cats.effect.{Fiber, FiberIO, IO, IOApp}
import cats.effect.kernel.Outcome
import concurrent.duration.DurationInt

object Fibers extends IOApp.Simple {
  import courses.rockthejvm.utils._

  val thirteen = IO.pure(13)
  val scala    = IO.pure("Scala")

  def sameThreadIOs: IO[Unit] =
    for {
      _ <- thirteen.trace
      _ <- scala.trace
    } yield ()

  // Fibers introduction
  // - almost impossible to create fibers manually
  def createFiber: Fiber[IO, Throwable, String] = ???

  // - the fiber is not actually started,
  //   but the fiber allocation is wrapped in another thread
  val fiber: IO[Fiber[IO, Throwable, Int]] = thirteen.trace.start

  def differentThreadIOs: IO[Unit] =
    for {
      ft <- thirteen.trace.start
      fs <- scala.trace.start
      _  <- ft.join.trace
      _  <- fs.join.trace
    } yield ()

  // Joining fiber
  // join - is an effect which waits for the fiber to terminate
  def runInAnotherThread[A, B](ioa: IO[A], iob: IO[B]): IO[(Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])] =
    for {
      fa <- ioa.trace.start.trace
      fb <- iob.trace.start.trace
      a  <- fa.join
      b  <- fb.join
    } yield (a, b)

  // What's going on in method above?
  // IO[Result type of fa.join]
  // fa.join = Outcome[IO, Throwable, A] and then wrap it into IO = IO[Outcome[IO, Throwable, A]]

  // Possible Outcomes:
  // - final case class Succeeded[F[_], E, A](fa: F[A]) extends Outcome[F, E, A]
  // - final case class Errored[F[_], E, A](e: E) extends Outcome[F, E, A]
  // - final case class Canceled[F[_], E, A]() extends Outcome[F, E, A]

  def runInAnotherThread[A](ioa: IO[A]): IO[Outcome[IO, Throwable, A]] =
    for {
      fa <- ioa.trace.start.trace
      a  <- fa.join
    } yield a

  val someIOInAnotherThread = runInAnotherThread(thirteen)
  val someResultFromAnotherThread = someIOInAnotherThread.flatMap {
    case Outcome.Succeeded(fa) => fa
    case Outcome.Errored(_)    => IO(0)
    case Outcome.Canceled()    => IO(0)
  }

  def throwOnAnotherThread: IO[Outcome[IO, Throwable, Int]] =
    for {
      fib    <- IO.raiseError[Int](new RuntimeException("no number for you :(")).start
      result <- fib.join
    } yield result

  def testCancel: IO[Outcome[IO, Throwable, String]] = {
    val task                        = IO("starting...").trace >> IO.sleep(1.second) >> IO("done!").trace
    val taskWithCancellationHandler = task.onCancel(IO("cancelled!").trace.void)

    for {
      fib    <- taskWithCancellationHandler.start                 // Run on a separate thread
      _      <- IO.sleep(500.millis) >> IO("cancelling...").trace // Running on the calling thread
      _      <- fib.cancel
      result <- fib.join
    } yield result
  }

  override def run: IO[Unit] =
    sameThreadIOs *>
      IO(println("------------------------------------------")) *>
      differentThreadIOs *>
      IO(println("------------------------------------------")) *>
      runInAnotherThread(thirteen, scala).trace.void *>
      IO(println("------------------------------------------")) *>
      throwOnAnotherThread.trace.void *>
      IO(println("------------------------------------------")) *>
      testCancel.trace.void
}
