package courses.rockthejvm.part_02_effects_and_io

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {
  val annIO: IO[String]   = IO(s"[${Thread.currentThread().getName}]: Ann")
  val jamesIO: IO[String] = IO(s"[${Thread.currentThread().getName}]: James")

  val composedIO: IO[String] =
    for {
      ann   <- annIO
      james <- jamesIO
    } yield s"$ann and $james"

  // Using debug extension method
  import courses.rockthejvm.utils._
  import cats.syntax.apply._

  val number: IO[Int]        = IO.delay(13).debug
  val scala: IO[String]      = IO("Scala").debug
  val goalInLife: IO[String] = (number, scala).mapN((n, s) => s"Goal in life is: $s and $n")

  // Parallelism in IO
  val parOneIO: IO.Par[Int]    = Parallel[IO].parallel(number)
  val parTwoIO: IO.Par[String] = Parallel[IO].parallel(scala)

  import cats.effect.implicits._
  val goalInLifeParallel: IO.Par[String] = (parOneIO, parTwoIO).mapN((n, s) => s"Goal in life is: $s and $n")

  // Turn back to sequential
  val goalInLifeSequential: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // Shorthand
  import cats.syntax.parallel._
  val goalInLifeParallelShort: IO[String] = (number, scala).parMapN((n, s) => s"Goal in life is: $s and $n")

  // Regarding failure
  val failed: IO[String] = IO.raiseError(new RuntimeException("Something went wrong..."))

  // Compose success + failure
  val parallelWithFailure: IO[String] = (scala, failed).parMapN(_ + _)

  // Compose failure + failure
  val anotherFailed: IO[String]          = IO.raiseError(new RuntimeException("WTF!?"))
  val parallelWithTwoFailure: IO[String] = (failed, anotherFailed).parMapN(_ + _)

  // The first effect to fail gives the failure of the result
  val parallelWithTwoFailureDelayed: IO[String] = (IO(Thread.sleep(1000)) >> failed, anotherFailed).parMapN(_ + _)

  override def run: IO[Unit] = {
    import courses.rockthejvm.utils._

    // If not use *> method after first IO it was not printing anything.
    // If you want to check how to works composition with failed IO comment previous failed IO.
    composedIO.map(println) *>
      goalInLife.map(println) *>
      goalInLifeSequential.debug.void *>
      goalInLifeParallelShort.debug.void *>
      parallelWithFailure.debug.void *>
      parallelWithTwoFailure.debug.void *>
      parallelWithTwoFailureDelayed.debug.void
  }
}
