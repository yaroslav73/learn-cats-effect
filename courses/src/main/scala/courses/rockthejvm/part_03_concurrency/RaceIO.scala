package courses.rockthejvm.part_03_concurrency

import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt

object RaceIO extends IOApp.Simple {
  import courses.rockthejvm.utils._

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"Starting computation: $value ...").trace >>
        IO.sleep(duration) >>
        IO(s"Computation for $value done").trace >>
        IO(value)
    ).onCancel(IO(s"Computation CANCELLED for $value").trace.void)

  def testRace(): IO[String] = {
    val thirteen = runWithSleep(13, 1.second)
    val scala    = runWithSleep("Scala", 2.seconds)

    // - both IOs run on separate threads
    // - the first one to finish will complete the result
    // - the loser will be cancelled

    val first: IO[Either[Int, String]] = IO.race(thirteen, scala)

    first.flatMap {
      case Left(n)  => IO(s"Number won: $n")
      case Right(s) => IO(s"String won: $s")
    }
  }

  def testRacePair(): IO[OutcomeIO[_ >: Int with String]] = {
    val thirteen = runWithSleep(13, 1.second)
    val scala    = runWithSleep("Scala", 2.seconds)

    val raceResult: IO[Either[
      (OutcomeIO[Int], FiberIO[String]),
      (FiberIO[Int], OutcomeIO[String])
    ]] = IO.racePair(thirteen, scala)

    raceResult.flatMap {
      case Left((outcomeInt, fiberString))  => fiberString.cancel >> IO(s"Number won").trace >> IO(outcomeInt)
      case Right((fiberInt, outcomeString)) => fiberInt.cancel >> IO(s"String won").trace >> IO(outcomeString)
    }
  }

  override def run: IO[Unit] = testRacePair().trace.void
}
