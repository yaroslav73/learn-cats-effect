package effects.chapter09

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import effects.debug._

object CountdownLatchApp extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- prepareAndRun
    } yield ExitCode.Success

  val prepareAndRun: IO[Unit] =
    for {
      latch <- CountdownLatch(1)
      _ <- (actionWithPrerequisites(latch), runPrerequisite(latch)).parTupled
    } yield ()

  def actionWithPrerequisites(latch: CountdownLatch): IO[String] =
    for {
      _ <- IO("waiting for prerequisites").debug
      _ <- latch.await
      result <- IO("action").debug
    } yield result

  def runPrerequisite(latch: CountdownLatch): IO[String] =
    for {
      result <- IO("prerequisite").debug
      _ <- latch.decrement
    } yield result
}
