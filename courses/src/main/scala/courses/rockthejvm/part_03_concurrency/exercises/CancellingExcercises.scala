package courses.rockthejvm.part_03_concurrency.exercises

import cats.effect.IOApp
import cats.effect.IO

import scala.concurrent.duration.DurationInt

object CancellingExcercises extends IOApp.Simple {
  import courses.rockthejvm.utils._

  val cancelBeforeNumber       = IO.canceled >> IO(13)
  val uncancelableBeforeNumber = IO.uncancelable(_ => IO.canceled >> IO(13).trace)

  // Uncancelable will eliminate ALL cancel points

  import courses.rockthejvm.part_03_concurrency.CancellingIO._

  val invincibleAuthProgram: IO[Unit] =
    for {
      authFiber <- IO.uncancelable(_ => authFlow).start
      _         <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").trace >> authFiber.cancel
      _         <- authFiber.join
    } yield ()

  def threeStepProgram: IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("First cancelable").trace >> IO.sleep(1.second) >> IO("... first cancelable end").trace) >>
        IO("Uncancelable").trace >> IO.sleep(1.second) >> IO("... uncancelable end").trace >>
        poll(IO("Second cancelable").trace >> IO.sleep(1.second) >> IO("... second cancelable end").trace)
    }

    for {
      fiber <- sequence.start
      _     <- IO.sleep(1500.millis) >> IO("CANCELED").trace >> fiber.cancel
      _     <- fiber.join
    } yield ()
  }

  override def run: IO[Unit] = threeStepProgram
}
