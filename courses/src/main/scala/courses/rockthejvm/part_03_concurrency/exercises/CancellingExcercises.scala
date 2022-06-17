package courses.rockthejvm.part_03_concurrency.exercises

import cats.effect.IOApp
import cats.effect.IO

import scala.concurrent.duration.DurationInt

object CancellingExcercises extends IOApp.Simple {
  import courses.rockthejvm.utils._

  val cancelBeforeNumber       = IO.canceled >> IO(13).debug
  val uncancelableBeforeNumber = IO.uncancelable(_ => IO.canceled >> IO(13).debug)

  // Uncancelable will eliminate ALL cancel points

  import courses.rockthejvm.part_03_concurrency.CancellingIO._

  val invincibleAuthProgram: IO[Unit] =
    for {
      authFiber <- IO.uncancelable(_ => authFlow).start
      _         <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFiber.cancel
      _         <- authFiber.join
    } yield ()

  def threeStepProgram: IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("First cancelable").debug >> IO.sleep(1.second) >> IO("... first cancelable end").debug) >>
        IO("Uncancelable").debug >> IO.sleep(1.second) >> IO("... uncancelable end").debug >>
        poll(IO("Second cancelable").debug >> IO.sleep(1.second) >> IO("... second cancelable end").debug)
    }

    for {
      fiber <- sequence.start
      _     <- IO.sleep(1500.millis) >> IO("CANCELED").debug >> fiber.cancel
      _     <- fiber.join
    } yield ()
  }

  override def run: IO[Unit] = threeStepProgram
}
