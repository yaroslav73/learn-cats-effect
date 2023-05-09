package courses.rockthejvm.part_03_concurrency

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.duration.DurationInt

object CancellingIO extends IOApp.Simple {

  import courses.rockthejvm.utils._

  // Cancelling IOs:
  // - fiber.cancel
  // - IO.race & other APIs
  // - manual cancelation

  val chainedIOs = IO("waiting...").trace >> IO.canceled >> IO(13).trace

  // Uncancelable
  // Example: online store, payment processor
  // Payment processor must NOT be cancelled

  val paymentProcessor: IO[String] = (
    IO("Payment running, don't cancel me...").trace >>
      IO.sleep(1.second) >>
      IO("Payment completed.").trace
  ).onCancel(IO("MEGA CANCEL!").trace.void)

  val atomicPaymentProcessor: IO[String]        = IO.uncancelable(_ => paymentProcessor)
  val anotherAtomicPaymentProcessor: IO[String] = paymentProcessor.uncancelable

  val paymentProcessorCancellation =
    for {
      fiber <- paymentProcessor.start
      _     <- IO.sleep(500.millis) >> fiber.cancel
      _     <- fiber.join
    } yield ()

  val paymentProcessorUncancelable =
    for {
      fiber <- atomicPaymentProcessor.start
      _     <- IO.sleep(500.millis) >> IO("Attempting cancellation...").trace >> fiber.cancel
      _     <- fiber.join
    } yield ()

  // The uncancelable API is more complex amd more general.
  // It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
  // The Poll object can be used to mark sections within the returned effect which CAN BE CANCELLED.

  // Example: authentication service. Has two parts:
  // - input password, CAN BE cancelled, because otherwise we might block indefinitiely on user input
  // - verify password, CANNOT cancelled once it's started

  val inputPassword: IO[String] =
    IO("Input password:").trace >> IO("... typing password ...").trace >> IO.sleep(3.seconds) >> IO("qwerty123")
  val verifyPassword: String => IO[Boolean] = (password: String) =>
    IO("Verifying...").trace >> IO.sleep(3.seconds) >> IO(password == "qwerty123")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      // This is cancelable again, because using poll(...)
      password <- poll(inputPassword).onCancel(IO("Authentication timeout. Try again later.").trace.void)
      verified <- verifyPassword(password)
      _        <- if (verified) IO("Authentication successful.").trace else IO("Authentication failed.").trace
    } yield ()
  }

  // Uncancelable calls are MASKS which suppress cancellation.
  // Poll calls are "gaps opened" in the uncancelable region.

  val authProgram: IO[Unit] =
    for {
      authFiber <- authFlow.start
      _         <- IO.sleep(2.seconds) >> IO("Authentication timeout, attempting cancel...").trace >> authFiber.cancel
      _         <- authFiber.join
    } yield ()

  override def run: IO[Unit] = authProgram
}
