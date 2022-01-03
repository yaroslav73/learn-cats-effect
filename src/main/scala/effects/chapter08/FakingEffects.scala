package effects.chapter08

import cats.effect.{ExitCode, IO, IOApp}

// Faking effects with interfaces
object FakingEffects extends App {
  registrationFailsIfEmailDeliveryFails(EmailAddress("test@email.failed"))
  registrationSuccess(EmailAddress("test@email.success"))

  def registrationFailsIfEmailDeliveryFails(email: EmailAddress): Unit =
    new UserRegistration(new FailingEmailDelivery)
      .register(email)
      .attempt
      .map(result =>
        assert(result.isLeft, s"expecting failure, but was $result")
      )
      .unsafeRunSync()

  def registrationSuccess(email: EmailAddress): Unit =
    new UserRegistration(new FailingEmailDelivery)
      .register(email)
      .attempt
      .map(result =>
        assert(result.isRight, s"expecting success, but result was: $result")
      )
      .unsafeRunSync()

  trait EmailDelivery {
    def send(to: EmailAddress, email: Email): IO[Unit]
  }

  final case class EmailAddress(address: String)
  final case class Email(content: String)

  class FailingEmailDelivery extends EmailDelivery {
    override def send(to: EmailAddress, email: Email): IO[Unit] =
      IO.raiseError(
        new RuntimeException(s"couldn't send email to ${to.address}")
      )
  }

  class UserRegistration(emailDelivery: EmailDelivery) {
    def register(email: EmailAddress): IO[Unit] =
      for {
        _ <- save(email)
        _ <- emailDelivery.send(email, Email("Activate account"))
      } yield ()

    private def save(email: EmailAddress): IO[Unit] =
      IO(println(s"Email ${email.address} saved"))
  }
}
