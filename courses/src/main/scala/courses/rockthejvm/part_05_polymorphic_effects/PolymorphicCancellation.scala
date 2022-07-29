package courses.rockthejvm.part_05_polymorphic_effects

import cats.effect.IOApp
import cats.effect.IO
import cats.Applicative
import cats.Monad
import cats.effect.Poll
import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import cats.FlatMap
import cats.Functor

object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  // Monad cancel
  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](poll: Poll[F] => F[A]): F[A] // Poll is a higher-kinded function type.
  }

  // MonadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // We can create value
  val molIO: IO[Int]          = monadCancelIO.pure(73)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _      <- monadCancelIO.pure("Once started, I can't go back...")
      result <- monadCancelIO.pure(73)
    } yield result
  }

  import cats.syntax.flatMap.* // flatMap
  import cats.syntax.functor.* // map
  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] =
    mc.uncancelable { _ =>
      for {
        _      <- mc.pure("Once started, I can't go back...")
        result <- mc.pure(73)
      } yield result
    }

  val mustComputeTryTwo = mustComputeGeneral[IO, Throwable]

  // Allow cancellation listeners
  val mustComputeWithListeners    = mustCompute.onCancel(IO("I'm being cancelled!").void)
  val mustComputeWithListenersTwo = monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled!").void)

  // .onCancel as extension method
  import cats.effect.syntax.monadCancel.* // onCancel

  // Allow finalizers
  val computationWithFinalizers = monadCancelIO.guaranteeCase(IO(73)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"Successful: $a").void)
    case Errored(e)    => IO(s"Failed: $e").void
    case Canceled()    => IO("Cancelled").void
  }

  // Bracket pattern is specific to MonadCancel
  val computationWithUsage = monadCancelIO.bracket(IO(73)) { value =>
    IO(s"Using the secret number: $value")
  } { _ => IO("Releasing the meaning of secret number...").void }

  // Exercise:
  // Generalize a pice of code

  def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis)) // Not semantic block

  import courses.rockthejvm.utils.general.*
  import cats.syntax.applicative.*

  def inputPassword[F[_], E](using MC: MonadCancel[F, E]): F[String] =
    for {
      _ <- "Input password:".pure[F].debug
      _ <- "... typing password ...".pure[F].debug
      _ <- unsafeSleep[F, E](3.seconds)
    } yield "qwerty123"

  def verifyPassword[F[_], E](password: String)(using MC: MonadCancel[F, E]): F[Boolean] =
    for {
      _ <- "Verifying...".pure[F].debug
      _ <- unsafeSleep[F, E](3.seconds)
    } yield password == "qwerty123"

  def authFlow[F[_], E](using MC: MonadCancel[F, E]): F[Unit] =
    MC.uncancelable { poll =>
      for {
        password <- poll(inputPassword).onCancel("Authentication timeout. Try again later.".pure[F].debug.void)
        verified <- verifyPassword(password)
        _        <- if (verified) "Authentication successful.".pure[F].debug else "Authentication failed.".pure[F].debug
      } yield ()
    }

  import cats.effect.Spawn
  import cats.effect.syntax.all.*
  import cats.syntax.all.*
  def authProgram[F[_]: Spawn](using MC: MonadCancel[F, Throwable]): F[Unit] =
    for {
      authFiber <- authFlow.start
      _ <- unsafeSleep[F, Throwable](2.seconds) >> "Authentication timeout, attempting cancel..."
        .pure[F]
        .debug >> authFiber.cancel
      _ <- authFiber.join
    } yield ()

  override def run: IO[Unit] = authProgram[IO]
}
