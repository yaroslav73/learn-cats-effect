package courses.rockthejvm.part_05_polymorphic_effects

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Fiber
import cats.effect.kernel.Spawn
import cats.effect.kernel.Outcome

object PolymorphicFibers extends IOApp.Simple {

  // Spawn = create fibers for any effects
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] // Create a fiber
    def never[A]: F[A]                                // A forever-suspending fiber
    def cede: F[Unit]                                 // A "yield" effect

    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[ // Fundamental racing
      (Outcome[F, E, A], Fiber[F, E, B]),
      (Fiber[F, E, A], Outcome[F, E, B])
    ]]
  }

  trait MySpawn[F[_], E] extends MyGenSpawn[F, E]

  val number                               = IO(73)
  val fiber: IO[Fiber[IO, Throwable, Int]] = number.start

  // pure, map/flatMap, raiseError, uncancelable, start

  val spawnIO = Spawn[IO] // Fetch the given/implicit Spawn[IO]

  def ioOnSomeThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] =
    for {
      fiber  <- spawnIO.start(io) // io.start assumes the presence of a Spawn[IO] in scope
      result <- fiber.join
    } yield result

  // Generalize
  import cats.effect.syntax.all.*
  import cats.syntax.all.*

  // If we want to use (using S: Spawn[F]) need add next imports:
  // import cats.syntax.flatMap.*
  // import cats.syntax.functor.*
  // and update for-comprehension with: S.start(fa)

  def effectOnSomeThread[F[_]: Spawn, A](fa: F[A]): F[Outcome[F, Throwable, A]] =
    for {
      fiber  <- fa.start
      result <- fiber.join
    } yield result

  val numberOnFiberOne = ioOnSomeThread(number)
  val numberOnFiberTwo = effectOnSomeThread(number)

  // Exercise:
  // Generalize the following code
  def simpleRace[F[_], A, B](fa: F[A], fb: F[B])(using S: Spawn[F]): F[Either[A, B]] =
    S.racePair(fa, fb).flatMap {
      case Left((outcomeA, fiberB)) =>
        outcomeA match {
          case Outcome.Succeeded(effectA) => fiberB.cancel.flatMap(_ => effectA.map(a => Left(a)))
          case Outcome.Errored(e)         => fiberB.cancel.flatMap(_ => S.raiseError(e))
          case Outcome.Canceled() =>
            fiberB.join.flatMap {
              case Outcome.Succeeded(effectB) => effectB.map(b => Right(b))
              case Outcome.Errored(e)         => S.raiseError(e)
              case Outcome.Canceled()         => S.raiseError(new RuntimeException("Both computation canceled"))
            }
        }
      case Right((fiberA, outcomeB)) =>
        outcomeB match {
          case Outcome.Succeeded(effectB) => fiberA.cancel.flatMap(_ => effectB.map(b => Right(b)))
          case Outcome.Errored(e)         => fiberA.cancel.flatMap(_ => S.raiseError(e))
          case Outcome.Canceled() =>
            fiberA.join.flatMap {
              case Outcome.Succeeded(effectA) => effectA.map(a => Left(a))
              case Outcome.Errored(e)         => S.raiseError(e)
              case Outcome.Canceled()         => S.raiseError(new RuntimeException("Both computation canceled"))
            }
        }
    }

  import scala.concurrent.duration.DurationInt
  import courses.rockthejvm.utils.*

  val fast = IO.sleep(1.second) >> IO(73).debug
  val slow = IO.sleep(2.second) >> IO("Slow").debug

  override def run: IO[Unit] = simpleRace(fast, slow).void
}
