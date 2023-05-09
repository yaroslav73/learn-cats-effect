package courses.rockthejvm.part_05_polymorphic_effects

import cats.Parallel
import cats.data.EitherT
import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Spawn
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred
import cats.effect.kernel.Outcome
import cats.effect.Fiber
import cats.effect.Concurrent
import cats.syntax.*
import cats.implicits.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import courses.rockthejvm.utils.general.*

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object PolymorphicCoordination extends IOApp.Simple {

  // Concurrent = Ref + Deferred for ANY effect type
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO]    // given/implicit instances of Concurrent[IO]
  val aDeferredOne = Deferred[IO, Int] // given/implicit Concurrent[IO] in scope
  val aDeferredTwo = concurrentIO.deferred[Int]
  val aRef         = concurrentIO.ref(73)

  // Capabilities: pure, map/flatMap, raiseError, uncancelable, start (fibers), + ref/deferred

  def eggBoiler[F[_]: Concurrent]: F[Unit] = {
    def eggReadyNotification[F[_]: Concurrent](signal: Deferred[F, Unit]): F[Unit] =
      for {
        _ <- "Egg boiling on some other fiber, waiting...".pure[F].trace
        _ <- signal.get
        _ <- "EGG READY!".pure[F].trace
      } yield ()

    def tickingClock[F[_]: Concurrent](counter: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] =
      for {
        _     <- unsafeSleep[F, Throwable](1.second)
        count <- counter.updateAndGet(_ + 1)
        _     <- count.pure[F].trace
        _     <- if (count >= 10) signal.complete(()).void else tickingClock(counter, signal)
      } yield ()

    for {
      counter           <- Ref[F].of(0)
      signal            <- Deferred[F, Unit]
      notificationFiber <- eggReadyNotification(signal).start
      clock             <- tickingClock(counter, signal).start
      _                 <- notificationFiber.join
      _                 <- clock.join
    } yield ()
  }

  // Exercise: generalize raicePair
  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]),
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B])
  ]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  def racePair[F[_], A, B](fa: F[A], fb: F[B])(using F: Concurrent[F]): F[RaceResult[F, A, B]] = {
    F.uncancelable { poll =>
      for {
        deferred <- Deferred[F, EitherOutcome[F, A, B]]
        _        <- deferred.get
        fiberA   <- fa.guaranteeCase(outcomeA => deferred.complete(Left(outcomeA)).void).start
        fiberB   <- fb.guaranteeCase(outcomeB => deferred.complete(Right(outcomeB)).void).start
        result <- poll(deferred.get).onCancel { // Blocking call - should be cancelable.
          for {
            cancalingFiberA <- fiberA.cancel.start
            cancalingFiberB <- fiberB.cancel.start
            _               <- cancalingFiberA.join
            _               <- cancalingFiberB.join
          } yield ()
        }
      } yield {
        result match {
          case Left(outcomeA)  => Left(outcomeA, fiberB)
          case Right(outcomeB) => Right(fiberA, outcomeB)
        }
      }
    }
  }

  def eitherT[F[_]: Concurrent](s: String): EitherT[F, Int, String] = EitherT.pure(s)

  import concurrent.ExecutionContext.Implicits.global

  def future(futureEither: Future[Either[Int, String]]): Unit =
    futureEither
      .map {
        case Left(value)  => println(s"${Thread.currentThread.getName}: $value")
        case Right(value) => println(s"${Thread.currentThread.getName}: $value")
      }
      .failed
      .foreach(_ => println(s"${Thread.currentThread.getName}: ERROR!"))

  def program[F[_]: Concurrent: Parallel]: F[Unit] = {
    import cats.implicits._

    val commands = for {
      s <- (1 to 100).toList.map(_.toString)
    } yield for {
      command <- eitherT(s)
      res = future(Future(Right(command)))
    } yield res // future(Future(Right(command)))

    // When use foldMap instead of parFoldMap we lost some numbers, why?
    commands.parFoldMapA(_.value.start.void)
  }

  override def run: IO[Unit] = // racePair(IO(""), IO(13)).trace.void
    program[IO]
}
