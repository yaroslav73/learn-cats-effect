package courses.rockthejvm.part_05_polymorphic_effects

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Spawn
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred
import cats.effect.Concurrent

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

  import courses.rockthejvm.utils.*
  import scala.concurrent.duration.FiniteDuration
  import scala.concurrent.duration.DurationInt
  def eggBoiler: IO[Unit] = {
    def eggReadyNotification(signal: Deferred[IO, Unit]): IO[Unit] =
      for {
        _ <- IO("Egg boiling on some other fiber, waiting...").debug
        _ <- signal.get
        _ <- IO("EGG READY!").debug
      } yield ()

    def tickingClock(counter: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] =
      for {
        _     <- IO.sleep(1.second)
        count <- counter.updateAndGet(_ + 1)
        _     <- IO(count).debug
        _     <- if (count >= 10) signal.complete(()) else tickingClock(counter, signal)
      } yield ()

    for {
      counter           <- Ref[IO].of(0)
      signal            <- Deferred[IO, Unit]
      notificationFiber <- eggReadyNotification(signal).start
      clock             <- tickingClock(counter, signal).start
      _                 <- notificationFiber.join
      _                 <- clock.join
    } yield ()
  }

  override def run: IO[Unit] = ???
}
