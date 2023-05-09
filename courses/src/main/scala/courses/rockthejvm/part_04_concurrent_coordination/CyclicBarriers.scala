package courses.rockthejvm.part_04_concurrent_coordination

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.std.CyclicBarrier
import cats.syntax.parallel.*
import scala.util.Random
import scala.concurrent.duration.DurationInt
import cats.effect.kernel.Deferred
import cats.effect.kernel.Ref

object CyclicBarriers extends IOApp.Simple {
  import courses.rockthejvm.utils.*

  // A cyclic barrier is a coordiantion primitive that
  //  - is initialized with a count
  //  - has a single API: await
  //
  // A cyclic barriers will (semantically) block all fibers calling it's await() method
  // until we have exactly N fibers waiting, at which point the barrier will unblock all fibers
  // and reset to it's original state.
  // Any further fiber will again block until we have exactly N fibers waiting.
  // ...
  // And son on.

  // Example: signing up for a social network just about to be launched.
  def createUser(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] =
    for {
      _ <- IO.sleep((Random.nextDouble * 500).toInt.millis)
      _ <- IO(s"[User $id] Just heard there's a new socual network - signing up for the waitlist...").trace
      _ <- IO.sleep((Random.nextDouble * 1500).toInt.millis)
      _ <- IO(s"[User $id] On the waitlist now, can't wait!!!").trace
      _ <- barrier.await // Block the fiber when there are exactly N users waiting
      _ <- IO(s"[User $id] OMG! This is so cool!!!").trace
    } yield ()

  def openNetwork: IO[Unit] =
    for {
      _ <- IO(
        "[Announcer] The Rock the JVM social network is up for registration! Launching when we have 10 users!"
      ).trace
      barrier <- CyclicBarrier[IO](10)
      _       <- (1 to 7).toList.parTraverse(id => createUser(id, barrier))
    } yield ()

  override def run: IO[Unit] = openNetwork

  abstract class CBarrier {
    def await: IO[Unit]
  }

  object CBarrier {
    final case class State(count: Int, signal: Deferred[IO, Unit])

    def apply(count: Int): IO[CBarrier] =
      for {
        signal <- Deferred[IO, Unit]
        state  <- Ref[IO].of(State(count, signal))
      } yield new CBarrier {
        def await: IO[Unit] = Deferred[IO, Unit].flatMap { newSignal =>
          state.modify {
            case State(1, signal) => State(count, newSignal) -> signal.complete(()).void
            case State(n, signal) => State(n - 1, signal)    -> signal.get
          }
        }
      }
  }
}
