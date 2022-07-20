package courses.rockthejvm.part_04_concurrent_coordination.exercises

import courses.rockthejvm.utils.*
import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred
import cats.syntax.parallel.*

import scala.concurrent.duration.*

object CountDownLatchExercises extends IOApp.Simple {

  abstract class CountDownLatch {
    def await: IO[Unit]
    def release: IO[Unit]
  }

  object CountDownLatch {
    def deferred: IO[Deferred[IO, Unit]] = Deferred[IO, Unit]

    sealed trait State
    final case class Waiting(latches: Int, signal: Deferred[IO, Unit]) extends State
    case object Done                                                   extends State

    def apply(count: Int): IO[CountDownLatch] =
      for {
        signal <- Deferred[IO, Unit]
        state  <- Ref[IO].of[State](Waiting(count, signal))
      } yield createCountDownLatch(state)

    private def createCountDownLatch(ref: Ref[IO, State]): CountDownLatch = new CountDownLatch {
      def await: IO[Unit] = ref.get.flatMap {
        case Waiting(_, signal) => signal.get
        case Done               => IO.unit
      }

      def release: IO[Unit] = ref.modify {
        case Waiting(n, signal) => if (n > 1) Waiting(n - 1, signal) -> IO.unit else Done -> signal.complete(()).void
        case d @ Done           => d -> IO.unit
      }.flatten
    }
  }

  object CDLAnswer {
    sealed trait State
    case object Done                                              extends State
    final case class Live(count: Int, signal: Deferred[IO, Unit]) extends State

    def apply(count: Int): IO[CountDownLatch] =
      for {
        signal <- Deferred[IO, Unit]
        state  <- Ref[IO].of[State](Live(count, signal))
      } yield new CountDownLatch {
        def await: IO[Unit] = state.get.flatMap { s =>
          if (s == Done) IO.unit // Continue, the latch is dead
          else signal.get        // Back here
        }

        def release: IO[Unit] = state.modify {
          case Done            => Done                -> IO.unit
          case Live(1, signal) => Done                -> signal.complete(()).void
          case Live(n, signal) => Live(n - 1, signal) -> IO.unit
        }.flatten
      }
  }

  def announcer(latch: CountDownLatch): IO[Unit] =
    for {
      _ <- IO("The race will starts soon...").debug >> IO.sleep(2.seconds)
      _ <- IO("5...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("4...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("3...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("2...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("1...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("GO GO GO!").debug
    } yield ()

  def createRunner(id: Int, latch: CountDownLatch): IO[Unit] =
    for {
      _ <- IO(s"[Runner $id] waiting for a signal...").debug
      _ <- latch.await // Block this fiber until the count reaches 0
      _ <- IO(s"[Runner $id] RUNNING!").debug
    } yield ()

  def sprint: IO[Unit] =
    for {
      latch          <- CountDownLatch(5)
      announcerFiber <- announcer(latch).start
      _              <- (1 to 10).toList.parTraverse(id => createRunner(id, latch))
    } yield ()

  override def run: IO[Unit] = sprint
}
