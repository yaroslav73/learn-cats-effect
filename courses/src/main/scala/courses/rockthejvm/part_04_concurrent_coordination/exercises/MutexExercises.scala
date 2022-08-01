package courses.rockthejvm.part_04_concurrent_coordination.exercises

import cats.syntax.parallel.*
import cats.effect.IOApp
import cats.effect.IO
import scala.util.Random
import scala.concurrent.duration.DurationInt
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred
import scala.collection.immutable.Queue
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled
import cats.effect.Concurrent
import cats.effect.Spawn
import cats.implicits.*
import cats.effect.implicits.*
import cats.effect.syntax.all.*
 import cats.syntax.all.*
import cats.effect.kernel.MonadCancel
import cats.Monad
import cats.FlatMap
import cats.effect.kernel.Fiber

object MutexExercises extends IOApp.Simple {
  // import courses.rockthejvm.utils.*
  import courses.rockthejvm.utils.general.*

  abstract class MutexIO {
    def acquire: IO[Unit]
    def release: IO[Unit]
  }

  object MutexIO {
    type Signal = Deferred[IO, Unit]

    final case class State(locked: Boolean, waiting: Queue[Signal])

    val unlocked = State(locked = false, waiting = Queue.empty)

    def createSignal: IO[Signal] = Deferred[IO, Unit]

    def create: IO[MutexIO] = Ref[IO].of(unlocked).map(solution)

    def solution(state: Ref[IO, State]): MutexIO = {
      new MutexIO {

        /** Change the state of the Ref:
          *   - if the mutex is currently unlocked, state becomes State(false, queue.empty)
          *   - if the mutex is locked, state becomes State(true, queue + new signal) AND wait on that signal
          */
        def acquire: IO[Unit] =
          for {
            d <- Deferred[IO, Unit]
            _ <- state.modify {
              case State(false, _)    => State(locked = true, Queue.empty)      -> IO.unit
              case State(true, queue) => State(locked = true, queue.enqueue(d)) -> d.get
            }.flatten
            // _ <-
            // if (s.locked) state.modify(s => State(locked = true, s.waiting.enqueue(d)) -> d.get)
            // else state.modify(s => State(locked = true, Queue.empty) -> IO.unit)
          } yield ()

        /** Change the state of the Ref:
          *   - if the mutex is currently unlocked, leave state unchanged
          *   - if the mutex is locked:
          *     - if the queue is empty, unlock the mutex, i.e. state becomes State(false, queue.empty)
          *     - if the queue is non empty, take a signal out of the queue and complete it (thereby unlocking a fiber
          *       waiting on it)
          */
        def release: IO[Unit] =
          for {
            s <- state.get
            _ <-
              if (s.locked) {
                if (s.waiting.isEmpty) state.modify(s => State(locked = false, Queue.empty) -> IO.unit).flatten
                else
                  state.modify { s =>
                    val (head, tail) = s.waiting.dequeue
                    State(locked = true, tail) -> head.complete(()).void
                  }.flatten
              } else state.modify(s => State(locked = false, Queue.empty) -> IO.unit).flatten
          } yield ()
      }
    }

    def answer(state: Ref[IO, State]): MutexIO = {
      new MutexIO {
        def acquire: IO[Unit] = createSignal.flatMap { signal =>
          state.modify {
            case State(false, _)    => State(locked = true, Queue.empty)           -> IO.unit
            case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> signal.get
          }.flatten
        }

        def release: IO[Unit] =
          state.modify {
            case State(false, _)                       => unlocked -> IO.unit
            case State(true, queue) if (queue.isEmpty) => unlocked -> IO.unit
            case State(true, queue) =>
              val (head, tail) = queue.dequeue
              State(locked = true, tail) -> head.complete(()).void
          }.flatten
      }
    }

    def solutionCancelable(state: Ref[IO, State]): MutexIO = {
      new MutexIO {
        def acquire: IO[Unit] = IO.uncancelable { poll =>
          createSignal.flatMap { signal =>

            val cleanup = state.modify { case State(locked, queue) =>
              State(locked, queue.filterNot(_ eq signal)) -> release
            }.flatten

            state.modify {
              case State(false, _) => State(locked = true, Queue.empty) -> IO.unit
              case State(true, queue) =>
                State(locked = true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup)
            }.flatten
          }
        }

        // Not need wrap with IO.uncancelable, because .modify atomic operation.
        def release: IO[Unit] =
          state.modify {
            case State(false, _)                       => unlocked -> IO.unit
            case State(true, queue) if (queue.isEmpty) => unlocked -> IO.unit
            case State(true, queue) =>
              val (head, tail) = queue.dequeue
              State(locked = true, tail) -> head.complete(()).void
          }.flatten
      }
    }
  }

  trait Mutex[F[_]] {
    def acquire: F[Unit]
    def release: F[Unit]
  }

  object Mutex {
    def create[F[_]: Concurrent]: F[Mutex[F]] = {
      type Signal = Deferred[F, Unit]

      final case class State(locked: Boolean, waiting: Queue[Signal])

      val unlocked = State(locked = false, waiting = Queue.empty)

      def createSignal: F[Signal] = Deferred[F, Unit]

      Ref[F].of(unlocked).map { state =>
        new Mutex {
          def acquire: F[Unit] = createSignal.flatMap { signal =>
            state.modify {
              case State(false, _)    => State(locked = true, Queue.empty)           -> ().pure[F]
              case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> signal.get
            }.flatten
          }

          def release: F[Unit] =
            state.modify {
              case State(false, _)                       => unlocked -> ().pure[F]
              case State(true, queue) if (queue.isEmpty) => unlocked -> ().pure[F]
              case State(true, queue) =>
                val (head, tail) = queue.dequeue
                State(locked = true, tail) -> head.complete(()).void
            }.flatten
        }
      }
    }
  }

  def criticalTask[F[_]: Spawn]: F[Int] = unsafeSleep[F, Throwable](1.second) >> Random.nextInt(100).pure[F]

  def createNonLockingTask(id: Int): IO[Int] =
    for {
      _      <- IO(s"[Task: $id]: working...").debug
      result <- criticalTask[IO]
      _      <- IO(s"[Task: $id]: gor result: $result").debug
    } yield result

  def demoNonLockingTasks: IO[List[Int]] = (1 to 10).toList.parTraverse(id => createNonLockingTask(id))

  def createLockingTask[F[_]: Spawn](id: Int, mutex: Mutex[F]): F[Int] =
    for {
      _ <- s"[Task $id]: waiting for permission...".pure[F].debug
      _ <- mutex.acquire // Blocks if the mutex has been acquired by some other fiber
      // Critical section start
      _      <- s"[Task $id]: working...".pure[F].debug
      result <- criticalTask[F]
      _      <- s"[Task $id]: gor result: $result".pure[F].debug
      // Critical section end
      _ <- mutex.release
      _ <- s"[Task $id]: lock remove.".pure[F].debug
    } yield result

  // Only one task will process at the time
  def demoLockingTasks: IO[List[Int]] =
    for {
      mutex   <- Mutex.create[IO]
      results <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
    } yield results

  def createCancellingTask[F[_]: Spawn](id: Int, mutex: Mutex[F]): F[Int] = {
    if (id % 2 == 0) createLockingTask(id, mutex)
    else
      for {
        fiber <- createLockingTask(id, mutex).onCancel(s"[Task $id]: received cancellation!".pure[F].debug.void).start
        _     <- unsafeSleep[F, Throwable](2.seconds) >> fiber.cancel
        out   <- fiber.join
        result <- out match {
          case Succeeded(effect) => effect
          case Errored(_)        => -1.pure[F]
          case Canceled()        => -2.pure[F]
        }
      } yield result
  }

  def demoCancellingTask =
    for {
      mutex   <- Mutex.create[IO]
      results <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
    } yield results

  def run: IO[Unit] =
    // demoNonLockingTasks.debug.void
    demoLockingTasks.debug.void
    // demoCancellingTask.debug.void
}
