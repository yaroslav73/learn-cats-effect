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

object MutexExercises extends IOApp.Simple {
  import courses.rockthejvm.utils.*

  abstract class Mutex {
    def acquire: IO[Unit]
    def release: IO[Unit]
  }

  object Mutex {
    type Signal = Deferred[IO, Unit]

    final case class State(locked: Boolean, waiting: Queue[Signal])

    val unlocked = State(locked = false, waiting = Queue.empty)

    def createSignal: IO[Signal] = Deferred[IO, Unit]

    def create: IO[Mutex] = Ref[IO].of(unlocked).map(solution)

    def solution(state: Ref[IO, State]): Mutex = {
      new Mutex {

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

    def answer(state: Ref[IO, State]): Mutex = {
      new Mutex {
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

    def solutionCancelable(state: Ref[IO, State]): Mutex = {
      new Mutex {
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

  def criticalTask: IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] =
    for {
      _      <- IO(s"[Task: $id]: working...").debug
      result <- criticalTask
      _      <- IO(s"[Task: $id]: gor result: $result").debug
    } yield result

  def demoNonLockingTasks: IO[List[Int]] = (1 to 10).toList.parTraverse(id => createNonLockingTask(id))

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] =
    for {
      _ <- IO(s"[Task $id]: waiting for permission...").debug
      _ <- mutex.acquire // Blocks if the mutex has been acquired by some other fiber
      // Critical section start
      _      <- IO(s"[Task $id]: working...").debug
      result <- criticalTask
      _      <- IO(s"[Task $id]: gor result: $result").debug
      // Critical section end
      _ <- mutex.release
      _ <- IO(s"[Task $id]: lock remove.").debug
    } yield result

  // Only one task will process at the time
  def demoLockingTasks: IO[List[Int]] =
    for {
      mutex   <- Mutex.create
      results <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
    } yield results

  def createCancellingTask(id: Int, mutex: Mutex): IO[Int] = {
    if (id % 2 == 0) createLockingTask(id, mutex)
    else
      for {
        fiber <- createLockingTask(id, mutex).onCancel(IO(s"[Task $id]: received cancellation!").debug.void).start
        _     <- IO.sleep(2.seconds) >> fiber.cancel
        out   <- fiber.join
        result <- out match {
          case Succeeded(effect) => effect
          case Errored(_)        => IO(-1)
          case Canceled()        => IO(-2)
        }
      } yield result
  }

  def demoCancellingTask =
    for {
      mutex   <- Mutex.create
      results <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
    } yield results

  def run: IO[Unit] =
    // demoNonLockingTasks.debug.void
    demoLockingTasks.debug.void
    // demoCancellingTask.debug.void
}
