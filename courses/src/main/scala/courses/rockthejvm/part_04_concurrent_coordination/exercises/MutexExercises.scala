package courses.rockthejvm.part_04_concurrent_coordination.exercises

import cats.syntax.parallel.*
import cats.effect.IOApp
import cats.effect.IO
import scala.util.Random
import scala.concurrent.duration.DurationInt
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred
import scala.collection.immutable.Queue

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

    def create: IO[Mutex] = {
      Ref[IO].of(unlocked).map { state =>
        new Mutex {

          /** Change the state of the Ref:
            *   - if the mutex is currently unlocked, state becomes State(false, queue.empty)
            *   - if the mutex is locked, state becomes State(true, queue + new signal) AND wait on that signal
            */
          def acquire: IO[Unit] =
            for {
              d <- Deferred[IO, Unit]
              s <- state.get
              _ <-
                if (s.locked) state.set(State(s.locked, s.waiting :+ d))
                else state.update(s => State(locked = true, s.waiting :+ d)) *> d.get
              _ <- IO(s"[acquire, ${System.currentTimeMillis}]: current state: $s").debug
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
              _ <- IO(s"[release]: current state: $s").debug
              _ <-
                if (s.locked) {
                  if (s.waiting.isEmpty) state.set(State(locked = false, s.waiting))
                  else s.waiting.head.complete(()) >> state.set(State(s.locked, s.waiting.tail))
                } else IO.unit
            } yield ()
        }
      }
    }
  }

  def criticalTask: IO[Int] = IO.sleep(2.seconds) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] =
    for {
      _      <- IO(s"[Task: $id]: working...").debug
      result <- criticalTask
      _      <- IO(s"[Task: $id]: gor result: $result").debug
    } yield result

  def demoNonLockingTasks: IO[List[Int]] = (1 to 10).toList.parTraverse(id => createNonLockingTask(id))

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] =
    for {
      _ <- IO(s"[Task: $id]: waiting for permission...").debug
      _ <- mutex.acquire // Blocks if the mutex has been acquired by some other fiber
      // Critical section start
      _      <- IO(s"[Task: $id]: working...").debug
      result <- criticalTask
      _      <- IO(s"[Task: $id]: gor result: $result").debug
      // Critical section end
      _ <- mutex.release
      _ <- IO(s"[Task: $id]: lock remove.").debug
    } yield result

  def demoLockingTasks: IO[List[Int]] =
    for {
      mutex <- Mutex.create
      tasks <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
    } yield tasks

  def run: IO[Unit] =
    // demoNonLockingTasks.debug.void
    demoLockingTasks.debug.void
}
