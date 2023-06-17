package courses.udemy.functional_effect.unit_13_thread_pools

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.traverse.toTraverseOps
import cats.syntax.parallel.catsSyntaxParallelTraverse1

import scala.concurrent.duration.DurationInt

object ComputeThreadPoolUsingExample extends IOApp:
  def task(id: Long): IO[Unit] =
    IO(println(s"Running task $id on the thread: ${Thread.currentThread.getName}"))

  def blockingTask(id: Long): IO[Unit] = IO.blocking {
    println(s"Running blocking task $id on the thread: ${Thread.currentThread.getName}")
    Thread.sleep(2000)
    println(s"Waking up blocking task $id on the thread: ${Thread.currentThread.getName}")
  }

  // IO.blocking {...} - will be using CachedUnbounded Thread Tool for computation
  // IO(...) - will be using WorkStealing Thread Pool (Default thread pool for CE)
  override def run(args: List[String]): IO[ExitCode] =
    (1 to 1000).toList
      .parTraverse { id => blockingTask(id) }
      .timeoutTo(5.seconds, IO.unit)
      .as(ExitCode.Success)
