package effects.chapter05

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import effects.debug._

object Parallelism extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(s"Number of CPUs: $numCPUs").debug
      _ <- tasks.debug
    } yield ExitCode.Success

  // We ask for the number of CPUs available so we can ensure
  // we submit more than this number of tasks.
  val numCPUs: Int = Runtime.getRuntime.availableProcessors()

  // We want to run a large number of tasks in parallel;
  // let’s try twice the number of CPUs available.
  val tasks: IO[List[Int]] = List.range(0, numCPUs * 2).parTraverse(task)

  //Each effect we execute is a “no-op”; it doesn’t do anything.
  def task(i: Int): IO[Int] = IO(i).debug
}
