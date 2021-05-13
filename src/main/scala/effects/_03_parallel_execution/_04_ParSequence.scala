package effects._03_parallel_execution

import cats.effect._
import cats.implicits._
import effects.debug._

object _04_ParSequence extends IOApp {

  val numTasks = 20
  val tasks: List[IO[Int]] = List.tabulate(numTasks)(task)

  def task(id: Int): IO[Int] = IO(id + 100).debug

  override def run(args: List[String]): IO[ExitCode] =
    tasks
      .parSequence
      .debug
      .as(ExitCode.Success)
}
