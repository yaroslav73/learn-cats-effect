package effects.chapter03

import cats.effect._
import cats.implicits._
import effects.debug._

import scala.concurrent.duration._

object _04_ParTraverse extends IOApp {

  val numTasks = 20
  val tasks: List[Int] = List.range(0, numTasks)

  def task(id: Int): IO[Int] = IO(id + 100).debug

  override def run(args: List[String]): IO[ExitCode] =
    tasks
      .parTraverse(task)
      .debug
      .as(ExitCode.Success)
}
