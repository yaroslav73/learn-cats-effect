package books.essential.effects.chapter03

import cats.effect._
import cats.implicits._
import books.essential.effects.debug._

import scala.concurrent.duration._

// The most common use case of (par)traverse
// is when you have a collection of work to be done,
// and a function which handles one unit of work.
// Then you get a collection of results combined into one effect.
// (par)traverse signature - F[A] => (A => G[B]) => G[F[B]]
object ParTraverse extends IOApp {

  val numTasks = 20
  val tasks: List[Int] = List.range(0, numTasks)

  def task(id: Int): IO[Int] = IO(id + 100).debug

  override def run(args: List[String]): IO[ExitCode] =
    tasks
      .parTraverse(task)
      .debug
      .as(ExitCode.Success)
}
