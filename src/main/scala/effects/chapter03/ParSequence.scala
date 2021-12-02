package effects.chapter03

import cats.effect._
import cats.implicits._
import effects.debug._

// (par)sequence - has signature F[G[A]] => G[F[A]]
// In this example we transform List[IO[A]] => IO[List[A]]
object ParSequence extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    tasks.parSequence.debug
      .as(ExitCode.Success)

  val numTasks = 20

  val tasks: List[IO[Int]] = List.tabulate(numTasks)(task)

  def task(id: Int): IO[Int] = IO(id + 100).debug
}
