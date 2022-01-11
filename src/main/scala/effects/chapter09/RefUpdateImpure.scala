package effects.chapter09

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxApplicative, catsSyntaxParallelTraverse1}

object RefUpdateImpure extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ref <- Ref[IO].of(0)
      _ <- List(1, 2, 3).parTraverse(task(_, ref))
    } yield ExitCode.Success

  def task(id: Int, ref: Ref[IO, Int]): IO[Unit] =
    ref
      .modify(previous => id -> println(s"$previous -> $id"))
      .replicateA(3)
      .void
}
