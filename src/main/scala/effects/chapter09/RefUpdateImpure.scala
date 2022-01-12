package effects.chapter09

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{
  catsSyntaxApplicative,
  catsSyntaxParallelTraverse1,
  toTraverseOps
}

object RefUpdateImpure extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      ref <- Ref[IO].of(0)
      _ <- List(1, 2, 3).parTraverse(task(_, ref))
//      TODO: uncomment line below to check how it's looks without parallel computation
//      _ <- List(1, 2, 3).traverse(task(_, ref))
    } yield ExitCode.Success

  def task(id: Int, ref: Ref[IO, Int]): IO[Unit] =
    ref
      .modify(previous => id -> println(s"$previous -> $id"))
      .replicateA(3)
      .void
}
