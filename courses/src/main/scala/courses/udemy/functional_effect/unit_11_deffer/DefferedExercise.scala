package courses.udemy.functional_effect.unit_11_deffer

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.kernel.Deferred

import cats.syntax.parallel.catsSyntaxParallelSequence_

object DefferedExercise extends IOApp:
  final case class Producer[A](name: String, deffered: Deferred[IO, A], exec: IO[A]):
    def run(): IO[Unit] =
      IO.println(s"Producer: $name ") *>
        exec.flatMap { a =>
          IO.println(s"produce the value: $a") *> deffered.complete(a)
        }.void

  final case class Consumer[A](name: String, deffered: Deferred[IO, A], consume: A => IO[Unit]):
    def run(): IO[Unit] =
      IO.println(s"Consumer: $name ") *>
        deffered.get.flatMap(a => IO.println(s"consuming the value: $a") *> consume(a))

  def run(args: List[String]): IO[ExitCode] =
    Deferred[IO, Int]
      .flatMap { deffered =>
        val p = Producer("p1", deffered, IO.pure(73))
        val c = Consumer("c1", deffered, n => IO.println(n))

        List(p.run(), c.run()).parSequence_

        // Also can use this:
        // p.run().both(c.run()).void
      }
      .as(ExitCode.Success)
