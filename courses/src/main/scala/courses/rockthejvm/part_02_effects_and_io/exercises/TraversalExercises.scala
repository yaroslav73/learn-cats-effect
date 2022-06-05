package courses.rockthejvm.part_02_effects_and_io.exercises

import cats.Traverse
import cats.effect.{IO, IOApp}
import courses.rockthejvm.part_02_effects_and_io.IOTraversal

object TraversalExercises extends IOApp.Simple {
  import courses.rockthejvm.utils._

  import cats.syntax.traverse._
  import cats.syntax.parallel._

  def sequence[A](xs: List[IO[A]]): IO[List[A]] =
    xs.traverse(identity)

  def sequenceF[F[_]: Traverse, A](xs: F[IO[A]]): IO[F[A]] =
    xs.traverse(identity)

  def parSequence[A](xs: List[IO[A]]): IO[List[A]] =
    xs.parTraverse(identity)

  def parSequenceF[F[_]: Traverse, A](xs: F[IO[A]]): IO[F[A]] =
    xs.parTraverse(identity)

  override def run: IO[Unit] = {
    val workLoad                      = IOTraversal.workLoad
    def computeIO(s: String): IO[Int] = IOTraversal.heavyComputationIO(s)

    val ios = workLoad.map(computeIO)

    sequence(ios).debug.void
    sequenceF(ios).debug.void
    parSequence(ios).debug.void
    parSequenceF(ios).debug.void
  }
}
