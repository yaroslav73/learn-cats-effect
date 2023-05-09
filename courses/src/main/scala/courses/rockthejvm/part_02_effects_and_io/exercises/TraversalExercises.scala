package courses.rockthejvm.part_02_effects_and_io.exercises

import cats.Traverse
import cats.effect.{IO, IOApp}
import courses.rockthejvm.part_02_effects_and_io.IOTraversal

object TraversalExercises extends IOApp.Simple {
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
    import courses.rockthejvm.utils._

    val workLoad                      = IOTraversal.workLoad
    def computeIO(s: String): IO[Int] = IOTraversal.heavyComputationIO(s)

    val ios = workLoad.map(computeIO)

    sequence(ios).trace.void
    sequenceF(ios).trace.void
    parSequence(ios).trace.void
    parSequenceF(ios).trace.void
  }
}
