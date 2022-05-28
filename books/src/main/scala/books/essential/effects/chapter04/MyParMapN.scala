package books.essential.effects.chapter04

import cats.effect.{ContextShift, IO}
import cats.implicits.catsSyntaxTuple2Semigroupal

// start both the ia and ib computations so they run concurrently (“fork” them);
// wait for each result;
// cancel the “other” effect if ia or ib fails;
// finally combine the results with the f function.
object MyParMapN {
  def myParMapN[A, B, C](ioa: IO[A], iob: IO[B])(
      f: (A, B) => C
  )(implicit cs: ContextShift[IO]): IO[C] =
    IO.racePair(ioa, iob).flatMap {
      case Left((a, fb))  => (IO.pure(a), fb.join).mapN(f)
      case Right((fa, b)) => (fa.join, IO.pure(b)).mapN(f)
    }
}
