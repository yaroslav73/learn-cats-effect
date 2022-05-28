package books.essential.effects.chapter03

import cats.effect._
import cats.implicits._
import books.essential.effects.debug._

import scala.concurrent.duration._

object ParMapNErrors extends IOApp {
  private val ok = IO("Hi!").debug
  private val ko1 =
    IO.sleep(1.second).as("ko1").debug *>
      IO.raiseError[String](new RuntimeException("Oh, crap!")).debug
  private val ko2 = IO.raiseError[String](new RuntimeException("WTF!?")).debug

  // The result of parMapN will fail if any—at least one—of the effects fail.
  private val e1 = (ok, ko1).parMapN((_, _) => ())
  private val e2 = (ko1, ok).parMapN((_, _) => ())
  // What happens if there are failures during parMapN?
  // The first failure to happen is used as the failure of the composed effect.
  private val e3 = (ko1, ko2).parMapN((_, _) => ())

  // Using void to ignore effect result and return Unit
  private val e1Void = (ok, ko1).parMapN((l, r) => (l, r)).void
  private val e2Void = (ko1, ok).parMapN((l, r) => (l, r)).void
  private val e3Void = (ko1, ko2).parMapN((l, r) => (l, r)).void

  // cats provides a (par-)mapN function that doesn’t do anything except
  // tuple the inputs together, called (par-)tupled
  private val e1TupledVoid = (ok, ko1).parTupled.void
  private val e2TupledVoid = (ko1, ok).parTupled.void
  private val e3TupledVoid = (ko1, ko2).parTupled.void

  override def run(args: List[String]): IO[ExitCode] =
    e1.attempt.debug *>
      IO("-------").debug *>
      e2Void.attempt.debug *>
      IO("-------").debug *>
      e3TupledVoid.attempt.debug *>
      IO.pure(ExitCode.Success)
}
