package effects.chapter03

import cats.effect._
import cats.implicits._
import effects.debug._

import scala.concurrent.duration._

object _04_ParMapNErrors extends IOApp {
  private val ok = IO("Hi!").debug
  private val ko1 =
    IO.sleep(1.second).as("ko1").debug *>
      IO.raiseError[String](new RuntimeException("Oh, crap!")).debug
  private val ko2 = IO.raiseError[String](new RuntimeException("WTF!?")).debug

  //  private val e1 = (ok, ko1).parMapN((_, _) => ())
  //  private val e2 = (ko1, ok).parMapN((_, _) => ())
  //  private val e3 = (ko1, ko2).parMapN((_, _) => ())

  //  private val e1 = (ok, ko1).parMapN((l, r) => (l, r)).void
  //  private val e2 = (ko1, ok).parMapN((l, r) => (l, r)).void
  //  private val e3 = (ko1, ko2).parMapN((l, r) => (l, r)).void

  private val e1 = (ok, ko1).parTupled.void
  private val e2 = (ko1, ok).parTupled.void
  private val e3 = (ko1, ko2).parTupled.void

  override def run(args: List[String]): IO[ExitCode] =
    e1.attempt.debug *>
      IO("-------").debug *>
      e2.attempt.debug *>
      IO("-------").debug *>
      e3.attempt.debug *>
      IO.pure(ExitCode.Success)
}
