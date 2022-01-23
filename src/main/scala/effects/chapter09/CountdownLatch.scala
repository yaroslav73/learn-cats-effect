package effects.chapter09

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.{Deferred, Ref}
import cats.implicits.catsSyntaxFlatten

trait CountdownLatch {
  def await: IO[Unit]
  def decrement: IO[Unit]
}

object CountdownLatch {
  sealed trait State
  final case class Outstanding(n: Long, whenDone: Deferred[IO, Unit]) extends State
  final case object Done extends State

  def apply(counter: Long)(implicit cs: ContextShift[IO]): IO[CountdownLatch] =
    for {
      whenDone <- Deferred[IO, Unit]
      state <- Ref[IO].of[State](Outstanding(counter, whenDone))
    } yield new CountdownLatch {
      def await: IO[Unit] =
        state.get.flatMap {
          case Outstanding(_, whenDone) => whenDone.get
          case Done                     => IO.unit
        }

      def decrement: IO[Unit] =
        state
          .modify {
            case Outstanding(1, whenDone) => Done -> whenDone.complete(())
            case Outstanding(n, whenDone) => Outstanding(n - 1, whenDone) -> IO.unit
            case Done                     => Done -> IO.unit
          }
          .flatten
          .uncancelable
    }
}
