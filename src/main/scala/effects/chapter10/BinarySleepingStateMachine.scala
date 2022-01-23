package effects.chapter10

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.{Deferred, Ref}
import cats.implicits.catsSyntaxFlatten

trait BinarySleepingStateMachine {
  def sleep: IO[Unit] // Sleep (semantically block) until wakeUp is invoked
  def wakeup: IO[Unit] // Wake up (semantically unblock) any sleepers. No effect if already awake.
}

object BinarySleepingStateMachine {
  sealed trait State
  final case class Asleep(whenDone: Deferred[IO, Unit]) extends State
  final case object Awake extends State

  //  1. Define an interface whose methods return effects.
  //  2. Implement the interface by building a state machine where:
  //    a. state (with type S) is atomically managed via a Ref[IO, S] value;
  //    b. each interface method is implemented by a state transition function affecting the Ref;
  //    c. any state-dependent blocking behavior is controlled via Deferred values.
  def apply(implicit cs: ContextShift[IO]): IO[BinarySleepingStateMachine] =
    for {
      whenDone <- Deferred[IO, Unit]
      state <- Ref[IO].of[State](Asleep(whenDone))
    } yield new BinarySleepingStateMachine {
      def sleep: IO[Unit] = state.get.flatMap {
        case Asleep(whenDone) => whenDone.get
        case Awake            => IO.unit
      }

      def wakeup: IO[Unit] =
        state
          .modify {
            case Asleep(whenDone) => Awake -> whenDone.complete(())
            case Awake            => Awake -> IO.unit
          }
          .flatten
          .uncancelable
    }
}
