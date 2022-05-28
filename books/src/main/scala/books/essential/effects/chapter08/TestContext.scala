package books.essential.effects.chapter08

import cats.effect.{ContextShift, IO, Timer}
import cats.effect.laws._

import scala.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.util.Failure

object TestContext extends App {
  val ctx = util.TestContext()

  implicit val cs: ContextShift[IO] = ctx.ioContextShift
  implicit val timer: Timer[IO] = ctx.timer

  val timeoutError = new TimeoutException
  val timeout = IO.sleep(10.seconds) *> IO.raiseError[Int](timeoutError)
  val f = timeout.unsafeToFuture()

  ctx.tick(5.seconds)
  assert(f.value.isEmpty)

  ctx.tick(5.seconds)
  assert(f.value.contains(Failure(timeoutError)))
}
