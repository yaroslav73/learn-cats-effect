package effects.chapter08

import cats.effect.{ContextShift, IO, Timer}
import cats.effect.laws._

object TestContext extends App {
  val ctx = util.TestContext()

  implicit val cs: ContextShift[IO] = ctx.ioContextShift
  implicit val timer: Timer[IO] = ctx.timer
}
