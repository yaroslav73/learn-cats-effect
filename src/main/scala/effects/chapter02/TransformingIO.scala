package effects.chapter02

import cats.effect.IO
import cats.implicits._

object TransformingIO extends App {
  val thirteen = IO(12).map(_ + 1)

  println(thirteen.unsafeRunSync())

  println {
    (IO(12), IO("Hello")).mapN((f, s) => s"$s, $f").unsafeRunSync()
  }

  val result = for {
    i <- IO(12)
    k <- IO(i + 1)
    j <- IO("Hello")
  } yield s"$j, $k"

  println(result.unsafeRunSync())
}
