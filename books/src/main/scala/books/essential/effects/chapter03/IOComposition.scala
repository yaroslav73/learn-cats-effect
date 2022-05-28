package books.essential.effects.chapter03

import cats.effect._
import cats.implicits._

object IOComposition extends App {
  val hello = IO(println(s"[${Thread.currentThread().getName}]: Hello"))
  val world = IO(println(s"[${Thread.currentThread().getName}]: World"))

  val hw01 = for {
    _ <- hello
    _ <- world
  } yield ()

  val hw02 = (hello, world).mapN((_, _) => ())

  hw01.unsafeRunSync()
  hw02.unsafeRunSync()
}
