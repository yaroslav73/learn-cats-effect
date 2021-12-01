package effects.chapter03

import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._

object Future02 extends App {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  // Here using method instead val as in Future01.
  def hello = Future(println(s"[${Thread.currentThread().getName}]: Hello"))
  def world = Future(println(s"[${Thread.currentThread().getName}]: World"))

  val hw01 = for {
    _ <- hello
    _ <- world
  } yield ()

  Await.ready(hw01, 5.seconds)

  val hw02 = (hello, world).mapN((_, _) => ())

  Await.ready(hw02, 5.seconds)

  val hw03 = (
    Future(println(s"[${Thread.currentThread().getName}]: Hello")),
    Future(println(s"[${Thread.currentThread().getName}]: World"))
  ).mapN((_, _) => ())

  Await.ready(hw03, 5.seconds)
}
