package books.essential.effects.chapter01.exercise

import books.essential.effects.chapter01.MyIO

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Exercise extends App {
  // Write a clock action that returns the current time in milliseconds,
  // i.e., via System.currentTimeMillis.
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // Write a timer that records the duration of another action.
  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] = for {
    start <- clock
    result <- action
    finish <- clock
  } yield (FiniteDuration(finish - start, TimeUnit.MILLISECONDS), result)

  val timedHello = time(MyIO.putStr("hello"))

  timedHello.unsafeRun() match {
    case (duration, _) => println(s"'hello' took $duration")
  }
}
