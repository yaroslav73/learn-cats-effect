package courses.udemy.functional_effect.unit_12_queue

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.std.Queue

import concurrent.duration.DurationInt

object Intro extends IOApp:
  enum Event:
    case UserAdded(id: Long)
    case UserDeleted(id: Long)

  def producer(queue: Queue[IO, Event]): IO[Nothing] =
    val generateEvent: IO[Event] = IO {
      val id = (math.random() * 1000).toLong
      if math.random() < 0.5 then Event.UserAdded(id)
      else Event.UserDeleted(id)
    }
    (IO.sleep(100.millis) *> generateEvent.flatMap(event => queue.offer(event))).foreverM

  def consumer(queue: Queue[IO, Event]): IO[Nothing] =
    (IO.sleep(1.second) *> queue.take.flatTap(IO.println)).foreverM

  def run(args: List[String]): IO[ExitCode] =
    Queue
      // .unbounded[IO, Event]
      .bounded[IO, Event](0)
      .flatMap { queue =>
        producer(queue).both(consumer(queue))
      }
      .timeoutTo(5.seconds, IO.unit)
      .as(ExitCode.Success)
