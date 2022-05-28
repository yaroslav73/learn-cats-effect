package books.essential.effects.experiments.io

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import cats.effect.{ContextShift, IO}

object AsyncEffects extends App {
  def convert[A](fa: => Future[A])(implicit ec: ExecutionContext): IO[A] =
    IO.async { cb =>
      // This triggers evaluation of the by-name param and of onComplete,
      // so it's OK to have side effects in this callback
      fa.onComplete {
        case Success(a) => cb(Right(a))
        case Failure(e) => cb(Left(e))
      }
    }

  implicit val ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  val f = Future { println("Hello, from future") }

  implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  val iof = IO.fromFuture(IO(f))

  // Why print only once?
  val p = for {
    _ <- iof
    _ <- iof
  } yield ()

  p.unsafeRunSync()

  val ioc = convert(f)

  // Why nothing print?
  val p1 = for {
    _ <- ioc
    _ <- ioc
  } yield ()

  p1.unsafeRunSync()

  // Тому що Future в scala - eager evaluated.
  // Вона починає виконуватись відразу після того як ми її задефайнили.
}
