import cats.effect.IO
import cats.effect.IOApp

import cats.implicits._
import cats.syntax.all._
import cats.effect.syntax._
import java.time.LocalTime

import scala.concurrent.duration.DurationInt

object PlayWithIO extends IOApp.Simple {
  def run: IO[Unit] =
    // IO.println(s"${LocalTime.now()}: Printed message with delay.") *> IO.sleep(3.seconds) >> run
    for {
      _ <- IO.println(s"${LocalTime.now()}: Printed message with delay.")
      _ <- IO.sleep(3.seconds)
      _ <- run
    } yield ()
}
