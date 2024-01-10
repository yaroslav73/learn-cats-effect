import cats.effect.{IO, IOApp}
import cats.implicits._

object WhenA extends IOApp.Simple:
  def test(io: IO[Unit]): IO[Unit] =
    io *> IO(println("World!"))

  def program: IO[Unit] =
    for {
      cond <- IO(10 < 3)
//      _    <- test(IO(println("Hello"))).whenA(cond)
      _ <- test(IO(println("Hello")).whenA(cond))
    } yield ()

  def run: IO[Unit] = program
