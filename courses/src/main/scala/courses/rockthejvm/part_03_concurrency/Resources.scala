package courses.rockthejvm.part_03_concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {
  import courses.rockthejvm.utils._

  // Use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String]  = IO(s"Open connection to: $url ...").debug
    def close: IO[String] = IO(s"Close connection to: $url ...").debug
  }

  val url = "hello.example.com"

  def wrongAsyncFetchUrl(url: String): IO[Unit] =
    for {
      fiber <- (Connection(url).open *> IO.sleep(Int.MaxValue.seconds)).start
      _     <- IO.sleep(1.second) *> fiber.cancel
    } yield ()
  // Problem with code above - leaking resource, we open connection but not close it

  def correctAsyncFetchUrl(url: String): IO[Unit] =
    for {
      connection <- IO(Connection(url))
      fiber      <- (connection.open *> IO.sleep(Int.MaxValue.seconds)).onCancel(connection.close.void).start
      _          <- IO.sleep(1.second) *> fiber.cancel
    } yield ()

  // Not bad, but CE has the Bracket Pattern:
  // - IO.bracket(useResourceCallback)(releaseResourceCallback)
  // - bracket is equivalent to try-catch, but pure FP
  def bracketFetchUrl(url: String): IO[Unit] =
    for {
      fiber <- IO(Connection(url))
        .bracket(connection => connection.open *> IO.sleep(Int.MaxValue.seconds))(connection => connection.close.void)
        .start
      _ <- IO.sleep(1.second) *> fiber.cancel
    } yield ()

  override def run: IO[Unit] =
    bracketFetchUrl(url)
}
