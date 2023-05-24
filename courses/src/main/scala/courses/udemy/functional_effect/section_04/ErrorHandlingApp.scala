package courses.udemy.functional_effect.section_04

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

object ErrorHandlingApp extends IOApp:
  object Controller:
    final case class Request(fromAccount: String, toAccount: String, amount: String)
    final case class Response(sattus: Int, body: String)

    def postTransfer(request: Request): IO[Response] = ???

  def run(args: List[String]): IO[ExitCode] =
    import Controller._

    val request = Request("12345", "67890", "1200")

    postTransfer(request)
      .flatTap(IO.println)
      .as(ExitCode.Success)
