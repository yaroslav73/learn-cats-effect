package courses.udemy.functional_effect.unit_06_resource_safety

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import java.net.URL
import java.net.HttpURLConnection
import java.io.BufferedReader
import java.io.InputStreamReader

object ResourceSafetyExercise extends IOApp:
  def createConnection(url: String): IO[HttpURLConnection] =
    IO.blocking {
      val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection
    }

  def readOutput(reader: BufferedReader): IO[String] =
    IO.blocking {
      Iterator
        .continually(reader.readLine)
        .takeWhile(_ != null)
        .mkString("\n")
    }

  def httpGet(url: String): IO[String] =
    for {
      connection  <- createConnection(url)
      inputStream <- IO(connection.getInputStream)
      reader      <- IO(new BufferedReader(new InputStreamReader(inputStream)))
      response    <- readOutput(reader)
    } yield response

  def run(args: List[String]): IO[ExitCode] =
    httpGet("https://www.google.com")
      .flatTap(IO.println)
      .as(ExitCode.Success)
