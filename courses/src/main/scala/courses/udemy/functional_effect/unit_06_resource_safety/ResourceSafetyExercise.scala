package courses.udemy.functional_effect.unit_06_resource_safety

import cats.syntax.applicative.catsSyntaxApplicativeId

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import java.net.URL
import java.net.HttpURLConnection
import java.io.BufferedReader
import java.io.InputStreamReader
import cats.effect.Resource
import java.io.InputStream

object ResourceSafetyExercise extends IOApp:
  def createConnection(url: String): IO[HttpURLConnection] =
    IO.blocking {
      val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection
    }

  def makeConnection(url: String): Resource[IO, HttpURLConnection] =
    Resource.make(createConnection(url))(connection => connection.disconnect.pure[IO])

  def makeBufferReader(is: InputStream): Resource[IO, BufferedReader] =
    Resource.fromAutoCloseable(IO(new BufferedReader(new InputStreamReader(is))))

  def makeResources(url: String): Resource[IO, (HttpURLConnection, BufferedReader)] =
    for {
      connection   <- makeConnection(url)
      bufferReader <- makeBufferReader(connection.getInputStream)
    } yield (connection, bufferReader)

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

  def httpGetResouces(url: String): IO[String] =
    makeResources(url).use { case (_, bufferReader) => readOutput(bufferReader) }

  def run(args: List[String]): IO[ExitCode] =
    httpGetResouces("https://www.google.com")
      .flatTap(IO.println)
      .as(ExitCode.Success)
