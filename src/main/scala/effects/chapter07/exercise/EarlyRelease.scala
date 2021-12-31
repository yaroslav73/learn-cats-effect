package effects.chapter07.exercise

import cats.effect._
import effects.debug._

import scala.io.Source

object EarlyRelease extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    dbConnectionResource
      .use { connection =>
        connection.query("SELECT * FROM users WHERE id = 12").debug
      }
      .as(ExitCode.Success)

  lazy val dbConnectionResource: Resource[IO, DBConnection] =
    for {
      config <- configResource
      connection <- DBConnection.make(config.connectionURL)
    } yield connection

  lazy val configResource: Resource[IO, Config] =
    Resource.eval(sourceResource.use(source => Config.fromSource(source)))

  lazy val sourceResource: Resource[IO, Source] = Resource.make(
    IO(s"> opening Source to config").debug *> IO(Source.fromString(config))
  )(source => IO(s"< closing Source to config").debug *> IO(source.close()))

  val config = "exampleConnectionURL"

  final case class Config(connectionURL: String)
  object Config {
    def fromSource(source: Source): IO[Config] =
      for {
        config <- IO(Config(source.getLines().next()))
        _ <- IO(s"read: $config").debug
      } yield config
  }

  trait DBConnection {
    def query(sql: String): IO[String]
  }
  object DBConnection {
    def make(connectionURL: String): Resource[IO, DBConnection] = Resource.make(
      IO(s"> opening Connection to $connectionURL").debug *> IO(
        new DBConnection {
          def query(sql: String): IO[String] = IO(s"(result for SQL: $sql)")
        }
      )
    )(_ => IO(s"< closing Connection to $connectionURL").debug.void)
  }
}
