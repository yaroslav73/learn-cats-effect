package courses.udemy.functional_effect.unit_05_parallelism

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import cats.syntax.parallel.catsSyntaxTuple2Parallel
import cats.syntax.traverse.toTraverseOps
import cats.syntax.parallel.catsSyntaxParallelTraverse1

import scala.concurrent.duration.DurationInt

object ConcurrencyAndParallelismAppExercise extends IOApp:
  // Fetch n quotes from the fastest source
  // Calculate the average age of the authors

  final case class Quote(author: String, text: String)

  def fetchHttp(n: Int): IO[List[Quote]] =
    IO.sleep(25.millis) *> (1 to n).toList.map(n => Quote(s"Author $n", s"text $n")).pure[IO]

  def fetchDatabase(n: Int): IO[List[Quote]] =
    IO.sleep(75.millis) *> (1 to n).toList.map(n => Quote(s"Author $n", s"text $n")).pure[IO]

  def fetchAuthorAge(author: String): IO[Int] =
    IO.sleep(150.millis) *> IO((math.random() * 100).toInt)

  def run(args: List[String]): IO[ExitCode] =
    val n = 10
    fetchHttp(n)
      .race(fetchDatabase(n))
      .flatMap {
        case Left(httpQuotes)      => httpQuotes.parTraverse(quote => fetchAuthorAge(quote.author))
        case Right(databaseQuotes) => databaseQuotes.parTraverse(quote => fetchAuthorAge(quote.author))
      }
      .flatTap(ages => IO.println(s"Average age: ${ages.sum / ages.size}"))
      .as(ExitCode.Success)

    // Instructor solution
    IO.race(fetchHttp(n), fetchDatabase(n))
      .flatMap { _.fold(identity, identity).parTraverse(q => fetchAuthorAge(q.author)) }
      .map(ages => ages.sum / ages.size)
      .flatTap(IO.println)
      .as(ExitCode.Success)
