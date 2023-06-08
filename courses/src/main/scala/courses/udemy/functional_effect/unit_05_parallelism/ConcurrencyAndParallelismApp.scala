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

object ConcurrencyAndParallelismApp extends IOApp:
  final case class Image(bytes: List[Byte])

  def httpImage(n: Int): IO[List[Image]] =
    IO.sleep(100.millis) *> (1 to n).toList.map(n => Image(List(n.toByte))).pure[IO]

  def databaseImage(n: Int): IO[List[Image]] =
    IO.sleep(100.millis) *> (1 to n).toList.map(n => Image(List((100 + n).toByte))).pure[IO]

  final case class Person(name: String)

  def save(p: Person): IO[Long] = IO.sleep(100.millis) *> p.name.length.toLong.pure[IO]

  def run(args: List[String]): IO[ExitCode] =
    val n = 10
    // httpImage(n).flatTap(IO.println).as(ExitCode.Success)

    // mapN example
    (httpImage(n), databaseImage(n))
      .mapN { case (httpImages, databaseImages) =>
        httpImages ++ databaseImages
      }
      .flatTap(IO.println)
      .as(ExitCode.Success)

    // parMapN example
    (httpImage(n), databaseImage(n))
      .parMapN { case (httpImages, databaseImages) =>
        httpImages ++ databaseImages
      }
      .flatTap(IO.println)
      .as(ExitCode.Success)

    // traverse example
    // people.map(p => save(p)) give us the type List[IO[Long]]
    // we can do it .sequence and got IO[List[Long]]
    // or we can do traverse instead of map + sequence
    // val people = List(Person("James"), Person("John"), Person("Yaroslav"), Person("Marie"))
    val people = (1 to 75).toList.map(n => Person(n.toString))
    people.traverse(p => save(p)).flatTap(IO.println).as(ExitCode.Success)

    // parTraverse
    people.parTraverse(p => save(p)).flatTap(IO.println).as(ExitCode.Success)

    // race example
    httpImage(n).race(databaseImage(n)).flatTap(IO.println).as(ExitCode.Success)
