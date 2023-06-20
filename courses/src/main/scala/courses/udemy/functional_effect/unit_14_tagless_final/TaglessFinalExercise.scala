package courses.udemy.functional_effect.unit_14_tagless_final

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode

import cats.syntax.parallel.catsSyntaxParallelTraverse1
import cats.syntax.parallel.catsSyntaxTuple2Parallel

object TaglessFinalExercise extends IOApp:
  final case class Image(bytes: List[Byte])

  trait ImageService:
    def fetchHttp(n: Int): IO[List[Image]]
    def fetchDatabase(n: Int): IO[List[Image]]
    def fetchBoth(n: Int): IO[List[Image]]

  object ImageService:
    def make: ImageService =
      new ImageService:
        def fetchHttp(n: Int): IO[List[Image]] =
          List.range(0, n).parTraverse { i => IO.blocking(Image(List(i.toByte))) }

        def fetchDatabase(n: Int): IO[List[Image]] =
          List.range(0, n).parTraverse { i => IO.blocking(Image(List((100 + i).toByte))) }
        def fetchBoth(n: Int): IO[List[Image]] =
          (fetchHttp(n), fetchDatabase(n)).parMapN { case (httpImages, databaseImages) => httpImages ++ databaseImages }

  override def run(args: List[String]): IO[ExitCode] =
    val imageService = ImageService.make
    imageService.fetchBoth(10).flatTap(IO.println).as(ExitCode.Success)
