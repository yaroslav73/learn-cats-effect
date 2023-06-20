package courses.udemy.functional_effect.unit_14_tagless_final

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode

import cats.syntax.parallel.catsSyntaxParallelTraverse1
import cats.syntax.parallel.catsSyntaxTuple2Parallel
import cats.effect.Sync
import cats.Parallel

object TaglessFinalExercise extends IOApp:
  final case class Image(bytes: List[Byte])

  trait ImageService[F[_]]:
    def fetchHttp(n: Int): F[List[Image]]
    def fetchDatabase(n: Int): F[List[Image]]
    def fetchBoth(n: Int): F[List[Image]]

  object ImageService:
    def make[F[_]: Sync: Parallel]: ImageService[F] =
      new ImageService:
        def fetchHttp(n: Int): F[List[Image]] =
          List.range(0, n).parTraverse { i => Sync[F].blocking(Image(List(i.toByte))) }

        def fetchDatabase(n: Int): F[List[Image]] =
          List.range(0, n).parTraverse { i => Sync[F].blocking(Image(List((100 + i).toByte))) }

        def fetchBoth(n: Int): F[List[Image]] =
          (fetchHttp(n), fetchDatabase(n)).parMapN { case (httpImages, databaseImages) => httpImages ++ databaseImages }

  override def run(args: List[String]): IO[ExitCode] =
    val imageService = ImageService.make[IO]
    imageService.fetchBoth(10).flatTap(IO.println).as(ExitCode.Success)
