package courses.rockthejvm.part_04_concurrent_coordination

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.parallel.*
import cats.syntax.traverse.*

import scala.concurrent.duration.*
import courses.rockthejvm.utils.*
import cats.effect.std.CountDownLatch
import cats.effect.Resource
import java.io.FileWriter
import java.io.File
import scala.io.Source
import scala.util.Random

object CountDownLatches extends IOApp.Simple {
  // CoundDownLatches are primitive initialized with a count.
  // All fibers calling await() on the CountDownLatch are (semantically) blocked.
  // When the internal count of the latch reaches 0 (via release() calls from other fibers),
  // all waiting fibers are unblocked.

  def announcer(latch: CountDownLatch[IO]): IO[Unit] =
    for {
      _ <- IO("The race will starts soon...").debug >> IO.sleep(2.seconds)
      _ <- IO("5...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("4...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("3...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("2...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("1...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("GO GO GO!").debug
    } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] =
    for {
      _ <- IO(s"[Runner $id] waiting for a signal...").debug
      _ <- latch.await // Block this fiber until the count reaches 0
      _ <- IO(s"[Runner $id] RUNNING!").debug
    } yield ()

  def sprint: IO[Unit] =
    for {
      latch          <- CountDownLatch[IO](5)
      announcerFiber <- announcer(latch).start
      _              <- (1 to 10).toList.parTraverse(id => createRunner(id, latch))
    } yield ()

  // Exercise: simulate a file downloader on a multiple threads
  object FileServer {
    val fileChunks = Array(
      "I love Cats Effect",
      "I love Scala",
      "Cats Effect seems quite fun",
      "Never would I have thought I would do a low-level concurrencty with Pure FP"
    )

    def numberOfChunks: IO[Int] = IO(fileChunks.size)

    def fileChunks(n: Int): IO[String] = IO(fileChunks(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] = {
    val fireResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fireResource.use { writer =>
      IO(writer.write(contents))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource =
      for {
        reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
        writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
      } yield (reader, writer)

    compositeResource.use { case (reader, writer) =>
      IO(reader.getLines().foreach(writer.write))
    }
  }

  private def craeteFileDownloaderTask(
    id: Int,
    latch: CountDownLatch[IO],
    filename: String,
    destination: String
  ): IO[Unit] =
    for {
      _     <- IO(s"[Task $id] downloading chunk...").debug
      _     <- IO.sleep((Random.nextDouble * 1000).toInt.millis)
      chunk <- FileServer.fileChunks(id)
      _     <- writeToFile(s"$destination/$filename.part_$id", chunk)
      _     <- IO(s"[Task $id] chunk download complete.").debug
      _     <- latch.release
    } yield ()

  // Call file server API and get the number of chunks (n)
  // Start a CountDownLatch
  // Start n fibers which download a chunk of the file (use the file server's download chunk API)
  // Block on the latch until each task has finished
  // After all chunks are done, stitch the files together under the same file on disk
  def downloadFile(filename: String, destination: String): IO[Unit] =
    for {
      n     <- FileServer.numberOfChunks
      latch <- CountDownLatch[IO](n)
      _     <- IO(s"Download started on $n fibers.").debug
      _     <- (0 until n).toList.parTraverse(id => craeteFileDownloaderTask(id, latch, filename, destination))
      _     <- latch.await
      _ <- (0 until n).toList.traverse { id =>
        appendFileContents(s"$destination/$filename.part_$id", s"$destination/$filename")
      }
    } yield ()

  override def run: IO[Unit] =
    sprint
    // downloadFile("example.txt", "courses/src/main/resources")
}
