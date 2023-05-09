package courses.rockthejvm.part_04_concurrent_coordination

import cats.syntax.traverse.*
import cats.effect.{IO, IOApp, Ref, Deferred}

import scala.concurrent.duration.DurationInt

object Defers extends IOApp.Simple {
  import courses.rockthejvm.utils.*

  // Deferred is a primitive for waiting for an effect,
  // while some other effect completes with a value.

  val firstDeferred: IO[Deferred[IO, Int]]  = Deferred[IO, Int]
  val secondDeferred: IO[Deferred[IO, Int]] = IO.deferred[Int]

  // Get blocks the calling fiber (semantically) until
  // some other fiber completes the Deferred with a value.
  val reader: IO[Int] = firstDeferred.flatMap { signal =>
    signal.get // Blocks the fiber
  }

  val writer = firstDeferred.flatMap { signal =>
    signal.complete(13)
  }

  def demoDeferred: IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]): IO[Unit] =
      for {
        _      <- IO("[consumer]: waiting for result...").trace
        number <- signal.get // Blocker
        _      <- IO(s"[consumer]: got the result: $number").trace
      } yield ()

    def producer(signal: Deferred[IO, Int]): IO[Unit] =
      for {
        _      <- IO("[producer]: evaluating number...").trace
        _      <- IO.sleep(1.second)
        _      <- IO("[producer]: complete: 13").trace
        number <- IO(13)
        _      <- signal.complete(number)
      } yield ()

    for {
      signal        <- Deferred[IO, Int]
      fiberConsumer <- consumer(signal).start
      fiberProducer <- producer(signal).start
      _             <- fiberConsumer.join
      _             <- fiberProducer.join
    } yield ()
  }

  // Simulate to download some content
  val fileParts = List("I", " love S", "cala", " with Cat", "s Effe", "ct!<EOF>")

  def fileNotifierWithRef: IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts
        .map { part =>
          IO(s"[Downloader]: Got: '$part'").trace >> IO.sleep(1.second) >> contentRef.update(content => content + part)
        }
        .sequence
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] =
      for {
        file <- contentRef.get
        _ <-
          if (file.endsWith("<EOF>")) IO("[Notifier]: File download complete.").trace
          else
            IO("[Notifier]: Downloading...").trace >> IO.sleep(500.milli) >> notifyFileComplete(
              contentRef
            ) // Busy waiting!
      } yield ()

    for {
      contentRef      <- Ref[IO].of("")
      fiberDownloader <- downloadFile(contentRef).start
      fiberNotifier   <- notifyFileComplete(contentRef).start
      _               <- fiberDownloader.join
      _               <- fiberNotifier.join
    } yield ()
  }

  // Deferred works miracles for waiting
  def fileNotifierWithDeferred: IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] =
      for {
        _ <- IO("[Notifier]: Downloading...").trace
        _ <- signal.get // Blocks until the signal is completed
        _ <- IO("[Notifier]: File download complete.").trace
      } yield ()

    def fileDownloader(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] =
      for {
        _       <- IO(s"[Downloader]: Got: '$part'").trace
        _       <- IO.sleep(1.second)
        content <- contentRef.updateAndGet(content => content + part)
        _       <- if (content.contains("<EOF>")) signal.complete(content) else IO.unit
      } yield ()

    for {
      contentRef    <- Ref[IO].of("")
      deferred      <- Deferred[IO, String]
      fiberNotifier <- notifyFileComplete(deferred).start
      tasksFiber    <- fileParts.map(part => fileDownloader(part, contentRef, deferred)).sequence.start
      _             <- tasksFiber.join
      _             <- fiberNotifier.join
    } yield ()
  }

  override def run: IO[Unit] = fileNotifierWithDeferred
}
