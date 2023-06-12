package courses.udemy.functional_effect.unit_11_deffer

import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.foldable.toFoldableOps
import cats.syntax.parallel.catsSyntaxTuple2Parallel
import cats.syntax.parallel.catsSyntaxParallelSequence_

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

import concurrent.duration.DurationInt
import cats.effect.kernel.Deferred
import java.time.LocalTime

object Intro extends IOApp:
  extension (s: String)
    def debugInfo: String =
      val t = Thread.currentThread.getName
      val g = Console.GREEN
      val r = Console.RESET
      s"$g[${LocalTime.now}, $t]$r: $s"

  final case class Item(id: Long)

  // Long running
  def loadItems(): IO[List[Item]] =
    IO.println("Loading items...".debugInfo) *>
      // IO.raiseError(new Exception("Failed to load items.")) *>
      IO.sleep(3.seconds) *>
      IO.println("Items loaded".debugInfo) *>
      List(Item(1), Item(2), Item(3)).pure[IO]

  // Long running
  def initUi(): IO[Unit] =
    IO.println("Initializing ui...".debugInfo) *>
      IO.sleep(2.seconds) *>
      IO.println("UI initialized".debugInfo)

  def showItems(items: List[Item]): IO[Unit] =
    IO.println("Showing items...".debugInfo) *>
      items.traverse_(IO.println)

  def showError(e: Throwable): IO[Unit] =
    IO.println("Showing error...".debugInfo) *>
      IO.println(s"ERROR: ${e.getMessage}")

  // First implementation:
  // flatMap - here we have sequential computation,
  // so... items loading and ui init with a delay in few seconds
  def setupUiV1(): IO[Unit] =
    initUi() *> loadItems().flatMap(items => showItems(items)).handleErrorWith { e => showError(e) }

  // initUi and loadItems executes in parallel, but have an issue
  // Issue: showing error, before UI initialized
  def setupUiV2(): IO[Unit] =
    (initUi(), loadItems())
      .parMapN { case (_, items) =>
        showItems(items)
      }
      .flatten
      .handleErrorWith(e => showError(e))

  // Now issue fixed.
  def setupUiV3(): IO[Unit] =
    (initUi(), loadItems().attempt)
      .parFlatMapN {
        case (_, Right(items)) => showItems(items)
        case (_, Left(error))  => showError(error)
      }

  // How to using Deffered for handle UI and items?
  // Deferred - a purely functional synchronization primitive
  // which represents a single value which may not yet be available.

  // Initialize UI and consumes the items from Deffered
  def handleUI(itemsDef: Deferred[IO, Either[Throwable, List[Item]]]): IO[Unit] =
    initUi() *> itemsDef.get.flatMap {
      case Right(items) => showItems(items)
      case Left(error)  => showError(error)
    }

  // Produces the items and put it into Deffered
  def handleItems(itemsDef: Deferred[IO, Either[Throwable, List[Item]]]): IO[Unit] =
    loadItems().attempt.flatMap { itemsOrError => itemsDef.complete(itemsOrError) }.void

  def handleItemsInstructor(defItems: Deferred[IO, Either[Throwable, List[Item]]]): IO[Unit] =
    loadItems()
      .flatMap(items => defItems.complete(Right(items)))
      .handleErrorWith(error => defItems.complete(Left(error)))
      .void

  def setupUiV4(): IO[Unit] =
    Deferred[IO, Either[Throwable, List[Item]]].flatMap { deffered =>
      List(handleUI(deffered), handleItems(deffered)).parSequence_
    }

  def run(args: List[String]): IO[ExitCode] =
    setupUiV4().as(ExitCode.Success)
