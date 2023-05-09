package courses.rockthejvm.part_02_effects_and_io

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {
  import scala.concurrent.ExecutionContext.Implicits.global

  // Traverse
  import cats.Traverse
  val traverseList: Traverse[List] = Traverse[List]

  val workLoad: List[String] = List("I quite like Cats Effect", "Scala is great", "I love learn new things")

  // Traverse Future
  def heavyComputation(s: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    s.split(" ").length
  }

  def clunkyFuture(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // List[Future[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  def traverseFuture(): Unit = {
    val singleFuture: Future[List[Int]] = traverseList.traverse(workLoad)(heavyComputation)
    // singleFuture stores one Future with ALL results inside as List
    singleFuture.foreach(println)
  }

  // Traverse IO
  import courses.rockthejvm.utils._

  def heavyComputationIO(s: String): IO[Int] = IO {
    Thread.sleep(1000)
    s.split(" ").length
  }.trace

  val ios: List[IO[Int]]      = workLoad.map(heavyComputationIO)
  val singleIO: IO[List[Int]] = traverseList.traverse(workLoad)(heavyComputationIO)

  // Parallel traverse IO
  // Add parTraverse extension method with next import
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(heavyComputationIO)

  override def run: IO[Unit] = {
//    clunkyFuture()
//    traverseFuture()
//    IO(Thread.sleep(10000))

    singleIO.map(_.sum).trace.void

    parallelSingleIO.map(_.sum).trace.void
  }
}
