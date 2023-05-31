package courses.udemy.functional_effect.unit_07_async

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Success
import java.util.concurrent.Executors

object AsyncApp extends IOApp:
  final case class Person(id: String, name: String)

  def findPersonByIdFuture(id: String)(using ec: ExecutionContext): Future[Person] =
    Future {
      println(s"[Thread: ${Thread.currentThread.getName}]: Finding person by id $id ...")
      Person(id, "John")
    }

  // Ugly and blocking implementation. Don't do that!
  def findPersonByIdIOUgly(id: String): IO[Person] =
    given ExecutionContext = ExecutionContext.global
    IO(Await.result(findPersonByIdFuture(id), Duration.Inf))

  def findPersonByIdIO(id: String): IO[Person] =
    IO.executionContext.flatMap { ec =>
      given ExecutionContext = ec
      IO.async_ { callback =>
        findPersonByIdFuture(id).onComplete {
          case Success(person) => callback(Right(person))
          case Failure(error)  => callback(Left(error))
        }
      }
    }

  def findPersonByIdIOFromFuture(id: String): IO[Person] =
    IO.executionContext.flatMap { ec =>
      given ExecutionContext = ec
      IO.fromFuture(IO(findPersonByIdFuture(id)))
    }

  def run(args: List[String]): IO[ExitCode] =
    findPersonByIdIO("73")
      .evalOn(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))
      .flatTap(IO.println)
      .as(ExitCode.Success)
      .as(ExitCode.Success)
