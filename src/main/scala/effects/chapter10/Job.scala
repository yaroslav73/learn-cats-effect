package effects.chapter10

import cats.effect.concurrent.Deferred
import cats.effect.{ContextShift, ExitCase, Fiber, IO}

import java.util.UUID

sealed trait Job

object Job {
  def create(task: IO[_]): IO[Scheduled] =
    IO(Id(UUID.randomUUID())).map(Scheduled(_, task))

  final case class Id(id: UUID)

  final case class Scheduled(id: Id, task: IO[_]) extends Job {
    def start(implicit cs: ContextShift[IO]): IO[Running] =
      for {
        exitCase <- Deferred[IO, ExitCase[Throwable]]
        fiber <- task.void.guaranteeCase(exitCase.complete).start
      } yield Running(id, fiber, exitCase)
  }

  final case class Running(
    id: Id,
    fiber: Fiber[IO, Unit],
    exitCase: Deferred[IO, ExitCase[Throwable]]
  ) extends Job {
    def await: IO[Completed] = exitCase.get.map(Completed(id, _))
  }

  final case class Completed(id: Id, exitCase: ExitCase[Throwable]) extends Job
}
