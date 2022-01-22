package effects.chapter10

import cats.data.Chain
import cats.effect.IO
import cats.effect.concurrent.Ref

trait JobScheduler {
  def schedule(task: IO[_]): IO[Job.Id]
}

object JobScheduler {
  def scheduler(stateRef: Ref[IO, State]): JobScheduler = new JobScheduler {
    def schedule(task: IO[_]): IO[Job.Id] =
      for {
        job <- Job.create(task)
        _ <- stateRef.update(_.enqueue(job))
      } yield job.id
  }

  final case class State(
    maxRunning: Int,
    scheduled: Chain[Job.Scheduled] = Chain.empty,
    running: Map[Job.Id, Job.Running] = Map.empty,
    completed: Chain[Job.Completed] = Chain.empty
  ) {
    def enqueue(job: Job.Scheduled): State =
      copy(scheduled = scheduled :+ job)

    def dequeue: (State, Option[Job.Scheduled]) =
      scheduled.deleteFirst(_ => scheduled.nonEmpty && running.sizeIs <= maxRunning) match {
        case Some((job, chain)) => copy(scheduled = chain) -> Some(job)
        case None               => this -> None
      }

    def running(job: Job.Running): State =
      copy(running = running + (job.id -> job))

    def onComplete(job: Job.Completed): State =
      copy(running = running.removed(job.id), completed = completed :+ job)
  }
}
