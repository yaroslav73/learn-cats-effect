package books.essential.effects.chapter10

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCase, Fiber, IO}
import cats.implicits._

trait Reactor {
  def whenAwake(
    onStart: Job.Id => IO[Unit],
    onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]
  ): IO[Unit]
}

object Reactor {
  def apply(stateRef: Ref[IO, JobScheduler.State])(implicit cs: ContextShift[IO]): Reactor =
    new Reactor {
      def whenAwake(
        onStart: Job.Id => IO[Unit],
        onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]
      ): IO[Unit] = {
        def startJob(scheduled: Job.Scheduled): IO[Job.Running] =
          for {
            running <- scheduled.start
            _ <- stateRef.update(_.running(running))
            _ <- registerOnComplete(running)
            _ <- onStart(running.id).attempt
          } yield running

        def registerOnComplete(job: Job.Running): IO[Fiber[IO, Unit]] =
          job.await
            .flatMap(jobCompleted)
            .start

        def jobCompleted(job: Job.Completed): IO[Unit] =
          stateRef
            .update(_.onComplete(job))
            .flatTap(_ => onComplete(job.id, job.exitCase).attempt)

        def startNextJob: IO[Option[Job.Running]] =
          for {
            job <- stateRef.modify(state => state.dequeue)
            running <- job.traverse(startJob)
          } yield running

        startNextJob
          .iterateUntil(_.isEmpty)
          .void
      }
    }
}
