package courses.udemy.functional_effect.unit_13_thread_pools

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext

object ExecutionContextExample:
  final case class Task(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running task $id on the thread: ${Thread.currentThread.getName}")

  final case class BlockingTask(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running blocking task $id on the thread: ${Thread.currentThread.getName}")
      Thread.sleep(2000)
      println(s"Waking up blocking task $id on the thread: ${Thread.currentThread.getName}")

  @main def runExecutionContext(): Unit =
    val cachedUnboundedThreadPool = Executors.newCachedThreadPool()
    val executionContext          = ExecutionContext.fromExecutorService(cachedUnboundedThreadPool)

    (1 to 1000).foreach { id =>
      executionContext.submit(BlockingTask(id))
    }

    executionContext.shutdown()
    executionContext.awaitTermination(5, TimeUnit.SECONDS)
