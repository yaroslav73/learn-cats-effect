package courses.udemy.functional_effect.unit_13_thread_pools

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object WorkStealingThreadPoolExample:
  final case class Task(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running task $id on the thread: ${Thread.currentThread.getName}")

  final case class ResultTask[A](id: Long, f: () => A) extends Callable[A]:
    override def call(): A =
      println(s"Running result task $id on the thread: ${Thread.currentThread.getName}")
      f()

  final case class BlockingTask(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running blocking task $id on the thread: ${Thread.currentThread.getName}")
      Thread.sleep(2000)
      println(s"Waking up blocking task $id on the thread: ${Thread.currentThread.getName}")

  @main def runWorkStealing(): Unit =
    println(s"Avaliable processors: ${Runtime.getRuntime().availableProcessors()}")

    val workStealingThreadPool = Executors.newWorkStealingPool()

    // With Runnable
    workStealingThreadPool.submit(Task(13))

    // With Callable
    val futureResult = workStealingThreadPool.submit(ResultTask(73, () => "Hello, Future!"))
    println(futureResult.get())

    // A lot of Tasks
    (1 to 1000).foreach { id =>
      workStealingThreadPool.submit(BlockingTask(id))
    }

    workStealingThreadPool.shutdown()
    workStealingThreadPool.awaitTermination(2, TimeUnit.SECONDS)
