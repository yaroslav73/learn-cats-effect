package courses.udemy.functional_effect.unit_13_thread_pools

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

object CachedUnboundedThreadPoolExample:
  final case class Task(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running task $id on the thread: ${Thread.currentThread.getName}")

  final case class BlockingTask(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running blocking task $id on the thread: ${Thread.currentThread.getName}")
      Thread.sleep(2000)
      println(s"Waking up blocking task $id on the thread: ${Thread.currentThread.getName}")

  @main def runCachedUnbounded(): Unit =
    val cachedUnboundedThreadPool = Executors.newCachedThreadPool()

    // TODO: What is different between .submit and .execute?
    (1 to 1000).foreach { id =>
      cachedUnboundedThreadPool.submit(BlockingTask(id))
    }

    cachedUnboundedThreadPool.shutdown()
    cachedUnboundedThreadPool.awaitTermination(5, TimeUnit.SECONDS)
