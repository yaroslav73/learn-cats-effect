package courses.udemy.functional_effect.unit_13_thread_pools

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

object ThreadPoolExercise:
  // Hints:
  // - LinkedBlockingQueue for tasks
  // - Workers run forever

  final case class FixedThreadPool(noThreads: Int):
    import FixedThreadPool._
    private var workers = List.range(0, noThreads).map(id => Worker(s"worker-$id"))

    workers.foreach(_.start)

    def execute(task: Runnable): Unit =
      queue.put(task)

  object FixedThreadPool:
    private val queue = new LinkedBlockingQueue[Runnable]()

    final case class Worker(name: String) extends Thread(name):
      override def run(): Unit =
        while true
        do
          val task = queue.take
          task.run()

  final case class Task(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running task $id on the thread: ${Thread.currentThread.getName}")

  final case class BlockingTask(id: Long) extends Runnable:
    override def run(): Unit =
      println(s"Running blocking task $id on the thread: ${Thread.currentThread.getName}")
      Thread.sleep(2000)
      println(s"Waking up blocking task $id on the thread: ${Thread.currentThread.getName}")

  @main def runThreadPoolExercise(): Unit =
    val pool = FixedThreadPool(2)
    (1 to 10).foreach { id =>
      pool.execute(Task(id))
    }
