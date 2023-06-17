package courses.udemy.functional_effect.unit_13_thread_pools

object ThreadPoolExercise:
  // Hints:
  // - LinkedBlockingQueue for tasks
  // - Workers run forever

  final case class FixedThreadPool(noThreads: Int):
    def execute(task: Runnable): Unit = ???

  @main def runThreadPoolExercise(): Unit = ???
