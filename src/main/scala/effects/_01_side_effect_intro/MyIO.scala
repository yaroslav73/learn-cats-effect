package effects._01_side_effect_intro

case class MyIO[A](unsafeRun: () => A) {
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(unsafeRun()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafeRun()).unsafeRun())
  // f(unsafeRun()) // why we can not use this?
}

object MyIO {
  def putStr(s: => String): MyIO[Unit] =
    MyIO[Unit](() => println(s))
}
