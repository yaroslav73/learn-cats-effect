package effects._01_side_effect_intro

case class MyIO[A](unsafeRun: () => A) {
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(unsafeRun()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafeRun()).unsafeRun())
//    f(unsafeRun()) // why we can not use this?
  // Що ж, давай подивимось:
  // - сигнатура методу (f: A => MyIO[B]): MyIO[B] - тобто беремо функцію A => MyIO[B]
  // - MyIO містить у собі функцію unsafeRun: () => A, якщо ми її викличимо, вона поверна нам тип А
  // - отже, якщо викликати f(unsafeRun()), то ми отримаємо MyIO[B]
  // - але якщо на результаті f(unsafeRun()) викликати .unsafeRun(), що дасть нам B
  // - і з цього можемо побудувати новий MyIO(() => B)
}

object MyIO {
  def putStr(s: => String): MyIO[Unit] =
    MyIO[Unit](() => println(s))
}
