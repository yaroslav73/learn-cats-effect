package courses.rockthejvm.part_01_intro

import scala.annotation.tailrec

object ContextualAbstractions extends App {
  // given and using
  def increment(n: Int)(using amount: Int): Int = n + amount
  given defaultAmount: Int = 1

  println(s"Increment with given/using: ${increment(10)}")

  trait Combiner[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  def combineAll[A](values: List[A])(using combiner: Combiner[A]) =
    values.foldLeft(combiner.empty)(combiner.combine)

  given combinerInt: Combiner[Int] with {
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int = 0
  }

  val numbers = (1 to 10).toList
  val sum10 = combineAll(numbers)
  println(s"Sum 1 to 10: $sum10")

  given combinerOptions[T](using combiner: Combiner[T]): Combiner[Option[T]] with {
    def combine(x: Option[T], y: Option[T]): Option[T] =
      for {
        a <- x
        b <- y
      } yield combiner.combine(a, b)
    def empty: Option[T] = Some(combiner.empty)
  }

  val optNumbers = (1 to 10).toList.map(Option(_))
  println(s"Sum option: ${combineAll(optNumbers)}")

  // extension methods
  final case class Person(name: String) {
    def greeting: String = s"Hello, my name is $name!"
  }

  extension (name: String) {
    def greeting: String = Person(name).greeting
  }

  println("Yaroslav".greeting)

  extension [T](xs: List[T])(using combiner: Combiner[T]) {
    def reduceAll: T = xs.foldLeft(combiner.empty)(combiner.combine)
  }

  println(s"Sum 1 to 10 again: ${(1 to 10).toList.reduceAll}")

  // type classes
  final case class Payment(name: String, code: Int)

  // 1. Type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // 2. Type class instances
  given stringSerializer: JSONSerializer[String] with
    def toJson(value: String): String = s"\"$value\""

  given intSerializer: JSONSerializer[Int] with
    def toJson(value: Int): String = value.toString

  given paymentSerializer: JSONSerializer[Payment] with
    def toJson(payment: Payment): String =
      s"""
         |{\n\t"name": ${payment.name},
         |\t"code": ${payment.code}
         |}
         |""".stripMargin.trim

  // 3. User-facing API
  def convertToJson[T](value: T)(using serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  // 4. Extension methods
  extension [T](value: T)(using serializer: JSONSerializer[T])
    def toJson: String = serializer.toJson(value)

  val visa = Payment("VISA", 12)
  println(visa.toJson)
}
