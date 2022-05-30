package courses.rockthejvm.part_01_intro

object CatsTypeClasses extends App {
  /*
    - applicative
    - functor
    - flatMap
    - monad
    - apply
    - traverse
    - applicativeError / monadError
   */

  // 1. Functor - mappable data structure
  trait _Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  import cats.Functor

  val listFunctor = Functor[List]

  // Generalized "mapping" API
  def incrementWithTypeBound[F[_]: Functor](container: F[Int]): F[Int] =
    Functor[F].map(container)(_ + 1)

  def incrementWithUsing[F[_]](container: F[Int])(using F: Functor[F]): F[Int] =
    F.map(container)(_ + 1)

  import cats.syntax.functor.*
  def incrementWithSyntax[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)



  // 2. Applicative - ability to "wrap" types
  trait _Applicative[F[_]] extends _Functor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative

  val listApplicative = Applicative[List]

  val oneElementList: List[Int] = listApplicative.pure(1)



  // 3. FlatMap - ability to chain multiple wrapper computations
  trait _FlatMap[F[_]] extends _Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap

  val listFlatMap = FlatMap[List]

  import cats.syntax.flatMap.*
  def product[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  println(product(List(1, 2, 3), List("a", "b", "c")))



  // 4. Monad - applicative + flatMap
  trait _Monad[F[_]] extends _Applicative[F] with _FlatMap[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  }

  import cats.Monad

  val listMonad = Monad[List]
  def productWithMonad[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  println(productWithMonad(List(1, 2, 3), List("a", "b", "c")))



  // 5. ApplicativeError
  trait _ApplicativeError[F[_], E] extends _Applicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]

  val eitherApplicativeError = ApplicativeError[ErrorOr, String]

  import cats.syntax.applicativeError
  def divide[F[_]](a: Int, b: Int)(using F: ApplicativeError[F, String]): F[Int] =
    if (b == 0) F.raiseError("division by zero") else F.pure(a / b)

  println(s"6 / 2 = ${divide(6, 2)}")
  println(s"7 / 0 = ${divide(7, 0)}")



  // 6. MonadError
  trait _MonadError[F[_], E] extends _ApplicativeError[F, E] with _Monad[F]

  // 7. Traverse
  trait _Traverse[F[_]] extends _Functor[F] {
    def traverse[G[_], A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  }

  // Turn nested wrappers inside out
  val listOfOptions: List[Option[Int]] = List(Some(1), Some(2), Some(3))

  import cats.Traverse
  val listTraverse = Traverse[List]
  val optionList: Option[List[Int]] = listTraverse.traverse(List(1, 2, 3))(n => Option(n))
}
