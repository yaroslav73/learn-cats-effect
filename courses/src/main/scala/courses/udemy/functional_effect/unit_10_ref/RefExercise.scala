package courses.udemy.functional_effect.unit_10_ref

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.parallel.catsSyntaxParallelTraverse_

object RefExercise extends IOApp:
  final case class User(name: String, age: Int, friends: List[User]):
    override def toString(): String = s"User($name, $age, ${friends.map(_.name)})"

  // Use Ref to hold the current oldest user
  def findOldest(user: User): IO[User] =
    Ref.of[IO, User](user).flatMap { ref =>
      findOldestRef(user, ref) *> ref.get
    }

  private def findOldestRef(user: User, ref: Ref[IO, User]): IO[Unit] =
    ref.update { oldest =>
      if user.age > oldest.age then user
      else oldest
    } *> user.friends.parTraverse_(friend => findOldestRef(friend, ref))

  def run(args: List[String]): IO[ExitCode] =
    val a = User("A", 69, Nil)
    val b = User("B", 73, Nil)
    val c = User("C", 87, Nil)
    val d = User("D", 113, List(a, b))
    val e = User("E", 173, List(c))
    val f = User("F", 210, List(d, e))

    findOldest(f).flatTap(IO.println).as(ExitCode.Success)
