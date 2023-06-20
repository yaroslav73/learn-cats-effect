package courses.udemy.functional_effect.unit_14_tagless_final

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.kernel.Ref

import cats.syntax.functor.toFunctorOps
import cats.syntax.traverse.toTraverseOps
import cats.syntax.foldable.toFoldableOps
import cats.syntax.parallel.catsSyntaxParallelTraverse_
import cats.Show

object WithIOApp extends IOApp:
  final case class User(name: String, age: Int)
  object User:
    given Show[User] = (user: User) => s"${user.name} is ${user.age} years old."

  trait UserRepository:
    def addUser(user: User): IO[Unit]
    def allUsers(): IO[List[User]]

  object UserRepository:
    def make: IO[UserRepository] =
      Ref.of[IO, Map[String, User]](Map.empty).map { ref =>
        new UserRepository:
          def addUser(user: User): IO[Unit] =
            ref.update { users => users + (user.name -> user) }

          def allUsers(): IO[List[User]] =
            ref.get.map(_.values.toList)
      }

  trait UserService:
    def printUsers(users: List[User]): IO[Unit]

  object UserService:
    def make: UserService =
      new UserService:
        def printUsers(users: List[User]): IO[Unit] =
          // users.map(IO.println(_)).sequence.void
          // the same:
          users.parTraverse_(IO.println)

  override def run(args: List[String]): IO[ExitCode] =
    val users = List(User("Yaroslav", 30), User("Marie", 31))

    for {
      userRepository <- UserRepository.make
      userService = UserService.make
      _     <- users.traverse_(user => userRepository.addUser(user))
      users <- userRepository.allUsers()
      _     <- userService.printUsers(users)
    } yield ExitCode.Success
