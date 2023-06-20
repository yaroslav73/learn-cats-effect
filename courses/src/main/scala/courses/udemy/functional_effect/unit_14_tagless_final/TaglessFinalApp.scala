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
import cats.effect.Sync
import cats.effect.std.Console
import cats.Parallel

// WithIOApp.scala contains the same example but using IO instead of F.
object TaglessFinalApp extends IOApp:
  final case class User(name: String, age: Int)
  object User:
    given Show[User] = (user: User) => s"${user.name} is ${user.age} years old."

  trait UserRepository[F[_]]:
    def addUser(user: User): F[Unit]
    def allUsers(): F[List[User]]

  object UserRepository:
    def make[F[_]: Sync]: F[UserRepository[F]] =
      Ref.of[F, Map[String, User]](Map.empty).map { ref =>
        new UserRepository[F]:
          def addUser(user: User): F[Unit] =
            ref.update { users => users + (user.name -> user) }

          def allUsers(): F[List[User]] =
            ref.get.map(_.values.toList)
      }

  trait UserService[F[_]]:
    def printUsers(users: List[User]): F[Unit]

  object UserService:
    def make[F[_]: Console: Parallel]: UserService[F] =
      new UserService[F]:
        def printUsers(users: List[User]): F[Unit] =
          users.parTraverse_(user => Console[F].println(user))

  override def run(args: List[String]): IO[ExitCode] =
    val users = List(User("Yaroslav", 30), User("Marie", 31))

    for {
      userRepository <- UserRepository.make[IO]
      userService = UserService.make[IO]
      _     <- users.traverse_(user => userRepository.addUser(user))
      users <- userRepository.allUsers()
      _     <- userService.printUsers(users)
    } yield ExitCode.Success
