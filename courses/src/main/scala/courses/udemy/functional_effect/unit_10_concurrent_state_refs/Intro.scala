package courses.udemy.functional_effect.unit_10_concurrent_state_refs

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.traverse.toTraverseOps
import cats.syntax.parallel.catsSyntaxTuple4Parallel
import cats.syntax.parallel.catsSyntaxParallelTraverse1
import cats.effect.kernel.Ref

object Intro extends IOApp:
  final case class Account()
  final case class Activity()
  final case class Purchase()
  final case class Customer(
    id: Long,
    name: String,
    accounts: List[Account],
    ativities: List[Activity],
    purchases: List[Purchase]
  )

  def loadName(id: Long)(ref: Ref[IO, List[String]]): IO[String] =
    ref.update(logs => s"[${Thread.currentThread.getName}]: Loading name for customer $id" :: logs) *>
      IO.pure(s"Customer $id")

  def loadAccounts(id: Long)(ref: Ref[IO, List[String]]): IO[List[Account]] =
    ref.update(logs => s"[${Thread.currentThread.getName}]: Loading accounts for customer $id" :: logs) *>
      IO.pure(List(Account()))

  def loadActivities(id: Long)(ref: Ref[IO, List[String]]): IO[List[Activity]] =
    ref.update(logs => s"[${Thread.currentThread.getName}]: Loading activities for customer $id" :: logs) *>
      IO.pure(List(Activity()))

  def loadPurchases(id: Long)(ref: Ref[IO, List[String]]): IO[List[Purchase]] =
    ref.update(logs => s"[${Thread.currentThread.getName}]: Loading purchases for customer $id" :: logs) *>
      IO.pure(List(Purchase()))

  def loadCustomer(id: Long)(ref: Ref[IO, List[String]]): IO[Customer] =
    (
      loadName(id)(ref),
      loadAccounts(id)(ref),
      loadActivities(id)(ref),
      loadPurchases(id)(ref)
    ).parMapN { case (name, accounts, activities, purchases) =>
      Customer(id, name, accounts, activities, purchases)
    }

  def run(args: List[String]): IO[ExitCode] =
    // val ref         = Ref.of[IO, List[String]](List.empty[String])
    val customerIds = List(73L, 87L, 113L)

    for {
      // Here we use one ref for all processed ids
      // But when we move creating ref to parTraverse method
      // We got build ref for each of processed ids.
      // ref <- Ref.of[IO, List[String]](List.empty[String])
      _ <- customerIds
        .parTraverse { id =>
          Ref.of[IO, List[String]](List.empty[String]).flatMap { ref =>
            loadCustomer(id)(ref).flatTap(_ => ref.get.flatTap(logs => IO.println(logs.mkString("\n"))))
          }
        }
        // .flatTap(_ => ref.get.flatTap(logs => IO.println(logs.mkString("\n"))))
        .flatTap(IO.println)
    } yield ExitCode.Success
