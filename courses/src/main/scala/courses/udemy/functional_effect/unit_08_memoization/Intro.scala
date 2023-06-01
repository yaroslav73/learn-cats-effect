package courses.udemy.functional_effect.unit_08_memoization

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO

import cats.syntax.traverse.toTraverseOps
import cats.syntax.foldable.toFoldableOps
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.parallel.catsSyntaxParallelTraverse1

import concurrent.duration.DurationInt
import cats.effect.Clock

object Intro extends IOApp:
  final case class Client(name: String, emailAddress: String)
  final case class Email(body: String, recipients: List[Client])

  trait EmailTemplates:
    def buildEmailForClient(temaplateId: String, client: Client): Email

  trait Error extends Throwable
  object Error:
    case object NegativeBalance extends Error
    case object AccountExpired  extends Error

  def loadEmailTemplates(): IO[EmailTemplates] =
    IO.sleep(5.seconds) *>
      IO.println("Loading email templates...") *>
      new EmailTemplates {
        def buildEmailForClient(temaplateId: String, client: Client): Email =
          if temaplateId.equalsIgnoreCase("negative") then
            Email(s"Dear, ${client.name}! Your account has a negative balance", List(client))
          else Email(s"Dear, ${client.name}! There is problem with your account", List(client))
      }.pure[IO]

  def proccessClient(client: Client): IO[Unit] =
    // TODO: Uncomment to check error handling path
    IO.println(s"Proccessing ${client.name}...") // *> IO.raiseError(Error.NegativeBalance)

  def sendEmail(email: Email): IO[Unit] =
    IO.println(s"Sending email to ${email.recipients.mkString(", ")}...")

  // Process each client once
  // Send email or error
  // Uses tempalate
  def proccessClients(clients: List[Client]): IO[Unit] =
    import Error._

    // loadEmailTemplates().flatMap { emailTemplates =>
    // Use memoization to cached result of loading templates
    loadEmailTemplates().memoize.flatMap { emailTemplatesIO =>
      clients.traverse { client =>
        proccessClient(client).handleErrorWith { error =>
          // Reuse cached version if possible
          emailTemplatesIO.flatMap { emailTemplates =>
            error match
              case NegativeBalance =>
                val email = emailTemplates.buildEmailForClient("negative", client)
                sendEmail(email)
              case _ =>
                val email = emailTemplates.buildEmailForClient(math.random.toString, client)
                sendEmail(email)
          }
        }
      }.void
    }

    // clients
    //   .map { client =>
    //     val template = loadEmailTemplates()
    //     proccessClient(client) *> template
    //       .flatMap(template => sendEmail(template.buildEmailForClient(math.random().toString(), client)))
    //     // TODO: Add printing time
    //   }
    //   .sequence
    //   .handleErrorWith(_ => IO.println("Error: Email does not send"))
    //   .void

  // Load templates only once
  def proccessClientsV1(clients: List[Client]): IO[Unit] =
    import Error._

    // Here we can remove .void call using .traverse_
    loadEmailTemplates().flatMap { template =>
      clients.traverse { client =>
        proccessClient(client).handleErrorWith {
          case NegativeBalance =>
            val email = template.buildEmailForClient("negative", client)
            sendEmail(email)
          case _ =>
            val email = template.buildEmailForClient(math.random.toString, client)
            sendEmail(email)
        }
      }.void
    }

  // Load templates only if errors happened
  def proccessClientsV2(clients: List[Client]): IO[Unit] =
    import Error._

    // Here we can remove .void call using .traverse_
    clients.traverse { client =>
      proccessClient(client).handleErrorWith { error =>
        loadEmailTemplates().flatMap { template => // TODO: loading will be happen each time when error occurs
          error match
            case NegativeBalance =>
              val email = template.buildEmailForClient("negative", client)
              sendEmail(email)
            case _ =>
              val email = template.buildEmailForClient(math.random.toString, client)
              sendEmail(email)
        }
      }
    }.void

  def run(args: List[String]): IO[ExitCode] =
    val clients = List(
      Client("Yaroslav", "test_1@mail.com"),
      Client("Marie", "test_2@mail.com"),
      Client("Anna", "test_3@mail.com"),
      Client("Jim", "test_4@mail.com")
    )

    proccessClients(clients).as(ExitCode.Success)
