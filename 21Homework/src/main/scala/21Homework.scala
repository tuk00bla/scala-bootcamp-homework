import cats.effect._
import cats.syntax.all._
import cats.{Applicative, Monoid}
import BookService.Error
import doobie.hikari.HikariTransactor
import doobie.implicits._
import doobie._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, _}
import org.slf4j.LoggerFactory

import java.time.{LocalDate, Year}
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.util.Try

final case class Author(id: UUID, name: String, birthday: LocalDate)
final case class EditAuthor(name: String, birthday: LocalDate)
final case class Book(id: UUID, authorId: UUID, title: String, year: Year, genre: String)
final case class EditBook(authorId: UUID, title: String, year: Year, genre: String)
final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year)
final case class BookFilter(authorId: Option[UUID])


trait BookService[F[_]] {
  def createAuthor(author: EditAuthor): F[Author]
  def getAuthor(id: UUID): F[Option[Author]]
  def getAuthors(): F[List[Author]]

  def createBook(book: EditBook): F[Either[Error, Book]]
  def getBook(id: UUID): F[Option[Book]]
  def getBooks(filter: BookFilter): F[List[Book]]
  def getBooksWithAuthor(): F[List[BookWithAuthor]]
  def updateBook(id: UUID, book: EditBook): F[Either[Error, Book]]
  def deleteBook(id: UUID): F[Option[Error]]
}

object BookServer extends IOApp {
  private val logger = LoggerFactory.getLogger("Server")

  object DbConfig {
    val dbDriverName = "org.h2.Driver"
    val dbUrl        = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1"
    val dbUser       = ""
    val dbPwd        = ""
  }

  override def run(args: List[String]): IO[ExitCode] = {
    pooled[IO]
      .use { ht =>
        for {
          repo <- BookService.of[IO](ht)
          http = new HttpBookStore(repo)
          _ <- httpServer(http.routes.orNotFound)
        } yield ()
      }
      .as(ExitCode.Success)
  }

  private def pooled[F[_]: ContextShift: Async]: Resource[F, Transactor[F]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[F](10)
      be <- Blocker[F]
      ht <- HikariTransactor.newHikariTransactor[F](
        driverClassName = DbConfig.dbDriverName,
        url = DbConfig.dbUrl,
        user = DbConfig.dbUser,
        pass = DbConfig.dbPwd,
        connectEC = ce, // await connection on this EC
        blocker = be    // execute JDBC operations on this EC
      )
    } yield ht

  private def httpServer(httpApp: HttpApp[IO]): IO[Unit] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
}

class HttpBookStore(service: BookService[IO]) {
  import io.circe.generic.auto._

  def routes: HttpRoutes[IO] = {
    books <+> addBook <+> updateBook <+> deleteBook <+> authors <+> addAuthor
  }

  implicit def uuidQPD: QueryParamDecoder[UUID] = QueryParamDecoder[String].emap(toUUID)
  object AuthorFilterParamDecoder extends OptionalQueryParamDecoderMatcher[UUID]("author")

  private val books = HttpRoutes.of[IO] {
    // curl "localhost:8080/v1/books/8f4d8b57-37ad-462a-82e0-a4f38d8e24e1" -i
    case GET -> Root / "v1" / "books" / UUIDVar(id) =>
      service.getBook(id).flatMap(fromOption(_))
    // curl "localhost:8080/v1/books/details" -i
    case GET -> Root / "v1" / "books" / "details" => service.getBooksWithAuthor().flatMap(Ok(_))
    // curl "localhost:8080/v1/books" -i
    case GET -> Root / "v1" / "books" :? AuthorFilterParamDecoder(authorId) =>
      service.getBooks(BookFilter(authorId)).flatMap(Ok(_))
  }

  private val addBook = HttpRoutes.of[IO] {
    // curl -XPOST "localhost:8080/v1/books" -i -d '{"authorId":"8e3e373e-2906-4b0f-98f5-b0dc3bd341f6","title":"Programming in Scala, 2nd Edition","year":"2011","genre":"Programming"}' -H "Content-Type: application/json"
    case req @ POST -> Root / "v1" / "books" =>
      val response =
        for {
          book    <- req.as[EditBook]
          created <- service.createBook(book)
          res = created match {
            case Right(b)  => Ok(b)
            case Left(err) => BadRequest(err)
          }
        } yield res

      response.flatten
  }

  private val updateBook = HttpRoutes.of[IO] {
    // curl -XPUT "localhost:8080/v1/books/8f4d8b57-37ad-462a-82e0-a4f38d8e24e1" -i -d '{"authorId": "8e3e373e-2906-4b0f-98f5-b0dc3bd341f6", "title": "Programming in Scala, 3rd Edition", "year": "2016", "genre": "Programming"}' -H "Content-Type: application/json"
    case req @ PUT -> Root / "v1" / "books" / UUIDVar(id) =>
      val response =
        for {
          book    <- req.as[EditBook]
          updated <- service.updateBook(id, book)
          res = updated match {
            case Right(_) => NoContent()
            case Left(err) =>
              err match {
                case Error.BookNotFound => NotFound()
                case _                            => BadRequest(err)
              }
          }
        } yield res

      response.flatten
  }

  private val deleteBook = HttpRoutes.of[IO] {
    // curl -XDELETE "localhost:8080/v1/books/8f4d8b57-37ad-462a-82e0-a4f38d8e24e1" -i
    case DELETE -> Root / "v1" / "books" / UUIDVar(id) =>
      service
        .deleteBook(id)
        .flatMap {
          case Some(Error.BookNotFound) => NotFound()
          case _                                  => NoContent()
        }

  }

  private val authors = HttpRoutes.of[IO] {
    // curl "localhost:8080/v1/authors/8e3e373e-2906-4b0f-98f5-b0dc3bd341f6" -i
    case GET -> Root / "v1" / "authors" / UUIDVar(id) =>
      service.getAuthor(id).flatMap(fromOption(_))
    // curl "localhost:8080/v1/authors" -i
    case GET -> Root / "v1" / "authors" => service.getAuthors().flatMap(Ok(_))
  }

  private val addAuthor = HttpRoutes.of[IO] {
    // curl -XPOST "localhost:8080/v1/authors" -i -d '{"name":"Martin Odersky","birthday":"1958-09-05"}' -H "Content-Type: application/json"
    case req @ POST -> Root / "v1" / "authors" =>
      val response =
        for {
          author  <- req.as[EditAuthor]
          created <- service.createAuthor(author)
        } yield Ok(created)

      response.flatten
  }

  private def fromOption[T](value: Option[T])(implicit encoder: EntityEncoder[IO, T]) =
    value match {
      case Some(v) => Ok(v)(IO.ioEffect, encoder)
      case None    => NotFound()(Applicative[IO])
    }

  private def toUUID(s: String): Either[ParseFailure, UUID] =
    Try(UUID.fromString(s)).toEither.leftMap(_ => ParseFailure(s"param must be a valid uuid", Monoid[String].empty))
}

private final class BookStoreImpl[F[_]: Sync](ht: Transactor[F])(implicit
                                                                 ev: Bracket[F, Throwable]
) extends BookService[F] {
  private implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  private implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
  private implicit val localDateMeta: Meta[LocalDate] =
    Meta[String].timap(LocalDate.parse)(_.toString)

  private val authors: Fragment = fr"SELECT id, name, birthday FROM authors"
  private val books: Fragment = fr"SELECT id, author_id, title, year, genre  FROM books"

  override def createBook(book: EditBook): F[Either[Error, Book]] = {
    for {
      author <- getAuthor(book.authorId)
      id <- Sync[F].delay(UUID.randomUUID())
      res <- author match {
        case Some(_) =>
          sql"INSERT INTO books (id, title, author_id, year, genre) VALUES ($id, ${book.title}, ${book.authorId}, ${book.year}, ${book.genre})".update.run
            .transact(ht) *>
            Sync[F].pure(
              Right[Error, Book](
                Book(id, book.authorId, book.title, book.year, book.genre)
              )
            )
        case None => Sync[F].pure(Left(Error.UnknownAuthor))
      }
    } yield res
  }

  override def updateBook(id: UUID, book: EditBook): F[Either[Error, Book]] = {
    for {
      existing <- getBook(id)
      author <- getAuthor(book.authorId)
      res <- existing match {
        case Some(_) =>
          author match {
            case Some(_) =>
              sql"UPDATE books SET title = ${book.title}, author_id = ${book.authorId}, year = ${book.year}, genre = ${book.genre} WHERE id = $id".update.run
                .transact(ht) *>
                Sync[F].pure(
                  Right[Error, Book](
                    Book(id, book.authorId, book.title, book.year, book.genre)
                  )
                )
            case None => Sync[F].pure(Left(Error.UnknownAuthor))
          }
        case None => Sync[F].pure(Left(Error.BookNotFound))
      }
    } yield res
  }

  override def deleteBook(id: UUID): F[Option[Error]] = {
    for {
      existing <- getBook(id)
      res <- existing match {
        case Some(_) =>
          sql"DELETE books WHERE id = $id".update.run.transact(ht) *> Sync[F].pure(none)
        case None => Sync[F].pure(Some(Error.BookNotFound))
      }
    } yield res
  }

  override def getBook(id: UUID): F[Option[Book]] =
    (books ++ fr"WHERE id = $id").query[Book].option.transact(ht)

  override def getBooks(filter: BookFilter): F[List[Book]] = {
    val sql = filter.authorId match {
      case Some(authorId) => books ++ fr"WHERE author_id = $authorId"
      case None => books
    }
    sql.query[Book].to[List].transact(ht)
  }

  override def getBooksWithAuthor(): F[List[BookWithAuthor]] = {
    sql"SELECT b.id, a.id, a.name, a.birthday, b.title, b.year FROM books b INNER JOIN authors a ON b.author_id = a.id".query[BookWithAuthor].to[List].transact(ht)
  }

  def createAuthor(author: EditAuthor): F[Author] = {
    for {
      id <- Sync[F].delay(UUID.randomUUID())
      res <-
        sql"INSERT INTO authors (id, name, birthday) VALUES ($id, ${author.name}, ${author.birthday})".update.run
          .transact(ht) *>
          Sync[F].pure(Author(id, author.name, author.birthday))

    } yield res
  }

  override def getAuthor(id: UUID): F[Option[Author]] =
    (authors ++ fr"WHERE id = $id").query[Author].option.transact(ht)

  override def getAuthors(): F[List[Author]] =
    authors.query[Author].to[List].transact(ht)

}

object BookService {
  abstract sealed class Error(val message: String) extends Throwable
  object Error {
    final case object UnknownAuthor extends Error("Unknown author")
    final case object BookNotFound  extends Error("Book not found")
  }

  def of[F[_]: Sync](ht: Transactor[F]): F[BookService[F]] =
    for {
      _ <- init.transact(ht)
    } yield new BookStoreImpl(ht)

  // setup
  private val ddl1 = Fragment.const("""CREATE TABLE authors (
                                      |  id UUID PRIMARY KEY,
                                      |  name VARCHAR(100) NOT NULL,
                                      |  birthday DATE);""".stripMargin)

  private val ddl2 = Fragment.const(
    """CREATE TABLE books (
      |  id UUID PRIMARY KEY,
      |  author_id UUID NOT NULL,
      |  title VARCHAR(100) NOT NULL,
      |  year INT,
      |  genre VARCHAR(100) NOT NULL,
      |  FOREIGN KEY (author_id) REFERENCES authors(id));""".stripMargin
  )

  val dml: Fragment = Fragment.const(s"""
                              |INSERT INTO authors (id, name, birthday) VALUES
                              |  ('8e3e373e-2906-4b0f-98f5-b0dc3bd341f6', 'Martin Odersky', '1958-09-05'),
                              |  ('385ecf4c-462c-4c42-a31a-55ed7ff2bf6a', 'J.K. Rowling', '1965-07-31');
                              |
                              |INSERT INTO books (id, author_id, title, year, genre) VALUES
                              |  ('8f4d8b57-37ad-462a-82e0-a4f38d8e24e1', '8e3e373e-2906-4b0f-98f5-b0dc3bd341f6', 'Programming in Scala', 2016, 'Programming'),
                              |  ('cf11907d-1370-47ea-912a-5a3ad40d041c', '8e3e373e-2906-4b0f-98f5-b0dc3bd341f6', 'Harry Potter and Philosopher''s Stone', 1997, 'Fantasy'),
                              |  ('1b42c69b-21cd-4296-bd80-5709083e8ab8', '385ecf4c-462c-4c42-a31a-55ed7ff2bf6a', 'Harry Potter and the Chamber of Secrets', 1998, 'Fantasy');
                              |""".stripMargin)

  private def init: ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()
}