import scala.concurrent.Await
import scala.concurrent.Future
import cats.data.{EitherT, OptionT}
import cats.syntax.applicative.catsSyntaxApplicativeId
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

type ListOption[A] = OptionT[List, A]

val result1: ListOption[Int] = OptionT(List(Option(10)))

val result2: ListOption[Int] = 32.pure[ListOption]

result1.flatMap { (x: Int) =>
  result2.map { (y: Int) =>
    x + y
  }
}

type FutureEither[A] = EitherT[Future, String, A]

type FutureEitherOption[A] = OptionT[FutureEither, A]

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

val intermediate = futureEitherOr.value
// intermediate: FutureEither[Option[Int]] = EitherT(
//   Future(Success(Right(Some(42))))
// )

val stack = intermediate.value
// stack: Future[Either[String, Option[Int]]] = Future(Success(Right(Some(42))))

Await.result(stack, 1.second)

import cats.data.Writer

type Logged[A] = Writer[List[String], A]

// Methods generally return untransformed stacks:
def parseNumber(str: String): Logged[Option[Int]] =
  util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None      => Writer(List(s"Failed on $str"), None)
  }

// Consumers use monad transformers locally to simplify composition:
def addAll(a: String, b: String, c: String): Logged[Option[Int]] =
  val result = 
    for
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    yield a + b + c

  result.value


// This approach doesn't force OptionT on other users' code:
val x = addAll("1", "2", "3")
// result1: Logged[Option[Int]] = WriterT(
//   (List("Read 1", "Read 2", "Read 3"), Some(6))
// )
val y = addAll("1", "a", "3")
// result2: Logged[Option[Int]] = WriterT(
//   (List("Read 1", "Failed on a"), None)
// )