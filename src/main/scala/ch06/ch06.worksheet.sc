import cats.Monad
import cats.Semigroupal
import cats.syntax.apply.catsSyntaxTuple3Semigroupal
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import cats.syntax.parallel.catsSyntaxTuple2Parallel
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps

Semigroupal[Option].product(Some(123), Some("abc"))

Semigroupal[Option].product(None, Some("abc"))

Semigroupal.tuple3(Option(1), Option(2), Option(3))

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)

final case class Cat(name: String, born: Int, color: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & black")
).mapN(Cat.apply)

/*
6.3.1.1 Exercise: The Product of Lists

Why does product for List produce the Cartesian product?
*/
def product[F[_]: Monad, A, B](x: F[A], y: F[B]): F[(A, B)] =
  for {
    a <- x // flatMap
    b <- y // map
  } yield (a, b)

// The semantics of flatMap are what give rise to the cross product for lists.

type ErrorOr[A] = Either[Vector[String], A]
val error1: ErrorOr[Int] = Left(Vector("Error 1"))
val error2: ErrorOr[Int] = Left(Vector("Error 2"))

Semigroupal[ErrorOr].product(error1, error2)

(error1, error2).parTupled

/*
6.4.0.1 Exercise: Parallel List

Does List have a Parallel instance? If so, what does the Parallel instance do?
*/
(List(1, 2), List(3, 4)).tupled

// List does have a Parallel instance, and it zips the List insted of creating the cartesian product.
(List(1, 2), List(3, 4)).parTupled
