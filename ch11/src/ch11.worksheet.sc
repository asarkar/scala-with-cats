import cats.Semigroupal
import cats.syntax.apply.*    // tupled and mapN
import cats.syntax.parallel.* // parTupled

// 11.1.1 Joining Two Contexts
/*
While Semigroup allows us to join values, Semigroupal allows us to join contexts.
If either parameter evaluates to None, the entire result is None.
 */
Semigroupal[Option].product(Some(123), Some("abc"))

// 11.1.2 Joining Three or More Contexts
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

// 11.2 Apply Syntax
(Option(123), Option("abc")).tupled

final case class Cat(name: String, born: Int, color: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & black")
).mapN(Cat.apply)

/*
11.3.1.1 Exercise: The Product of Lists
Why does product for List produce the Cartesian product?
 */
(List(1, 2), List(3, 4)).tupled

// def product[F[_]: Monad, A, B](x: F[A], y: F[B]): F[(A, B)] =
//   x.flatMap(a => y.map(b => (a, b)))

/*
^^^This code is equivalent to a for comprehension.
The semantics of flatMap are what give rise to the behaviour for List and Either.
 */

// 11.4 Parallel
type ErrorOr[A] = Either[Vector[String], A]
val error1: ErrorOr[Int] = Left(Vector("Error 1"))
val error2: ErrorOr[Int] = Left(Vector("Error 2"))
Semigroupal[ErrorOr].product(error1, error2)

(error1, error2).tupled

// To collect all the errors we simply replace tupled with its "parallel" version called parTupled.
(error1, error2).parTupled

/*
11.4.0.1 Exercise: Parallel List
Does List have a Parallel instance? If so, what does the Parallel instance do?
 */

// List does have a Parallel instance, and it zips the List insted of creating the cartesian product.
(List(1, 2), List(3, 4)).parTupled

// Semigroupal and Applicative effectively provide alternative encodings of the same notion of joining contexts.
