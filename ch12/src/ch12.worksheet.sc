import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.{Applicative, Traverse}
import scala.concurrent.{Await, Future}
import cats.data.Validated
import cats.syntax.apply.* // mapN
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import cats.syntax.traverse.toTraverseOps // traverse, sequence

/*
12.1.2 Exercise: Reflecting on Folds

Try using foldLeft and foldRight with an empty list
as the accumulator and :: as the binary operator.

What results do you get in each case?
 */

// Folding from left to right reverses the list.
List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)

// Folding right to left copies the list, leaving the order intact.
List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a)

/*
12.1.3 Exercise: Scaf-fold-ing Other Methods

Implement substitutes for List's map, flatMap, filter, and sum methods in terms of foldRight.
 */
def map[A, B](list: List[A])(f: A => B): List[B] =
  list.foldRight(List.empty[B])((x, acc) => f(x) :: acc)

def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
  // ::: == ++
  list.foldRight(List.empty[B])((x, acc) => f(x) ::: acc)

def filter[A](list: List[A])(f: A => Boolean): List[A] =
  list.foldRight(List.empty[A])((x, acc) => if f(x) then x :: acc else acc)

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)
/*
12.2.2.1 Exercise: Traversing with Vectors

What is the result of the following?

The argument is of type List[Vector[Int]], so we're using the Applicative
for Vector and the return type is going to be Vector[List[Int]].
Vector is a monad, so its semigroupal combine function is based on flatMap.
We end up with a cross-product.
 */
listSequence(List(Vector(1, 2), Vector(3, 4)))

/*
12.2.2.2 Exercise: Traversing with Options

What is the return type of this method? What does it produce for the following inputs?
 */
def process(inputs: List[Int]) =
  listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

/*
The arguments to listTraverse are of types List[Int] and Int => Option[Int],
so, the return type is Option[List[Int]]. Again, Option is a monad, so, the
semigroupal combine function follows from flatMap. The semantics are, therefore,
fail-fast error handling: if all inputs are even, we get a list of outputs.
Otherwise we get None.
 */
process(List(2, 4, 6))
process(List(1, 2, 3))

/*
12.2.2.3 Exercise: Traversing with Validated

What does this method produce for the following inputs?
 */
type ErrorsOr[A] = Validated[List[String], A]

def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if (n % 2 == 0)
    then Validated.valid(n)
    else Validated.invalid(List(s"$n is not even"))
  }

/*
The return type here is ErrorsOr[List[Int]], which expands to Validated[List[String], List[Int]].
The semantics for semigroupal combine on validated are accumulating error handling, so, the
result is either a list of even Ints, or a list of errors detailing which Ints failed the test.
 */
process2(List(2, 4, 6))
process2(List(1, 2, 3))

// 12.2.3 Traverse in Cats
val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60)

val totalUptime: Future[List[Int]] =
  Traverse[List].traverse(hostnames)(getUptime)

Await.result(totalUptime, 1.second)

val numbers = List(Future(1), Future(2), Future(3))

val numbers2: Future[List[Int]] =
  Traverse[List].sequence(numbers)

Await.result(numbers2, 1.second)

Await.result(hostnames.traverse(getUptime), 1.second)

Await.result(numbers.sequence, 1.second)

/*
Foldable abstracts the foldLeft and foldRight methods we know from collections in the standard library.
It adds stack-safe implementations of these methods to a handful of extra data types, and defines a
host of situationally useful additions. That said, Foldable doesn't introduce much that we didn't already know.

The real power comes from Traverse, which abstracts and generalises the traverse and sequence methods
we know from Future. Using these methods we can turn an F[G[A]] into a G[F[A]] for any F with an
instance of Traverse and any G with an instance of Applicative.
 */
