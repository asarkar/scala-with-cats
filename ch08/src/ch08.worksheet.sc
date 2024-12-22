import ch08.{display, Display, encode, decode, Codec}

display("hello")
display(true)
display(Display.Box("hello"))
// If we don’t have a Display for the type inside the Box,
// calls to display should fail to compile:

// display(Box(123))

val ca = encode(123.4)
val cb = decode[Double]("123.4")

val cc = encode(Codec.Box(123.4))
val cd = decode[Codec.Box[Double]]("123.4")

// 3.5.2 Functor Syntax
val func1 = (a: Int) => a + 1
val func2 = (a: Int) => a * 2
val func3 = (a: Int) => s"${a}!"
val func4 = func1.map(func2).map(func3)

func4(123)

def doMath[F[_]](start: F[Int])(using functor: Functor[F]): F[Int] =
  start.map(n => n + 1 * 2)

doMath(Option(20))
doMath(List(1, 2, 3))

// 8.7.1 Contravariant in Cats
import cats.Contravariant
import cats.Show
import cats.instances.string.catsStdShowForString

val showString = Show[String]

// trait Contravariant[F[_]]:
//   def contramap[A, B](fa: F[A])(f: B => A): F[B]

val showSymbol: Show[Symbol] = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")

showSymbol.show(Symbol("dave"))

import cats.syntax.contravariant.toContravariantOps

showString
  .contramap[Symbol](sym => s"'${sym.name}")
  .show(Symbol("dave"))

// 8.7.2 Invariant in Cats
import cats.Monoid
import cats.instances.string.catsKernelStdMonoidForString
import cats.syntax.invariant.toInvariantOps
import cats.syntax.semigroup.catsSyntaxSemigroup

given Monoid[Symbol] =
  Monoid[String].imap(Symbol.apply)(_.name)

Monoid[Symbol].empty

Symbol("a") |+| Symbol("few") |+| Symbol("words")

// 8.8 Aside: Partial Unification
val f1 = (x: Int) => x.toDouble
val f2 = (y: Double) => y * 2

val f3 = func1.map(func2)

val either: Either[String, Int] = Right(123)

either.map(_ + 1)

val f3a: Int => Double =
  a => f2(f1(a))

val f3b: Int => Double =
  f2.compose(f1)

// error: value contramap is not a member of Double => Double
// val f3c = f2.contramap(f1)

type <=[B, A] = A => B

type F[A] = Double <= A

val f2b: Double <= Double = f2

val f3c = f2b.contramap(f1)
