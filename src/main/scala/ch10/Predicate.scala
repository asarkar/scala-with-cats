package ch10

import cats.kernel.Semigroup
import cats.data.Validated
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import cats.syntax.validated.catsSyntaxValidatedId
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.data.Validated.{Invalid, Valid}

// 10 Case Study: Data Validation

// Preciate is basically a wrapper around a function:
//   A => Validated[E, A]
sealed trait Predicate[E, A]:
  import Predicate.*

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def run(using s: Semigroup[E]): A => Either[E, A] =
    (a: A) => this(a).toEither

  private def apply(a: A)(using s: Semigroup[E]): Validated[E, A] =
    this match
      case Pure(func) =>
        func(a)

      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)

      case Or(left, right) =>
        left(a) match
          case Valid(_) => Valid(a)
          case Invalid(e1) =>
            right(a) match
              case Valid(_)    => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)

object Predicate:
  private final case class And[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]
  ) extends Predicate[E, A]

  private final case class Or[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]
  ) extends Predicate[E, A]

  private final case class Pure[E, A](
      func: A => Validated[E, A]
  ) extends Predicate[E, A]

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if (fn(a)) a.valid else err.invalid)
