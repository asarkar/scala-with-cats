package ch10

import cats.data.NonEmptyList
import cats.data.Kleisli
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import Predicate.*

type Errors = NonEmptyList[String]

def error(s: String): NonEmptyList[String] =
  NonEmptyList(s, Nil)

type Result[A] = Either[Errors, A]

// Kleisli lets us sequence monadic transforms,
// A => F[B] `flatMap` B => F[C]
type Check[A, B] = Kleisli[Result, A, B]

def check[A, B](func: A => Result[B]): Check[A, B] =
  Kleisli(func)

def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
  // Running the predicate produces a func of type:
  //   A => Either[NonEmptyList[String], A]
  //   = A => Result[A]
  //
  // We must be able to convert a Predicate to a function,
  // as Kleisli only works with functions.
  // When we convert a Predicate to a function,
  // it should have type A => Either[E, A] rather than
  // A => Validated[E, A] because Kleisli relies on the
  // wrapped function returning a monad.
  Kleisli[Result, A, A](pred.run)

def longerThan(n: Int): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must be longer than $n characters"),
    str => str.size > n
  )

val alphanumeric: Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must be all alphanumeric characters"),
    str => str.forall(_.isLetterOrDigit)
  )

def contains(char: Char): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.contains(char)
  )

def containsOnce(char: Char): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must contain the character $char only once"),
    str => str.filter(_ == char).size == 1
  )

// Kleisli[[A] =>> Either[Errors, A], String, String]
val checkUsername: Check[String, String] =
  checkPred(longerThan(3) `and` alphanumeric)

val splitEmail: Check[String, (String, String)] =
  check(_.split('@') match {
    case Array(name, domain) =>
      Right((name, domain))

    case _ =>
      Left(error("Must contain a single @ character"))
  })

val checkLeft: Check[String, String] =
  checkPred(longerThan(0))

val checkRight: Check[String, String] =
  checkPred(longerThan(3) `and` contains('.'))

val joinEmail: Check[(String, String), String] =
  check:
    case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)

val checkEmail: Check[String, String] =
  splitEmail `andThen` joinEmail
