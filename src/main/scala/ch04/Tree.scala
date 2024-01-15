package ch04

import cats.Monad as CatsMonad

/*
4.10.1 Exercise: Branching out Further with Monads

Let's write a Monad for the Tree data type given below.

Verify that the code works on instances of Branch and Leaf,
and that the Monad provides Functor-like behaviour for free.

Also verify that having a Monad in scope allows us to use for comprehensions,
despite the fact that we haven’t directly implemented flatMap or map on Tree.

Don't feel you have to make tailRecM tail-recursive. Doing so is quite difficult.
 */
sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)

given CatsMonad[Tree] with
  override def pure[A](x: A): Tree[A] =
    Leaf(x)

  override def flatMap[A, B](t: Tree[A])(f: A => Tree[B]): Tree[B] =
    t match
      case Leaf(x)      => f(x)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))

  // Not stack-safe!
  override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
    flatMap(f(a)):
      case Left(value)  => tailRecM(value)(f)
      case Right(value) => Leaf(value)
