package ch03

import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree:
  given Functor[Tree] with
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(value)  => Leaf(f(value))

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
