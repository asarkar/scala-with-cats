package ch08

import cats.Functor

enum Tree[A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:
  import Tree.*

  /* 8.5.4 Exercise: Branching out with Functors
  Write a Functor for the following binary tree data type. 
  Verify that the code works as expected on instances of Branch and Leaf.
 */
  given Functor[Tree]:
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(value)  => Leaf(f(value))

  // The compiler can find a Functor instance for Tree but not for Branch or Leaf 
  // (Functor is invariant in F). Let's add some smart constructors to compensate.
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)