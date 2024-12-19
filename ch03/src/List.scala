package ch03

package data:
  enum List[A]:
    case Pair(head: A, tail: List[A])
    case Empty()

    def foldRight[B](empty: B)(f: (A, B) => B): B =
      this match
        case Pair(head, tail) => f(head, tail.foldRight(empty)(f))
        case Empty()          => empty

package codata:
  trait List[A]:
    def foldRight[B](empty: B)(f: (A, B) => B): B

    final class Pair(head: A, tail: List[A]) extends List[A]:
      def foldRight[B](empty: B)(f: (A, B) => B): B =
        f(head, tail.foldRight(empty)(f))

    final class Empty extends List[A]:
      def foldRight[B](empty: B)(f: (A, B) => B): B =
        empty
