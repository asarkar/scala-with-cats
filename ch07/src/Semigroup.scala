package ch07

trait Semigroup[A]:
  def combine(x: A, y: A): A

object Semigroup:
  // No identity element, can't form a Monoid.
  given setIntersectionSemigroup: [A] => Semigroup[Set[A]]:
    def combine(a: Set[A], b: Set[A]) =
      a.intersect(b)
