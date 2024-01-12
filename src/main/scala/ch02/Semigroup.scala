package ch02

trait Semigroup[A]:
  def combine(x: A, y: A): A

object SemigroupInstances:
  // No identity element, can't form a Monoid.
  given setIntersectionSemigroup[A]: Semigroup[Set[A]] with
    def combine(a: Set[A], b: Set[A]) =
      a intersect b
