package ch02

trait Monoid[A] extends Semigroup[A]:
  def empty: A

object Monoid:
  def apply[A](using monoid: Monoid[A]): Monoid[A] =
    monoid

object MonoidInstances:
  /*
  2.3 Exercise: The Truth About Monoids
  Consider Boolean. How many monoids can you define for this type?
   */
  given booleanAndMonoid: Monoid[Boolean] with
    def combine(a: Boolean, b: Boolean) = a && b
    def empty                           = true

  given booleanOrMonoid: Monoid[Boolean] with
    def combine(a: Boolean, b: Boolean) = a || b
    def empty                           = false

  given booleanXorMonoid: Monoid[Boolean] with
    def combine(a: Boolean, b: Boolean) =
      a != b
    def empty = false

  // The negation of XOR
  given booleanXnorMonoid: Monoid[Boolean] with
    def combine(a: Boolean, b: Boolean) =
      (!a || b) && (a || !b)

    def empty = true

  /*
  2.4 Exercise: All Set for Monoids
  What monoids and semigroups are there for sets?
   */
  given setUnionMonoid[A]: Monoid[Set[A]] with
    def combine(a: Set[A], b: Set[A]) = a union b
    def empty                         = Set.empty[A]

  given symDiffMonoid[A]: Monoid[Set[A]] with
    def combine(a: Set[A], b: Set[A]): Set[A] =
      (a diff b) union (b diff a)
    def empty: Set[A] = Set.empty

  // given intAdditionMonoid: Monoid[Int] with
  //   def combine(x: Int, y: Int): Int = x + y
  //   def empty: Int = 0
