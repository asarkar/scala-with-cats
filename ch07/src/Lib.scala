package ch07

/*
We can use Semigroups and Monoids by importing two things: the type classes themselves, 
and the semigroup syntax to give us the |+| operator.
*/
import cats.syntax.semigroup.catsSyntaxSemigroup  // A |+| A if Semigroup[A] exists

object Lib:

  /*
  7.3.3.1 Exercise: Adding All The Things
  The cutting edge SuperAdder v3.5a-32 is the world's first choice for adding together numbers.
  The main function in the program has signature def add(items: List[Int]): Int.
  In a tragic accident this code is deleted! Rewrite the method and save the day!

  SuperAdder's market share continues to grow, and now there is demand for additional functionality.
  People now want to add List[Option[Int]]. Change add so this is possible. The SuperAdder code base
  is of the highest quality, so make sure there is no code duplication!
   */
  def add[A: cats.Monoid as m](items: List[A]): A =
    items.foldLeft(m.empty)(_ |+| _)

//   import cats.instances.int.catsKernelStdGroupForInt
//   add(List(1, 2, 3))

//   add(List(Some(1), None, Some(2), None, Some(3)))

// Doesn't compile: No given instance of type cats.kernel.Monoid[Some[Int]] was found.
// The inferred type of the list is List[Some[Int]], Cats Monoid is invariant, so,
// Monoid[Option[A]] is not applicable.
//   add(List(Some(1), Some(2), Some(3)))

  /*
  SuperAdder is entering the POS (point-of-sale, not the other POS) market.
  Now we want to add up Orders.

  We need to release this code really soon so we can’t make any modifications to add.
  Make it so!
   */
  case class Order(totalCost: Double, quantity: Double)

  given Monoid[Order]:
    def combine(o1: Order, o2: Order) =
      Order(
        o1.totalCost + o2.totalCost,
        o1.quantity + o2.quantity
      )

    def empty = Order(0, 0)