package ch06

import cats.{Eq, Show}
import cats.syntax.show.toShow     // A.show if Show[A] exists
import cats.syntax.eq.catsSyntaxEq // ===

final case class Cat(name: String, age: Int, color: String)

object Cat:
  /*
  6.2.1.1 Exercise: Cat Show
  Re-implement the Cat application from Section 4.5.1 using Show instead of Display.
   */
  given Show[Cat]:
    override def show(cat: Cat): String =
      val name  = cat.name.show
      val age   = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."

  /*
  6.3.4.1 Exercise: Equality, Liberty, and Felinity
  Implement an instance of Eq for our running Cat example.
   */
  given Eq[Cat]:
    override def eqv(x: Cat, y: Cat): Boolean =
      (x.name === y.name) &&
        (x.age === y.age) &&
        (x.color === y.color)
