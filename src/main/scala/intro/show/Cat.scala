package intro.show

import cats.Show
import cats.instances.int.catsStdShowForInt
import cats.instances.string.catsStdShowForString
import cats.syntax.show.toShow

/*
1.4.6 Exercise: Cat Show
Re-implement the Cat application from the previous section using Show instead of Printable.
 */
final case class Cat(name: String, age: Int, color: String)

object Cat:
  given Show[Cat] with
    override def show(cat: Cat): String =
      val name  = cat.name.show
      val age   = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
