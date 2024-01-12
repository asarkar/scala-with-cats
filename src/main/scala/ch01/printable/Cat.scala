package ch01.printable

// This is a standard use of the type class pattern.
// First we define a set of custom data types for our application.
final case class Cat(name: String, age: Int, color: String)

// Then we define type class instances for the types we care about.
// These either go into the companion object of Cat or a separate
// object to act as a namespace.
object Cat:
  import PrintableInstances.given
  import PrintableSyntax.format

  given Printable[Cat] with
    override def format(cat: Cat): String =
      val name  = cat.name.format
      val age   = cat.age.format
      val color = cat.color.format
      s"$name is a $age year-old $color cat."

// Finally, we use the type class by bringing the relevant instances
// into scope and using interface object/syntax.
// If we defined the instances in companion objects Scala brings them
// into scope for us automatically. Otherwise we use an import to access them.
object CatPrintable extends App:
  import PrintableSyntax.print

  Cat("Garfield", 41, "ginger and black").print()
