package ch04

final case class Cat(name: String, age: Int, color: String)

object Cat:
  given catDisplay: Display[Cat]:
    def display(cat: Cat): String =
      val name = Display.display(cat.name)
      val age = Display.display(cat.age)
      val color = Display.display(cat.color)
      s"$name is a $age year-old $color cat."
