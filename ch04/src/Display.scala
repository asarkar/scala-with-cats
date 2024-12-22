package ch04

/*
4.5 Exercise: Display Library
*/
trait Display[A]:
  def display(value: A): String


object Display:
  given stringDisplay: Display[String]:
    def display(input: String): String = input

  given intDisplay: Display[Int]:
    def display(input: Int): String = input.toString

  def display[A](input: A)(using p: Display[A]): String =
    p.display(input)

  def print[A](input: A)(using Display[A]): Unit =
    println(display(input))

object DisplaySyntax:
  extension [A](value: A)(using p: Display[A])
    def display: String = p.display(value)
    def print: Unit = Display.print(value)
