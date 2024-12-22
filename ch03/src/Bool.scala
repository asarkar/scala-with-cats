package ch03

trait Bool:
  def `if`[A](t: A)(f: A): A

val True = new Bool:
  def `if`[A](t: A)(f: A): A = t

val False = new Bool:
  def `if`[A](t: A)(f: A): A = f

def and(l: Bool, r: Bool): Bool =
  new Bool:
    def `if`[A](t: A)(f: A): A =
      l.`if`(r)(False).`if`(t)(f)

/*
Exercise: Or and Not
Test your understanding of Bool by implementing or and not.
 */
def or(l: Bool, r: Bool): Bool =
  new Bool:
    def `if`[A](t: A)(f: A): A =
      l.`if`(True)(r).`if`(t)(f)

def not(b: Bool): Bool =
  new Bool:
    def `if`[A](t: A)(f: A): A =
      b.`if`(True)(False).`if`(f)(t)
