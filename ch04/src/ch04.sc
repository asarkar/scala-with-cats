import ch04.DisplaySyntax.*
import ch04.Display

/*
components that make up a type class:

- A trait, which is the type class
- Type class instances, which are given instances.
- Type class usage, which uses using clauses.

Type classes can be composed from components using type class composition.
We can view type classes as marrying codata with tools to select and compose implementations based on type.
We can also view type classes as shifting implementation from the definition site to the call site.
Finally, can see type classes as a mechanism for ad-hoc polymorphism,
allowing us to define common functionality for otherwise unrelated types.
 */
given Display[ch04.Cat] with
  def display(cat: ch04.Cat): String =
    val name  = cat.name.display
    val age   = cat.age.display
    val color = cat.color.display
    s"$name is a $age year-old $color cat."

ch04.Cat("Garfield", 41, "ginger and black").display

trait Animal
trait Cat               extends Animal
trait DomesticShorthair extends Cat

trait Inv[A]:
  def result: String

object Inv:
  given Inv[Cat] with
    def result = "Invariant"

  def apply[A](using instance: Inv[A]): String =
    instance.result

trait Co[+A]:
  def result: String

object Co:
  given Co[Cat] with
    def result = "Covariant"

  def apply[A](using instance: Co[A]): String =
    instance.result

trait Contra[-A]:
  def result: String

object Contra:
  given Contra[Cat] with
    def result = "Contravariant"

  def apply[A](using instance: Contra[A]): String =
    instance.result

// Works!
Inv[Cat]
// res1: String = "Invariant"
Co[Animal]
// res2: String = "Covariant"
Co[Cat]
// res3: String = "Covariant"
Contra[DomesticShorthair]
// res4: String = "Contravariant"
Contra[Cat]
// res5: String = "Contravariant"

// Don't work!

// With invariance any type that is not Cat will fail.
// Inv[Animal]
// Inv[DomesticShorthair]

// Covariance fails for any subtype of the type for which the instance is declared.
// Co[DomesticShorthair]

// Contravariance fails for any supertype of the type for which the instance is declared.
// Contra[Animal]
