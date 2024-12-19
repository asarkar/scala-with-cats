package ch03

/*
codata is a product of destructors, where destructors are functions from
the codata type (and, optionally, some other inputs) to some type.

In Scala we define codata as a trait, and implement it as a final class,
anonymous subclass, or an object.

We have two strategies for implementing methods using codata: structural corecursion, which we can
use when the result is codata, and structural recursion, which we can use when an input is codata.

Data is connected to codata via fold: any data can instead be implemented as codata with
a single destructor that is the fold for that data. This is called Church encoding.
The reverse is also: we can enumerate all potential pairs of inputs and outputs of
destructors to represent codata as data.

Data and codata offer different forms of extensibility: data makes it easy to add
new functions, but adding new elements requires changing existing code, while it is
easy to add new elements to codata but we change existing code if we add new functions.
 */
// Polymorphic function type, new in Scala 3.
// https://www.youtube.com/watch?v=sauaDZ-1-zM
type List[A, B] = (B, (A, B) => B) => B

// Polymorphic function of two types, A and B, that takes no arg
// and returns the type defined above.
val Empty: [A, B] => () => List[A, B] =
  // empty: B
  [A, B] => () => (empty, _) => empty

val Pair: [A, B] => (A, List[A, B]) => List[A, B] =
  // empty: B, f: (A, B) => B
  [A, B] => (head: A, tail: List[A, B]) => (empty, f) => f(head, tail(empty, f))

val list: [B] => () => List[Int, B] =
  [B] => () => Pair(1, Pair(2, Pair(3, Empty())))
