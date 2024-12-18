package ch02

/*
Structural recursion works by considering all the possible inputs (which we usually represent as patterns),
and then working out what we do with each input case.

We could abstract structural recursion as an fold.
 */
enum Tree[A]:
  case Leaf(value: A)
  case Node(left: Tree[A], right: Tree[A])

  /** @return
    *   the number of values (Leafs) stored in the Tree
    */
  def size: Int = this.fold(_ => 1, _ + _)
//    this match
//      case Tree.Leaf(value) => 1
//      case Tree.Node(left, right) => left.size + right.size

  /** @param elem
    *   element to look for
    * @return
    *   true if the Tree contains a given element of type A, and false otherwise
    */
  def contains(elem: A): Boolean = this.fold(_ == elem, _ || _)
//    this match
//      case Tree.Leaf(value) => value == elem
//      case Tree.Node(left, right) => left.contains(elem) || right.contains(elem)

  /** @param f
    *   value transformation function
    * @return
    *   a Tree[B] given a function A => B
    */
  def map[B](f: A => B): Tree[B] = this.fold(v => Tree.Leaf(f(v)), (l, r) => Tree.Node(l, r))
//    this match
//      case Tree.Leaf(value) => Tree.Leaf(f(value))
//      case Tree.Node(left, right) => Tree.Node(left.map(f), right.map(f))

  def fold[B](f: (A => B), g: (B, B) => B): B =
    this match
      case Tree.Node(left, right) => g(left.fold(f, g), right.fold(f, g))
      case Tree.Leaf(value)       => f(value)
