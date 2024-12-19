package ch03

trait Set[A]:

  def contains(elt: A): Boolean

  def insert(elt: A): Set[A] =
    InsertOneSet(elt, this)

  def union(that: Set[A]): Set[A] =
    UnionSet(this, that)

final class ListSet[A](elements: scala.collection.immutable.List[A]) extends Set[A]:

  def contains(elt: A): Boolean =
    elements.contains(elt)

  override def insert(elt: A): Set[A] =
    ListSet(elt :: elements)

  override def union(that: Set[A]): Set[A] =
    elements.foldLeft(that) { (set, elt) => set.insert(elt) }

object ListSet:
  def empty[A]: Set[A] = ListSet(List.empty)

final class InsertOneSet[A](element: A, source: Set[A]) extends Set[A]:

  def contains(elt: A): Boolean =
    elt == element || source.contains(elt)

final class UnionSet[A](first: Set[A], second: Set[A]) extends Set[A]:

  def contains(elt: A): Boolean =
    first.contains(elt) || second.contains(elt)

object Evens extends Set[Int]:

  def contains(elt: Int): Boolean =
    (elt % 2 == 0)
