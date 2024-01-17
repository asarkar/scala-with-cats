package ch11

import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup.catsSyntaxSemigroup
import cats.syntax.foldable.toFoldableOps

// 11 Case Study: CRDTs
trait GCounter[F[_, _], K, V]:
  def increment(f: F[K, V])(k: K, v: V)(using CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(using BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(using CommutativeMonoid[V]): V

object GCounter:
  import KeyValueStoreSyntax.*

  given [F[_, _], K, V](using KeyValueStore[F], CommutativeMonoid[F[K, V]]): GCounter[F, K, V] with
    def increment(f: F[K, V])(key: K, value: V)(using m: CommutativeMonoid[V]): F[K, V] =
      val total = f.getOrElse(key, m.empty) |+| value
      f.put(key, total)

    def merge(f1: F[K, V], f2: F[K, V])(using BoundedSemiLattice[V]): F[K, V] =
      f1 |+| f2

    def total(f: F[K, V])(using CommutativeMonoid[V]): V =
      f.values.combineAll
