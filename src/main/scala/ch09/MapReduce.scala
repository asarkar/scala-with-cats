package ch09

import cats.Monoid
import scala.concurrent.Future
import cats.syntax.traverse.toTraverseOps
import cats.syntax.foldable.toFoldableOps
import scala.concurrent.ExecutionContext

/*
9 Case Study: Map-Reduce

1. Start with an initial list of all the data we need to process
2. Divide the data into batches, sending one batch to each CPU
3. The CPUs run a batch-level map phase in parallel
4. The CPUs run a batch-level reduce phase in parallel,
   producing a local result for each batch
5. Reduce the results for each batch to a single final result
 */
object MapReduce:
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B)(using ExecutionContext): Future[B] =
    val numCores  = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    values
      .grouped(groupSize)
      // grouped returns an Iterator but cats
      // doesn't have a Traverse instance for Iterator,
      // so, convert to Vector.
      .toVector
      // ExecutionContext.Implicits.global. This default context allocates
      // a thread pool with one thread per CPU in our machine.
      // When we create a Future the ExecutionContext schedules it for execution.
      // If there is a free thread in the pool, the Future starts executing immediately.
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
