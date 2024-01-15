package ch04

import cats.Eval as CatsEval

/*
4.6.5 Exercise: Safer Folding using Eval
The naive implementation of foldRight below is not stack safe. Make it so using Eval.
 */
object Eval:
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    def foldR(xs: List[A]): CatsEval[B] =
      xs match
        case head :: tail => CatsEval.defer(foldR(tail).map(fn(head, _)))
        case Nil          => CatsEval.now(acc)

    foldR(as).value
