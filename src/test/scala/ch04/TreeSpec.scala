package ch04

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps

class TreeSpec extends AnyFunSpec:
  it("Tree monad should support flatMap, map, and for-comprehension"):
    val actual = branch(leaf(100), leaf(200))
      .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
    val expected = branch(
      branch(leaf(99), leaf(101)),
      branch(leaf(199), leaf(201))
    )
    actual `shouldBe` expected

    val actual2 = for
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    yield c
    val expected2 = branch(
      branch(
        branch(leaf(89), leaf(91)),
        branch(leaf(109), leaf(111))
      ),
      branch(
        branch(leaf(189), leaf(191)),
        branch(leaf(209), leaf(211))
      )
    )
    actual2 `shouldBe` expected2
