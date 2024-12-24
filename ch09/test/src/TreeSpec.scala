package ch09

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import cats.syntax.flatMap.toFlatMapOps  // flatMap
import cats.syntax.functor.toFunctorOps  // map
import Tree.given

class TreeSpec extends AnyFunSpec:
  describe("Tree monad"):
    it("should support flatMap, map, and for-comprehension"):
      val actual = Tree.branch(Tree.leaf(100), Tree.leaf(200))
        .flatMap(x => Tree.branch(Tree.leaf(x - 1), Tree.leaf(x + 1)))
      val expected = Tree.branch(
        Tree.branch(Tree.leaf(99), Tree.leaf(101)),
        Tree.branch(Tree.leaf(199), Tree.leaf(201))
      )
      actual shouldBe expected

      val actual2 = for
        a <- Tree.branch(Tree.leaf(100), Tree.leaf(200))  // flatMap
        b <- Tree.branch(Tree.leaf(a - 10), Tree.leaf(a + 10))  // flatMap
        c <- Tree.branch(Tree.leaf(b - 1), Tree.leaf(b + 1))  // map
      yield c
      val expected2 = Tree.branch(
        Tree.branch(
          Tree.branch(Tree.leaf(89), Tree.leaf(91)),
          Tree.branch(Tree.leaf(109), Tree.leaf(111))
        ),
        Tree.branch(
          Tree.branch(Tree.leaf(189), Tree.leaf(191)),
          Tree.branch(Tree.leaf(209), Tree.leaf(211))
        )
      )
      actual2 shouldBe expected2
