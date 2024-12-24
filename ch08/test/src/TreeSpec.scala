package ch08
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import cats.syntax.functor.toFunctorOps  // map

class TreeSpec extends AnyFunSpec:
  describe("Tree Functor"):
    it("should map on leaf"):
      val actual = Tree.leaf(100).map(_ * 2)
      actual shouldBe Tree.leaf(200)

    it("should map on branch"):
      val actual = Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)
      actual shouldBe Tree.branch(Tree.leaf(20), Tree.leaf(40))