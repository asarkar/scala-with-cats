package ch03
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import cats.syntax.functor.toFunctorOps

class TreeSpec extends AnyFunSpec:
  describe("Tree"):
    it("should be able to map on leaf"):
      val actual = Tree.leaf(100).map(_ * 2)
      actual `shouldBe` Leaf(200)

    it("should be able to map on branch"):
      val actual = Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)
      actual `shouldBe` Branch(Leaf(20), Leaf(40))
