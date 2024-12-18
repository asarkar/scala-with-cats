package ch02

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class MyListSpec extends AnyFunSpec:
  describe("fill"):
    it("stateless"):
      val actual = MyList.fill(5)(1)
      actual.toSeq shouldBe Seq.fill(5)(1)

    it("stateful"):
      var counter = 0

      def getAndInc(): Int =
        val temp = counter
        counter = counter + 1
        temp

      val actual = MyList.fill(5)(getAndInc())
      actual.toSeq shouldBe (0 to 4)
      counter shouldBe 5

  describe("iterate"):
    it("decrement"):
      val actual = MyList.iterate(0, 5)(_ - 1)
      actual.toSeq shouldBe (0 to -4 by -1)

  describe("map"):
    it("evens"):
      val actual = MyList.iterate(0, 5)(_ + 1).map(_ * 2)
      actual.toSeq shouldBe (0 to 8 by 2)







