package ch03

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class SetSpec extends AnyFunSpec:
  describe("Set"):
    it("evens"):
      val evensAndOne = Evens.insert(1)
      val evensAndOthers =
        Evens.union(ListSet.empty.insert(1).insert(3))

      evensAndOne.contains(1) shouldBe true
      evensAndOthers.contains(1) shouldBe true
      evensAndOne.contains(2) shouldBe true
      evensAndOthers.contains(2) shouldBe true
      evensAndOne.contains(3) shouldBe false
      evensAndOthers.contains(3) shouldBe true
