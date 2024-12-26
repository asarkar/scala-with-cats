package ch10

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class LibSpec extends AnyFunSpec:
  describe("Autobots"):
    it("should generate a tactitcal report"):
      val report1 = Lib.tacticalReport("Jazz", "Bumblebee")
      report1 shouldBe "Jazz and Bumblebee need a recharge."

      val report2 = Lib.tacticalReport("Bumblebee", "Hot Rod")
      report2 shouldBe "Bumblebee and Hot Rod are ready to roll out!"

      val report3 = Lib.tacticalReport("Jazz", "Ironhide")
      report3 shouldBe "Comms error: Ironhide unreachable"
