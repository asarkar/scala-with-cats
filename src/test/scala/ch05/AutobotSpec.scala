package ch05

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class AutobotSpec extends AnyFunSpec:
  it("should generate a tactitcal report"):
    val report1 = Autobot.tacticalReport("Jazz", "Bumblebee")
    report1 `shouldBe` "Jazz and Bumblebee need a recharge."

    val report2 = Autobot.tacticalReport("Bumblebee", "Hot Rod")
    report2 `shouldBe` "Bumblebee and Hot Rod are ready to roll out!"

    val report3 = Autobot.tacticalReport("Jazz", "Ironhide")
    report3 `shouldBe` "Comms error: Ironhide unreachable"
