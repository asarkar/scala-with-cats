package ch04

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class StateSpec extends AnyFunSpec:
  it("evalInput should be able to evaluate a post-order expression"):
    State.evalInput("1 2 + 3 4 + *") `shouldBe` 21
