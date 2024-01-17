package ch11

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.Ignore

@Ignore
class GCounterSpec extends AnyFunSpec:
  it("should reconcile counters"):
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    val counter = summon[GCounter[Map, String, Int]]

    val merged = counter.merge(g1, g2)
    merged `shouldBe` Map("a" -> 7, "b" -> 5)

    val total = counter.total(merged)
    total `shouldBe` 12
