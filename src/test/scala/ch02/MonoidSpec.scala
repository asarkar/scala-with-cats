package ch02
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import ch02.Lib.add

class MonoidSpec extends AnyFunSpec:
  it("should add integers"):
    val ints = List(1, 2, 3)
    add(ints) `shouldBe` 6

  it("should add strings"):
    val strings = List("Hi ", "there")
    add(strings) `shouldBe` "Hi there"

  it("should add sets"):
    val sets = List(Set("A", "B"), Set("B", "C"))
    add(sets) `shouldBe` Set("A", "B", "C")

  it("should add options"):
    val opts = List(Option(22), Option(20))
    add(opts) `shouldBe` Option(42)
