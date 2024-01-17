package ch04
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class ReaderSpec extends AnyFunSpec:
  it("DbReader should check password"):
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )

    val db = Reader.Db(users, passwords)

    Reader.checkLogin(1, "zerocool").run(db) `shouldBe` true
    Reader.checkLogin(4, "davinci").run(db) `shouldBe` false
