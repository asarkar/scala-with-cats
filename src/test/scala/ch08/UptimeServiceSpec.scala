package ch08

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class UptimeServiceSpec extends AnyFunSpec:
  it("should abstract over the monad"):
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClient(hosts)
    val service  = new UptimeService(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum

    actual `shouldBe` expected
