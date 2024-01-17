package ch08

import cats.Monad
import scala.concurrent.Future
import cats.Id

// 8 Case Study: Testing Asynchronous Code
trait UptimeClient[F[_]: Monad]:
  def getUptime(hostname: String): F[Int]

trait RealUptimeClient extends UptimeClient[Future]:
  def getUptime(hostname: String): Future[Int]

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id]:
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
