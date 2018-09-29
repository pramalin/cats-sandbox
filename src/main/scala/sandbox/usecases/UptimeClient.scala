package sandbox.usecases

import cats.instances.list._
import cats.instances.future._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Id}

import scala.concurrent.{Await, Future}
import scala.language.higherKinds // for traverse

/*
 * my first cut. better implementation is com.github.beikern.usescases
 */
object UptimeClient extends App {

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  //   Exercise 8.1
  //  trait RealUptimeClient extends UptimeClient[Future] {
  //    def getUptime(hostname: String): Future[Int]
  //  }

  //  trait TestUptimeClient extends UptimeClient[Id] {
  //    def getUptime(hostname: String): Id[Int]
  //  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }

  class RealUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int] =
      Future.successful(hosts.getOrElse(hostname, 0))
  }

  class UptimeService[F[_]](client: UptimeClient[F])(implicit a: Applicative[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    println(s"Uptime test success: ${actual == expected}")

  }
  testTotalUptime()

  def test2() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val testUptimeClient = new TestUptimeClient(hosts)
    println(new UptimeService(testUptimeClient).getTotalUptime(List("host1", "host2")))

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val realUptimeClient = new RealUptimeClient(hosts)
    println(s"access Future result: ${Await.result(realUptimeClient.getUptime("host1"), 10.seconds)}")

    println("Asynch:")
    val result =  new UptimeService[Future](realUptimeClient).getTotalUptime(List("host1", "host2"))
    println(Await.result(result, 10.seconds))
  }

  test2()
}