package book.ch8

object a_uptime {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
 
 // Case Study: Teating Asynchronous Code
 
  import scala.concurrent.Future
  trait UptimeClient {
    def getUptime(hostname: String): Future[Int]
  }

  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse
  import cats.syntax.traverse._ // for traverse
  import scala.concurrent.ExecutionContext.Implicits.global

  class UptimeService(client: UptimeClient) {
    def getTotalUptime(hostnames: List[String]): Future[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
    def getUptime(hostname: String): Future[Int] =
      Future.successful(hosts.getOrElse(hostname, 0))
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }                                               //> testTotalUptime: ()Unit



	val result = testTotalUptime()            //> java.lang.AssertionError: assertion failed
                                                  //| 	at scala.Predef$.assert(Predef.scala:204)
                                                  //| 	at book.ch8.a_uptime$.testTotalUptime$1(book.ch8.a_uptime.scala:34)
                                                  //| 	at book.ch8.a_uptime$.$anonfun$main$1(book.ch8.a_uptime.scala:39)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$anonfun$$ex
                                                  //| ecute$1(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:76)
                                                  //| 	at book.ch8.a_uptime$.main(book.ch8.a_uptime.scala:3)
                                                  //| 	at book.ch8.a_uptime.main(book.ch8.a_uptime.scala)
	
	// the book says, we would get compilation error
	// <console>:31: warning: scala.concurrent.Future[Int] and Int are
  // unrelated: they will most likely never compare equal
  // assert(actual == expected)
  //               ^

	// but we get runtime error only.
}