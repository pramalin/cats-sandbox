package book.ch8

object b_abstracting_type_ctor {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  import scala.concurrent.Future

  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse
  import cats.syntax.traverse._ // for traverse
  import scala.concurrent.ExecutionContext.Implicits.global
  
  trait RealUptimeClient extends UptimeClient {
    def getUptime(hostname: String): Future[Int]
  }
  
  trait TestUptimeClient extends UptimeClient {
    def getUptime(hostname: String): Int
  }


	/*
		The question is: what result type should we give to the abstract method in
		UptimeClient? We need to abstract over Future[Int] and Int:
	*/
		trait UptimeClient {
			// def getUptime(hostname: String):  ???
		}
	
}