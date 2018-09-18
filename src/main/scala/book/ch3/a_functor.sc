package book.ch3

object a_functor {
	//
	// built in map()
	//
  List(1, 2, 3).map(n => n + 1)                   //> res0: List[Int] = List(2, 3, 4)
  List(1, 2, 3).
    map(n => n + 1).
    map(n => n * 2).
    map(n => n + "!")                             //> res1: List[String] = List(4!, 6!, 8!)

  import scala.concurrent.{ Future, Await }
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  val future: Future[String] =
    Future(123).
      map(n => n + 1).
      map(n => n * 2).
      map(n => n + "!")                           //> future  : scala.concurrent.Future[String] = Future(<not completed>)
  Await.result(future, 1.second)                  //> res2: String = 248!
}