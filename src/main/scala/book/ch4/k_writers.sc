package book.ch4

object k_writers {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  // 4.7.1 Creating and Unpacking Writers
  // A Writer[W, A] carries two values: a log of type W and a result of type A. We
  // can create a Writer from values of each type as follows:

  import cats.data.Writer
  import cats.instances.vector._ // for Monoid
  Writer(Vector(
    "It was the best of times",
    "it was the worst of times"), 1859)           //> res0: cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],In
                                                  //| t] = WriterT((Vector(It was the best of times, it was the worst of times),18
                                                  //| 59))
  /*
	For convenience, Cats provides a way of crea􀦞ng Writers specifying only the
	log or the result. If we only have a result we can use the standard pure syntax.
	To do this we must have a Monoid[W] in scope so Cats knows how to produce
	an empty log:
	*/
  import cats.instances.vector._ // for Monoid
  import cats.syntax.applicative._ // for pure
  type Logged[A] = Writer[Vector[String], A]
  123.pure[Logged]                                //> res1: book.ch4.k_writers.Logged[Int] = WriterT((Vector(),123))

  // If we have a log and no result we can create a Writer[Unit] using the tell
  // syntax from cats.syntax.writer:
  import cats.syntax.writer._ // for tell
  Vector("msg1", "msg2", "msg3").tell             //> res2: cats.data.Writer[scala.collection.immutable.Vector[String],Unit] = Wr
                                                  //| iterT((Vector(msg1, msg2, msg3),()))
  /*
 	If we have both a result and a log, we can either use Writer.apply or we can
	use the writer syntax from cats.syntax.writer:
	*/
  import cats.syntax.writer._ // for writer
  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
                                                  //> a  : cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],In
                                                  //| t] = WriterT((Vector(msg1, msg2, msg3),123))
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))
                                                  //> b  : cats.data.Writer[scala.collection.immutable.Vector[String],Int] = Writ
                                                  //| erT((Vector(msg1, msg2, msg3),123))

  /*
	We can extract the result and log from a Writer using the value and written
	methods respectively:
	*/
  val aResult: Int = a.value                      //> aResult  : Int = 123
  val aLog: Vector[String] = a.written            //> aLog  : Vector[String] = Vector(msg1, msg2, msg3)

  // We can extract both values at the same time using the run method:
  val (log, result) = b.run                       //> log  : scala.collection.immutable.Vector[String] = Vector(msg1, msg2, msg3)
                                                  //| 
                                                  //| result  : Int = 123

  /* 4.7.2 Composing and Transforming Writers
		The log in a Writer is preserved when we map or flatMap over it. flatMap
		appends the logs from the source Writer and the result of the user’s sequencing
		function. For this reason it’s good practice to use a log type that has an
		efficient append and concatenate operations, such as a Vector:
		*/

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b                                   //> writer1  : cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((Vector(
                                                  //| a, b, c, x, y, z),42))

  writer1.run                                     //> res3: cats.Id[(Vector[String], Int)] = (Vector(a, b, c, x, y, z),42)

  /*
		In addition to transforming the result with map and flatMap, we can transform
		the log in a Writer with the mapWritten method:
	*/
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
                                                  //> writer2  : cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[Stri
                                                  //| ng],Int] = WriterT((Vector(A, B, C, X, Y, Z),42))
  writer2.run                                     //> res4: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (Vector(A
                                                  //| , B, C, X, Y, Z),42)

  /*
		We can transform both log and result simultaneously using bimap or mapBoth.
		bimap takes two function parameters, one for the log and one for the result.
		mapBoth takes a single func􀦞on that accepts two parameters:
	*/
  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100)                             //> writer3  : cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[Stri
                                                  //| ng],Int] = WriterT((Vector(A, B, C, X, Y, Z),4200))

  writer3.run                                     //> res5: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (Vector(A
                                                  //| , B, C, X, Y, Z),4200)

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }                                               //> writer4  : cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[Stri
                                                  //| ng],Int] = WriterT((Vector(a!, b!, c!, x!, y!, z!),42000))

  writer4.run                                     //> res6: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (Vector(a
                                                  //| !, b!, c!, x!, y!, z!),42000)

  /* Finally, we can clear the log with the reset method and swap log and result
		with the swap method:
	*/
  val writer5 = writer1.reset                     //> writer5  : cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((Vector(
                                                  //| ),42))

  writer5.run                                     //> res7: cats.Id[(Vector[String], Int)] = (Vector(),42)
  val writer6 = writer1.swap                      //> writer6  : cats.data.WriterT[cats.Id,Int,Vector[String]] = WriterT((42,Vect
                                                  //| or(a, b, c, x, y, z)))

  writer6.run                                     //> res8: cats.Id[(Int, Vector[String])] = (42,Vector(a, b, c, x, y, z))

  /* 4.7.3 Exercise: Show Your Working
		Writers are useful for logging operations in multi-threaded environments.
		Let’s confirm this by computing (and logging) some factorials.
		The factorial function below computes a factorial and prints out the intermediate
		steps as it runs. The slowly helper function ensures this takes a while
		to run, even on the very small examples below:
	*/

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)            //> slowly: [A](body: => A)A
  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }                                               //> factorial: (n: Int)Int
  // Here’s the output—a sequence of monotonically increasing values:
  factorial(5)                                    //> fact 0 1
                                                  //| fact 1 1
                                                  //| fact 2 2
                                                  //| fact 3 6
                                                  //| fact 4 24
                                                  //| fact 5 120
                                                  //| res9: Int = 120
  /*
		If we start several factorials in parallel, the log messages can become interleaved
		on standard out. This makes it difficult to see which messages come
		from which computation:
	*/

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3)))), 5.seconds)            //> fact 0 1
                                                  //| fact 0 1
                                                  //| fact 1 1
                                                  //| fact 1 1
                                                  //| fact 2 2
                                                  //| fact 2 2
                                                  //| fact 3 6
                                                  //| fact 3 6
                                                  //| res10: scala.collection.immutable.Vector[Int] = Vector(6, 6)

}