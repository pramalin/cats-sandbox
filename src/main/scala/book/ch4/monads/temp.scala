package book.ch4.monads
		import cats.syntax.either._
		
object temp extends App {
		val either1: Either[String, Int] = Right(10)
                                                  //> either1  : Either[String,Int] = Right(10)
		val either2: Either[String, Int] = Right(32)
     
  println("Welcome to the Scala worksheet")
	val x =	for {
			a1 <- either1
			b1 <- either2
		} yield a1 + b1
		
		println(s"x: $x")

		val z = -1.asRight[String].ensure("Must be non-negative!")(_ > 0)
		
		println(z)


	/*
		----------------------- solution ------------------------------
	*/
	
	/*
	  ---------------------------------------------------------------
	*/

}