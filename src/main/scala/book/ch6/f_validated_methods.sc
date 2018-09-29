package book.ch6

object f_validated_methods {

  /*
		6.4.3 Methods of Validated
		Validated comes with a suite of methods that closely resemble those available
		for Either, including the methods from cats.syntax.either. We can
		use map, leftMap, and bimap to transform the values inside the valid and
		invalid sides:
	*/
  import cats.data.Validated
	import cats.syntax.validated._

  type AllErrorsOr[A] = Validated[List[String], A]

  123.valid.map(_ * 100)                          //> res0: cats.data.Validated[Nothing,Int] = Valid(12300)
  "?".invalid.leftMap(_.toString)                 //> res1: cats.data.Validated[String,Nothing] = Invalid(?)
  123.valid[String].bimap(_ + "!", _ * 100)       //> res2: cats.data.Validated[String,Int] = Valid(12300)
  "?".invalid[Int].bimap(_ + "!", _ * 100)        //> res3: cats.data.Validated[String,Int] = Invalid(?!)

  /*
		We can’t flatMap because Validated isn’t a monad. However, we can
		convert back and forth between Validated and Either using the toEither
		and toValidated methods. Note that toValidated comes from
		[cats.syntax.either]:
	*/

  import cats.syntax.either._ // for toValidated
  // import cats.syntax.either._
  "Badness".invalid[Int]                          //> res4: cats.data.Validated[String,Int] = Invalid(Badness)
  "Badness".invalid[Int].toEither                 //> res5: Either[String,Int] = Left(Badness)
  "Badness".invalid[Int].toEither.toValidated     //> res6: cats.data.Validated[String,Int] = Invalid(Badness)
  /*
		We can even use the withEither method to temporarily convert to an Either
		and convert back again immediately:
	*/

  41.valid[String].withEither(_.flatMap(n => Right(n + 1)))
                                                  //> res7: cats.data.Validated[String,Int] = Valid(42)
  /*
		There is also a withValidated method in cats.syntax.either.
		As with Either, we can use the ensure method to fail with a specified error
		if a predicate does not hold:
	*/

  // 123.valid[String].ensure("Negative!")(_ > 0)
  /*
		Finally, we can call getOrElse or fold to extract values from the Valid and
		Invalid cases:
	*/

  "fail".invalid[Int].getOrElse(0)                //> res8: Int = 0
  // res26: Int = 0
  "fail".invalid[Int].fold(_ + "!!!", _.toString) //> res9: String = fail!!!
  
}