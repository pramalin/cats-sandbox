package book.chp10

object test_validation {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  import sandbox.usecases.Validation1._
  import cats.syntax.either._ // for asLeft and asRight

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }                                             //> a  : sandbox.usecases.Validation1.CheckF[List[String],Int] = CheckF(book.chp
                                                  //| 10.test_validation$$$Lambda$8/2088051243@57829d67)

	/*
		What happens if we try to create checks that fail with a type that we can’t
		accumulate? For example, there is no Semigroup instance for Nothing. What
		happens if we create instances of CheckF[Nothing, A]?
	*/
	
  val a1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)                        //> a1  : sandbox.usecases.Validation1.CheckF[Nothing,Int] = CheckF(book.chp10.t
                                                  //| est_validation$$$Lambda$10/2114889273@3d24753a)
  val b1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)                        //> b1  : sandbox.usecases.Validation1.CheckF[Nothing,Int] = CheckF(book.chp10.t
                                                  //| est_validation$$$Lambda$11/1504109395@7a0ac6e3)

	// val check = a1 and b1
  /*
		We can create checks just fine but when we come to combine them we get an
		error we we might expect:
		
		Multiple markers at this line
		- could not find implicit value for parameter s: cats.Semigroup[Nothing]
		- not enough arguments for method and: (implicit s: cats.Semigroup[Nothing])sandbox.usecases.Validation.CheckF[Nothing,Int].  Unspecified value parameter s.
	*/


  /*
		Now let’s see another implementation strategy. In this approach we model
		checks as an algebraic data type, with an explicit data type for each combinator.
		We’ll call this implementation Check:
	*/

  import cats.instances.list._ // for Semigroup
  // Let’s see an example:
  val a2: Check[List[String], Int] =
    Pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }                                             //> a2  : sandbox.usecases.Validation1.Check[List[String],Int] = Pure(book.chp1
                                                  //| 0.test_validation$$$Lambda$12/1908316405@6fadae5d)
    
  val b2: Check[List[String], Int] =
    Pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }                                             //> b2  : sandbox.usecases.Validation1.Check[List[String],Int] = Pure(book.chp1
                                                  //| 0.test_validation$$$Lambda$13/25126016@2d6e8792)
    
  val check: Check[List[String], Int] =
    a2 and b2                                     //> check  : sandbox.usecases.Validation1.Check[List[String],Int] = And(Pure(bo
                                                  //| ok.chp10.test_validation$$$Lambda$12/1908316405@6fadae5d),Pure(book.chp10.t
                                                  //| est_validation$$$Lambda$13/25126016@2d6e8792))

	check(9)                                  //> res0: Either[List[String],Int] = Left(List(Must be < -2))
	check(0)                                  //> res1: Either[List[String],Int] = Left(List(Must be > 2, Must be < -2))
}